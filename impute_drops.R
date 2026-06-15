# impute admin data for study withdrawals
# created 3/11/26: FE
# log: hello world
# load packages -----------------------------------------------------------
library(tidyverse)
library(mice)
set.seed(1)
ID_list <- read_csv("./data/group_assignment.csv") |>
  rename(GROUP = Group)
# create list of PINs in data
ID_list <- c_long |>
  select(ID) |>
  distinct()

# read wave 1 survey, return subset of useful variables for imputation
w1 <- read_csv("./data/w1_reduced.csv") |>
  mutate(COUNTY = str_sub(County, 1, -8)) |>
  select(
    ID,
    COUNTY,
    unit,
    n_child,
    Age_c,
    Gender_s,
    Race_White,
    Race_Black,
    SexOrient_Straight,
    SF_PhysicalFunctioning,
    SF_RoleLimitationsPhys,
    Edu,
    Kessler_Score:Chaos_Score,
    AH_Total:ParentalStressTotal
  ) |>
  mutate(Gender_f = Gender_s == "Female") |>
  select(-Gender_s)

# read IDs of drops
drops <- read_csv("./data/drops.csv")

# prep df for drops -------------------------------------------------------
# read
extras <- w1 |>
  filter(ID %in% drops$ID)

# create empty DF for children of drops
extras_temp <- data.frame(
  Child = as.character(sample(1e4:1e5, sum(extras$n_child))),
  ID = rep(extras$ID, extras$n_child),
  COUNTY = rep(extras$COUNTY, extras$n_child),
  GROUP = rep(extras$unit, extras$n_child),
  TotalCPS = NA,
  Confirmed = NA,
  Prev = NA,
  FC = NA
)

# four waves
extras_df <- extras_temp |>
  mutate(Wave = 1) |>
  bind_rows(
    extras_temp |>
      mutate(Wave = 2)
  ) |>
  bind_rows(
    extras_temp |>
      mutate(Wave = 3)
  ) |>
  bind_rows(
    extras_temp |>
      mutate(Wave = 4)
  ) |>
  select(Child, GROUP, ID, COUNTY, Wave, TotalCPS:FC)

# bind
temp <- c_long |>
  mutate(Child = as.character(Child)) |>
  bind_rows(extras_df)

# attach survey, prefer admin county classification
temp <- temp |>
  left_join(
    w1 |>
      select(-COUNTY)
  ) |>
  mutate(COUNTY = factor(COUNTY), GROUP = factor(GROUP), Wave = factor(Wave))

# imputation --------------------------------------------------------------
# setup predictor matrix
predmat <- make.predictorMatrix(temp)
# turn off IDs
predmat[, "Child"] <- 0
predmat[, "ID"] <- 0
predmat[, "unit"] <- 0
# predmat[, "Edu"] <- 0
# predmat["Edu", ] <- 0
c_imp <- mice(temp, predictorMatrix = predmat, m = 5, maxit = 20)

c_long_imp <- mice::complete(c_imp, action = "long", include = T)

# diagnostics -------------------------------------------------------------
pdf("./output/mice_trace.pdf", height = 8, width = 8)
plot(c_imp, y = c("TotalCPS", "Confirmed", "Prev", "FC"), layout = c(2, 4))
dev.off()

pdf("./output/mice_density.pdf", height = 8, width = 8)
densityplot(c_imp, ~ TotalCPS + Confirmed + Prev + FC)
dev.off()

c_long_imp |>
  group_by(.imp, Wave, GROUP) |>
  summarize(
    n = sum(!(is.na(TotalCPS))),
    across(TotalCPS:FC, \(x) sum(x, na.rm = T))
  )

# compute added cases on each outcome, compute prob
observed <- temp |>
  group_by(GROUP, Wave) |>
  summarize(
    n_obs = sum(!(is.na(TotalCPS))),
    across(TotalCPS:FC, \(x) sum(x, na.rm = T)),
  ) |>
  rename(
    TotalCPS_obs = TotalCPS,
    Confirmed_obs = Confirmed,
    Prev_obs = Prev,
    FC_obs = FC
  )

imputed <- c_long_imp |>
  filter(.imp > 0) |>
  group_by(GROUP, Wave, .imp) |>
  summarize(
    n = n(),
    across(TotalCPS:FC, \(x) sum(x, na.rm = T))
  )
# compute probability within imputed, compare to observed
delta <- observed |>
  left_join(imputed) |>
  mutate(
    n_delta = n - n_obs,
    TotalCPS = TotalCPS - TotalCPS_obs,
    Confirmed = Confirmed - Confirmed_obs,
    Prev = Prev - Prev_obs,
    FC = FC - FC_obs
  ) |>
  select(GROUP, Wave, .imp, n_delta, TotalCPS:FC) |>
  pivot_longer(
    cols = TotalCPS:FC,
    names_to = "var",
    values_to = "delta"
  ) |>
  mutate(delta_rate = delta / n_delta) |>
  mutate(delta_rate = ifelse(is.nan(delta_rate), 0, delta_rate))

observed_rt <- observed |>
  pivot_longer(TotalCPS_obs:FC_obs, names_to = "var", values_to = "observed") |>
  mutate(observed_rate = observed / n_obs) |>
  mutate(var = str_sub(var, 1, -5)) |>
  select(GROUP, Wave, var, observed_rate)

delta <- delta |>
  left_join(observed_rt) |>
  filter(GROUP == "C")

# plot rate among imputed against empirical to confirm distributions are similar

p <- ggplot(delta, aes(y = delta_rate, x = Wave, group = .imp)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~var) +
  geom_point(aes(y = observed_rate), color = "red", shape = 2, alpha = 0.5) +
  labs(
    y = "Event rate",
    subtitle = "Black points are event rates only among imputed cells, red triangle is empirical mean in control group"
  )
ggsave(plot = p, filename = "./output/mice_delta_rate.pdf")

# plot full sample rates against empirical

imputed_rt <- imputed |>
  pivot_longer(TotalCPS:FC, names_to = "var", values_to = "imputed") |>
  mutate(imputed_rate = imputed / n) |>
  select(GROUP, Wave, .imp, var, imputed_rate) |>
  filter(GROUP == "C") |>
  left_join(observed_rt)

p <- ggplot(imputed_rt, aes(y = imputed_rate, x = Wave, group = .imp)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~var, scales = "free") +
  geom_point(aes(y = observed_rate), color = "red", shape = 2, alpha = 0.5) +
  labs(
    y = "Event rate",
    subtitle = "Black points are event rates for post-imputation samples (full data), red triangle is empirical mean in control group"
  )
ggsave(plot = p, filename = "./output/mice_full_rate.pdf")

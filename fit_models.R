set.seed(8625)
### run once, then comment out with #
# install.packages(c("tidyverse", "brms", "tidybayes"))
# install.packages(c("plm", "lmtest"))

# load packages
library(tidyverse)
library(tidybayes)
library(brms)
library(plm)
library(lmtest)

### settings for brms
cores <- parallel::detectCores()
model_controls <- list(
  adapt_delta = 0.999,
  stepsize = 0.01,
  max_treedepth = 15
)
iter <- 1e4

n_imps <- max(c_long_imp$.imp)
start_time <- Sys.time()

# regression models -------------------------------------------------------
# estimate focal ITT models with all cases
# sensitivity models:
# 1) complete case
# 2) with covars for ITT for potential imbalance
# 3) collapsed time
# 4) FE LPM

# set up imputed data in list with secondary collapsed treatment time
# and pdata.frames for plm
c_imp <- list()
p_data <- list()
for (i in 1:5) {
  temp <- c_long_imp |>
    filter(.imp == i) |>
    mutate(
      Wave_collapsed = case_when(
        Wave == 2 | Wave == 3 ~ "Treatment",
        Wave == 1 ~ "Pre",
        Wave == 4 ~ "Post"
      )
    ) |>
    mutate(Wave_collapsed = as.factor(Wave_collapsed))
  c_imp[[i]] <- temp

  temp <- c_long_imp |>
    filter(.imp == i)
  p_data[[i]] <- pdata.frame(temp, index = c("Child", "Wave"))
}

# subset for complete case sensitivity models
c_imp_complete_case <- c_long_imp |>
  filter(.imp == 0, !(is.na(Confirmed)))

# regression models -------------------------------------------------------
# priors ------------------------------------------------------------------
priorsL <- priorsL_FC <- c(
  set_prior("student_t(3, 0, 10)", class = "b"),
  set_prior("student_t(3, 0, 10)", class = "Intercept"),
  set_prior("student_t(3, 0, 1)", class = "sd")
)

# total reports -----------------------------------------------------------
# 0. focal model
TotalCPS_b <- brm_multiple(
  TotalCPS > 0 ~ Wave + GROUP:Wave + COUNTY + (1 | Child),
  family = bernoulli(),
  data = c_imp,
  prior = priorsL,
  iter = iter,
  cores = cores,
  control = model_controls
)
#### output
# parameter estimates and CI
sink("./output/TotalCPS_b.txt")
print(TotalCPS_b)
sink()
# posteriors for fixed effects
tidy_draws(TotalCPS_b) |>
  select(.chain:Intercept) |>
  write_csv("./output/TotalCPS_b.csv")


# sensitivity

# 1. complete case
TotalCPS_b_s1 <- brm(
  TotalCPS > 0 ~ Wave + GROUP:Wave + COUNTY + (1 | Child),
  family = bernoulli(),
  data = c_imp_complete_case,
  prior = priorsL,
  iter = iter,
  cores = cores,
  control = model_controls
)
sink("./output/TotalCPS_b_s1.txt")
print(TotalCPS_b_s1)
sink()

# 2. with controls
TotalCPS_b_s2 <- brm_multiple(
  TotalCPS > 0 ~
    Wave +
    GROUP:Wave +
    COUNTY +
    n_child +
    Gender_f +
    Race_White +
    scale(Edu) +
    SexOrient_Straight +
    scale(Kessler_Score) +
    scale(AH_Total) +
    scale(Total_FW_Score) +
    (1 | Child),
  data = c_imp,
  family = bernoulli(),
  prior = priorsL,
  iter = iter,
  cores = cores,
  control = model_controls
)

sink("./output/TotalCPS_b_s2.txt")
print(TotalCPS_b_s2)
sink()

# 3. Collapsed time
TotalCPS_b_s3 <- brm_multiple(
  TotalCPS > 0 ~ Wave + GROUP:Wave + COUNTY + (1 | Child),
  family = bernoulli(),
  data = c_imp,
  prior = priorsL,
  iter = iter,
  cores = cores,
  control = model_controls
)

sink("./output/TotalCPS_b_s_3.txt")
print(TotalCPS_b_s3)
sink()

# 4. fixed effects linear probability
m_out <- list()
for (i in 1:length(p_data)) {
  temp <- p_data[[i]]
  m_temp <- plm(
    TotalCPS > 0 ~ COUNTY + Wave + GROUP:Wave,
    data = temp,
    effect = "individual",
    model = "within"
  )

  m_out[[i]] <- coeftest(m0, function(x) {
    vcovHC(x, type = 'HC0', cluster = 'group')
  })
}

write_csv(summary(pool(m_out)), file = "./output/TotalCPS_lpm.csv")

# confirmed -----------------------------------------------------------
# 0. focal model
Confirmed_b <- brm_multiple(
  Confirmed > 0 ~ Wave + GROUP:Wave + COUNTY + (1 | Child),
  family = bernoulli(),
  data = c_imp,
  prior = priorsL,
  iter = iter,
  cores = cores,
  control = model_controls
)
#### output
# parameter estimates and CI
sink("./output/Confirmed_b.txt")
print(Confirmed_b)
sink()
# posteriors for fixed effects
tidy_draws(Confirmed_b) |>
  select(.chain:Intercept) |>
  write_csv("./output/Confirmed_b.csv")


# sensitivity

# 1. complete case
Confirmed_b_s1 <- brm(
  Confirmed > 0 ~ Wave + GROUP:Wave + COUNTY + (1 | Child),
  family = bernoulli(),
  data = c_imp_complete_case,
  prior = priorsL,
  iter = iter,
  cores = cores,
  control = model_controls
)
sink("./output/Confirmed_b_s1.txt")
print(Confirmed_b_s1)
sink()

# 2. with controls
Confirmed_b_s2 <- brm_multiple(
  Confirmed > 0 ~
    Wave +
    GROUP:Wave +
    COUNTY +
    n_child +
    Gender_f +
    Race_White +
    scale(Edu) +
    SexOrient_Straight +
    scale(Kessler_Score) +
    scale(AH_Total) +
    scale(Total_FW_Score) +
    (1 | Child),
  data = c_imp,
  family = bernoulli(),
  prior = priorsL,
  iter = iter,
  cores = cores,
  control = model_controls
)

sink("./output/Confirmed_b_s2.txt")
print(Confirmed_b_s2)
sink()

# 3. Collapsed time
Confirmed_b_s3 <- brm_multiple(
  Confirmed > 0 ~ Wave + GROUP:Wave + COUNTY + (1 | Child),
  family = bernoulli(),
  data = c_imp,
  prior = priorsL,
  iter = iter,
  cores = cores,
  control = model_controls
)

sink("./output/Confirmed_b_s3.txt")
print(Confirmed_b_s3)
sink()

# 4. fixed effects linear probability
m_out <- list()
for (i in 1:length(p_data)) {
  temp <- p_data[[i]]
  m_temp <- plm(
    Confirmed > 0 ~ COUNTY + Wave + GROUP:Wave,
    data = temp,
    effect = "individual",
    model = "within"
  )

  m_out[[i]] <- coeftest(m0, function(x) {
    vcovHC(x, type = 'HC0', cluster = 'group')
  })
}

write_csv(summary(pool(m_out)), file = "./output/Confirmed_lpm.csv")

# Prevention --------------------------------------------------------------

# 0. focal model
Prev_b <- brm_multiple(
  Prev > 0 ~ Wave + GROUP:Wave + COUNTY + (1 | Child),
  family = bernoulli(),
  data = c_imp,
  prior = priorsL,
  iter = iter,
  cores = cores,
  control = model_controls
)
#### output
# parameter estimates and CI
sink("./output/Prev_b.txt")
print(Prev_b)
sink()
# posteriors for fixed effects
tidy_draws(Prev_b) |>
  select(.chain:Intercept) |>
  write_csv("./output/Prev_b.csv")


# sensitivity

# 1. complete case
Prev_b_s_1 <- brm(
  Prev > 0 ~ Wave + GROUP:Wave + COUNTY + (1 | Child),
  family = bernoulli(),
  data = c_imp_complete_case,
  prior = priorsL,
  iter = iter,
  cores = cores,
  control = model_controls
)
sink("./output/Prev_b_s1.txt")
print(Prev_b_s_1)
sink()

# 2. with controls
Prev_b_s2 <- brm_multiple(
  Prev > 0 ~
    Wave +
    GROUP:Wave +
    COUNTY +
    n_child +
    Gender_f +
    Race_White +
    scale(Edu) +
    SexOrient_Straight +
    scale(Kessler_Score) +
    scale(AH_Total) +
    scale(Total_FW_Score) +
    (1 | Child),
  data = c_imp,
  family = bernoulli(),
  prior = priorsL,
  iter = iter,
  cores = cores,
  control = model_controls
)

sink("./output/Prev_b_s2.txt")
print(Prev_b_s2)
sink()

# 3. Collapsed time
Prev_b_s3 <- brm_multiple(
  Prev > 0 ~ Wave + GROUP:Wave + COUNTY + (1 | Child),
  family = bernoulli(),
  data = c_imp,
  prior = priorsL,
  iter = iter,
  cores = cores,
  control = model_controls
)

sink("./output/Prev_b_s3.txt")
print(Prev_b_s3)
sink()

# 4. fixed effects linear probability
m_out <- list()
for (i in 1:length(p_data)) {
  temp <- p_data[[i]]
  m_temp <- plm(
    Prev > 0 ~ COUNTY + Wave + GROUP:Wave,
    data = temp,
    effect = "individual",
    model = "within"
  )

  m_out[[i]] <- coeftest(m0, function(x) {
    vcovHC(x, type = 'HC0', cluster = 'group')
  })
}

write_csv(summary(pool(m_out)), file = "./output/Prev_lpm.csv")


# Foster care -------------------------------------------------------------

# 0. focal model
FC_b <- brm_multiple(
  FC > 0 ~ Wave + GROUP:Wave + COUNTY + (1 | Child),
  family = bernoulli(),
  data = c_imp,
  prior = priorsL,
  iter = iter,
  cores = cores,
  control = model_controls
)
#### output
# parameter estimates and CI
sink("./output/FC_b.txt")
print(FC_b)
sink()
# posteriors for fixed effects
tidy_draws(FC_b) |>
  select(.chain:Intercept) |>
  write_csv("./output/FC_b.csv")


# sensitivity

# 1. complete case
FC_b_s1 <- brm(
  FC > 0 ~ Wave + GROUP:Wave + COUNTY + (1 | Child),
  family = bernoulli(),
  data = c_imp_complete_case,
  prior = priorsL,
  iter = iter,
  cores = cores,
  control = model_controls
)
sink("./output/FC_b_s1.txt")
print(FC_b_s1)
sink()

# 2. with controls
FC_b_s2 <- brm_multiple(
  FC > 0 ~
    Wave +
    GROUP:Wave +
    COUNTY +
    n_child +
    Gender_f +
    Race_White +
    scale(Edu) +
    SexOrient_Straight +
    scale(Kessler_Score) +
    scale(AH_Total) +
    scale(Total_FW_Score) +
    (1 | Child),
  data = c_imp,
  family = bernoulli(),
  prior = priorsL,
  iter = iter,
  cores = cores,
  control = model_controls
)

sink("./output/FC_b_s2.txt")
print(FC_b_s2)
sink()

# 3. Collapsed time
FC_b_s3 <- brm_multiple(
  FC > 0 ~ Wave + GROUP:Wave + COUNTY + (1 | Child),
  family = bernoulli(),
  data = c_imp,
  prior = priorsL,
  iter = iter,
  cores = cores,
  control = model_controls
)

sink("./output/FC_b_s3.txt")
print(FC_b_s3)
sink()

# 4. fixed effects linear probability
m_out <- list()
for (i in 1:length(p_data)) {
  temp <- p_data[[i]]
  m_temp <- plm(
    FC > 0 ~ COUNTY + Wave + GROUP:Wave,
    data = temp,
    effect = "individual",
    model = "within"
  )

  m_out[[i]] <- coeftest(m0, function(x) {
    vcovHC(x, type = 'HC0', cluster = 'group')
  })
}

write_csv(summary(pool(m_out)), file = "./output/FC_lpm.csv")

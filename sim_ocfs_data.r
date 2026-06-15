library(tidyverse)
# read in empirical distributions
# scale down w1 to six months
# delete after adjusting descriptive tables
dat <- read_csv("./output1_12_26/child_table_period.csv")
dat <- read_csv("./output6_5_26/child_period_freq.csv") |>
  rename(n = n_child) |>
  filter(.imp == 0) |>
  select(-.imp)
# pull IDs
ID_list <- read_csv("./consort_data/group_assignment.csv") |>
  rename(GROUP = Group)

### set up fake data for model diagnostics
counties <- c("Monroe", "Onondaga", "Westchester")
# sim child and hh
## county error
county_error <- data.frame(
  County = counties,
  county_error = rnorm(3, 0, 2)
)
## household error
# hh_error <- ID_list |>
#   ,
#          County = sample(counties, nrow(ID_list), replace = T))

### FIX THIS SO EACH HH HAS CORRECT N OF CHILDREN
w1 <- read_csv("./data/w1_reduced.csv") |>
  mutate(COUNTY = str_sub(County, 1, -8)) |>
  select(ID, COUNTY, n_child) |>
  left_join(ID_list) |>
  filter(!(is.na(GROUP)))

# ok loop over each hh, populate child id, then randomly sample to n
child_list <- list()
child_index <- 1
for (i in unique(w1$ID)) {
  temp <- w1 |>
    filter(ID == i)
  if (temp$n_child > 0) {
    child_ids <- data.frame(
      Child = child_index:(child_index + temp$n_child - 1),
      ID = temp$ID,
      County = temp$COUNTY,
      GROUP = temp$GROUP,
      hh_error = rnorm(1, 0, 2)
    )
    child_index <- max(child_ids$Child) + 1
    child_list[[i]] <- child_ids
  }
}
child_ids <- bind_rows(child_list)

sim_dat <- child_ids |>
  mutate(child_error = rnorm(1012, 0, 2)) |>
  left_join(county_error) |>
  mutate(total_error = county_error + hh_error + child_error) |>
  expand_grid(Wave = 1:4)

# allocation ------------------------------------------------------------------
# define lambda
lambda <- dat |>
  select(GROUP, Wave, n, TotalCPS, Confirmed, Prev, FC) |>
  mutate(across(TotalCPS:FC, function(x) x / n)) |>
  rename(
    TotalCPS_l = TotalCPS,
    Confirmed_l = Confirmed,
    Prev_l = Prev,
    FC_l = FC
  ) |>
  select(Wave, GROUP, TotalCPS_l:FC_l)

# initialize df
sim_dat <- sim_dat |>
  left_join(lambda) |>
  mutate(
    TotalCPS_l = TotalCPS_l * abs(1 + total_error / 10),
    Confirmed_l = Confirmed_l * abs(1 + total_error / 10),
    Prev_l = Prev_l * abs(1 + total_error / 10),
    FC_l = FC_l * abs(1 + total_error / 10)
  )

# simulate
sim_dat <- sim_dat |>
  mutate(
    TotalCPS = rpois(
      n = nrow(sim_dat),
      lambda = TotalCPS_l
    ),
    Confirmed = rpois(
      n = nrow(sim_dat),
      lambda = Confirmed_l
    ),
    Prev = rpois(
      n = nrow(sim_dat),
      lambda = Prev_l
    ),
    FC = rpois(
      n = nrow(sim_dat),
      lambda = FC_l
    )
  )

# remove errors, output data in admin format
c_long <- sim_dat |>
  select(Child:GROUP, Wave, TotalCPS:FC) |>
  rename(COUNTY = County)

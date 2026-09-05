# simulate data -----------------------------------------------------------
library(tidyverse)
library(brms)
library(tidybayes)
set.seed(1)

# construct anonymized sim data from observed
# # pull IDs
ID_list <- read_csv("./consort_data/group_assignment.csv") |>
  rename(GROUP = Group) |>
  mutate(ID_n = 1:n())
w1 <- read_csv("./data/w1_reduced.csv") |>
  mutate(COUNTY = str_sub(County, 1, -8)) |>
  select(ID, COUNTY, n_child) |>
  left_join(ID_list) |>
  filter(!(is.na(GROUP))) |>
  mutate(COUNTY = as.numeric(factor(COUNTY))) |>
  mutate(n_child = ifelse(n_child == 0, 1, n_child)) |>
  select(ID_n, GROUP, COUNTY, n_child)
# # over 18s, 1012, 986, subset out 26, n = 986 complete cases
over_18s <- w1 |>
  filter(n_child > 1) |>
  sample_n(sum(w1$n_child) - 986)
w1 <- w1 |>
  mutate(n_child = ifelse(ID_n %in% over_18s$ID_n, n_child - 1, n_child)) |>
  rename(ID = ID_n)
# modify to

sim_dat <- write_csv(w1, "./data/anon_sim_dat.csv")

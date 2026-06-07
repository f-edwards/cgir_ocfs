# impute admin data for study withdrawals
# created 3/11/26: FE
# log: hello world
# load packages -----------------------------------------------------------
library(tidyverse)
library(mice)
set.seed(1)
# data read ---------------------------------------------------------------
# source("sim_ocfs_data.r")

# create list of PINs in data
ID_list <- c_long |> 
  select(ID) |> 
  distinct()

# read wave 1 survey, return subset of useful variables for imputation
w1 <- read_csv("./data/w1_reduced.csv") |> 
  mutate(COUNTY = str_sub(County, 1, -8)) |> 
  select(ID, COUNTY, unit, n_child,
         Age_c, Gender_s, Race_White, Race_Black, Edu, SexOrient_Straight, 
         SF_PhysicalFunctioning, SF_RoleLimitationsPhys,
         Kessler_Score:Chaos_Score,
         AH_Total:ParentalStressTotal) |> 
  mutate(Gender_f = Gender_s == "Female") |> 
  select(-Gender_s)

# read IDs of drops
drops <- read_csv("./data/drops.csv")

# prep df for drops -------------------------------------------------------
# read
extras <- w1 |>
  filter(ID%in%drops$ID)

# create empty DF for children of drops
extras_temp <- data.frame(
  Child = as.character(sample(1e4:1e5, sum(extras$n_child))),
  ID = rep(extras$ID, extras$n_child),
  COUNTY = rep(extras$COUNTY, extras$n_child),
  GROUP = rep(extras$unit, extras$n_child),
  NoBaseline_FLAG = 0,
  TotalCPS = NA,
  Confirmed = NA,
  Prev = NA,
  FC = NA)

# four waves
extras_df <- extras_temp |> 
  mutate(Wave = 1) |> 
  bind_rows(extras_temp |> 
              mutate(Wave = 2)) |> 
  bind_rows(extras_temp |> 
              mutate(Wave = 3)) |> 
  bind_rows(extras_temp |> 
              mutate(Wave = 4)) |> 
  select(Child, GROUP, ID, COUNTY, Wave, 
         NoBaseline_FLAG, 
         TotalCPS:FC)

# bind
temp <- c_long |> 
  bind_rows(extras_df)

# attach survey, prefer admin county classification
temp <- temp |> 
  left_join(w1 |> 
              select(-COUNTY)) |> 
  mutate(COUNTY = factor(COUNTY), 
         GROUP = factor(GROUP),
         Wave = factor(Wave),
         Edu = factor(Edu))

# imputation --------------------------------------------------------------
# setup predictor matrix
predmat <- make.predictorMatrix(temp)
# turn off IDs
predmat[,"Child"] <- 0
predmat[,"ID"] <- 0
predmat[,"unit"] <- 0
predmat[,"NoBaseline_FLAG"] <- 0
temp_imp <- mice(temp, 
                   predictorMatrix = predmat,
                   m = 5)

# output
saveRDS(temp_imp, file = "imputed_data_DO_NOT_SEND_ME.rds")



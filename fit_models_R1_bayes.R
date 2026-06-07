set.seed(8625)
### run once, then comment out with #
# install.packages(c("tidyverse", "brms", "tidybayes"))
# load packages
library(tidyverse)
library(tidybayes)
library(brms)
### settings for brms
cores <- parallel::detectCores()
model_controls <- list(
  adapt_delta = 0.999, 
  stepsize = 0.01,
  max_treedepth = 15)
iter <- 1e4

n_imps <- max(c_long_imp$.imp)
start_time <- Sys.time()
# regression models -------------------------------------------------------
# estimate focal ITT models with all cases
# sensitivity models: 
# 1) Per-protocol a: no inelligibles
# 2) Original randomization: cases originally assigned to C but moved to T treated as C
# 3) Per-protocoal b: no inelligibles, no withdrawals (complete-case)
# 4) with covars for ITT for potential imbalance
# re 4 also include bivars on included predictors

# convert factors
c_long_imp <- c_long_imp |> 
  mutate(
    across(
      c(COUNTY, Wave, 
        Race_White, SexOrient_Straight, Gender_f),
      as.factor))

# subset for sensitivity models

c_imp_s3 <- c_long_imp |> 
  filter(.imp==0,
         !(is.na(Confirmed)))

# regression models -------------------------------------------------------
# priors ------------------------------------------------------------------
priorsL<-c(
  set_prior("normal(0,4)", class = "b"),
  set_prior("normal(0,5)", class = "Intercept"),
  set_prior("student_t(3, 0, 2.5)", class = "sd"))

# total reports -----------------------------------------------------------
# focal model
TotalCPS_b<- brm_multiple(
  TotalCPS > 0 ~ Wave + GROUP:Wave + COUNTY + (1|ID/Child),
  family = bernoulli(),
  data = c_imp, 
  prior = priorsL, iter = iter,
  cores = cores, control = model_controls)
#### output
# parameter estimates and CI
sink("./output/TotalCPS_b.txt")
print(TotalCPS_b)
sink()
# posteriors for fixed effects
tidy_draws(TotalCPS_b) |> 
  select(.chain:Intercept) |> 
  write_csv("./output/TotalCPS_b.csv")

# sensitivity models


TotalCPS_b_s3 <- brm(
  TotalCPS > 0 ~ Wave + GROUP:Wave + COUNTY + (1|ID/Child),
  family = bernoulli(),
  data = c_imp_s3,
  prior = priorsL, iter = iter,
  cores = cores, control = model_controls)

sink("./output/TotalCPS_b_s3.txt")
print(TotalCPS_b_s3)
sink()

TotalCPS_b_s4 <- brm_multiple(
  TotalCPS > 0 ~
    Wave + GROUP:Wave + COUNTY +
    n_child + Gender_f + Race_White + Edu + SexOrient_Straight +
    scale(Kessler_Score) + scale(AH_Total) + scale(Total_FW_Score) +
    (1|ID/Child),
  data = c_imp,
  family = bernoulli(),
  prior = priorsL, iter = iter,
  cores = cores, control = model_controls)

sink("./output/TotalCPS_b_s4.txt")
print(TotalCPS_b_s4)
sink()


# confirmed -----------------------------------------------------------
# focal model
Confirmed_b<- brm_multiple(
  Confirmed > 0 ~ Wave + GROUP:Wave + COUNTY + (1|ID/Child),
  family = bernoulli(),
  data = c_imp, 
  prior = priorsL, iter = iter,
  cores = cores, control = model_controls)

#### output
# parameter estimates and CI
sink("./output/Confirmed_b.txt")
print(Confirmed_b)
sink()
# posteriors for fixed effects
tidy_draws(Confirmed_b) |> 
  select(.chain:Intercept) |> 
  write_csv("./output/Confirmed_b.csv")


# sensitivity models


Confirmed_b_s3 <- brm(
  Confirmed > 0 ~ Wave + GROUP:Wave + COUNTY + (1|ID/Child),
  family = bernoulli(),
  data = c_imp_s3,
  prior = priorsL, iter = iter,
  cores = cores, control = model_controls)

sink("./output/Confirmed_b_s3.txt")
print(Confirmed_b_s3)
sink()

Confirmed_b_s4 <- brm_multiple(
  Confirmed > 0 ~
    Wave + GROUP:Wave + COUNTY +
    n_child + Gender_f + Race_White + Edu + SexOrient_Straight +
    scale(Kessler_Score) + scale(AH_Total) + scale(Total_FW_Score) +
    (1|ID/Child),
  data = c_imp,
  family = bernoulli(),
  prior = priorsL, iter = iter,
  cores = cores, control = model_controls)

sink("./output/Confirmed_b_s4.txt")
print(Confirmed_b_s4)
sink()

# prev -----------------------------------------------------------
# focal model
Prev_b<- brm_multiple(
  Prev > 0 ~ Wave + GROUP:Wave + COUNTY + (1|ID/Child),
  family = bernoulli(),
  data = c_imp, 
  prior = priorsL, iter = iter,
  cores = cores, control = model_controls)

#### output
# parameter estimates and CI
sink("./output/Prev_b.txt")
print(Prev_b)
sink()
# posteriors for fixed effects
tidy_draws(Prev_b) |> 
  select(.chain:Intercept) |> 
  write_csv("./output/Prev_b.csv")


# sensitivity models


Prev_b_s3 <- brm(
  Prev > 0 ~ Wave + GROUP:Wave + COUNTY + (1|ID/Child),
  family = bernoulli(),
  data = c_imp_s3,
  prior = priorsL, iter = iter,
  cores = cores, control = model_controls)

sink("./output/Prev_b_s3.txt")
print(Prev_b_s3)
sink()

Prev_b_s4 <- brm_multiple(
  Prev > 0 ~
    Wave + GROUP:Wave + COUNTY +
    n_child + Gender_f + Race_White + Edu + SexOrient_Straight +
    scale(Kessler_Score) + scale(AH_Total) + scale(Total_FW_Score) +
    (1|ID/Child),
  data = c_imp,
  family = bernoulli(),
  prior = priorsL, iter = iter,
  cores = cores, control = model_controls)

sink("./output/Prev_b_s4.txt")
print(Prev_b_s4)
sink()

# foster care -------------------------------------------------------------
FC_b<- brm_multiple(
  FC > 0 ~ Wave + GROUP:Wave + COUNTY + (1|ID/Child),
  family = bernoulli(),
  data = c_imp, 
  prior = priorsL, iter = iter,
  cores = cores, control = model_controls)

#### output
# parameter estimates and CI
sink("./output/FC_b.txt")
print(FC_b)
sink()
# posteriors for fixed effects
tidy_draws(FC_b) |> 
  select(.chain:Intercept) |> 
  write_csv("./output/FC_b.csv")


# sensitivity models


FC_b_s3 <- brm(
  FC > 0 ~ Wave + GROUP:Wave + COUNTY + (1|ID/Child),
  family = bernoulli(),
  data = c_imp_s3,
  prior = priorsL, iter = iter,
  cores = cores, control = model_controls)

sink("./output/FC_b_s3.txt")
print(FC_b_s3)
sink()

FC_b_s4 <- brm_multiple(
  FC > 0 ~
    Wave + GROUP:Wave + COUNTY +
    n_child + Gender_f + Race_White + Edu + SexOrient_Straight +
    scale(Kessler_Score) + scale(AH_Total) + scale(Total_FW_Score) +
    (1|ID/Child),
  data = c_imp,
  family = bernoulli(),
  prior = priorsL, iter = iter,
  cores = cores, control = model_controls)

sink("./output/FC_b_s4.txt")
print(FC_b_s4)
sink()

end_time <- Sys.time()
end_time - start_time

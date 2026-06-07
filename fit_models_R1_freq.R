set.seed(8625)

### run once, then comment out with #
# install.packages(c("tidyverse", "lme4", "broom.mixed"))

# load packages
library(tidyverse)
library(lme4)
library(broom.mixed)

n_imps <- max(c_long_imp$.imp)
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

# Total CPS reports -------------------------------------------------------
print("estimating total CPS report models")
# empty lists for model output
TotalCPS <- list()
TotalCPS_s4 <- list()
# estimation
for(i in 1:n_imps){
  print(paste("estimating models for imputation", i))
  # full data
  temp <- c_long_imp |> 
    filter(.imp==i)
  # original randomization, all cases
  
  TotalCPS[[i]] <-  glmer(
    TotalCPS>0 ~
      Wave + 
      GROUP:Wave + 
      COUNTY + 
      (1|ID/Child),
    family = "binomial",
    data = temp,
    glmerControl(optimizer = "bobyqa"))
  
  
  TotalCPS_s4[[i]] <-  glmer(
    TotalCPS>0 ~
      n_child + Gender_f + 
      Race_White + Edu + SexOrient_Straight + 
      scale(Kessler_Score) + 
      scale(AH_Total) + 
      scale(Total_FW_Score) + 
      Wave + 
      GROUP:Wave + 
      COUNTY + 
      (1|ID/Child),
    family = "binomial",
    data = temp,
    glmerControl(optimizer = "bobyqa"))
}

# complete case model
TotalCPS_s3 <- glmer(
  TotalCPS>0 ~
    Wave + 
    GROUP:Wave + 
    COUNTY + 
    (1|ID/Child),
  family = "binomial",
  data = c_long_imp |> 
    filter(.imp==0),
  glmerControl(optimizer = "bobyqa"))

# pooled output
pool(TotalCPS) |> 
  summary() |> 
  write_csv("./output/m_TotalCPS.csv")
print("./output/TotalCPS.txt")
print(TotalCPS)

tidy(TotalCPS_s3) |> 
  write_csv("./output/m_TotalCPS_s3.csv")

pool(TotalCPS_s4) |> 
  summary() |> 
  write_csv("./output/m_TotalCPS_s4.csv")


# Confirmed report -------------------------------------------------------
print("estimating confirmed report models")
# empty lists for model output
Confirmed <- list()
Confirmed_s4 <- list()
# estimation
for(i in 1:n_imps){
  print(paste("estimating models for imputation", i))
  # full data, ITT
  temp <- c_long_imp |> 
    filter(.imp==i)
  
  #ITT
  
  Confirmed[[i]] <-  glmer(
    Confirmed>0 ~
      Wave + 
      GROUP:Wave + 
      COUNTY + 
      (1|ID/Child),
    family = "binomial",
    data = temp,
    glmerControl(optimizer = "bobyqa"))
  
  # with controls
  
  Confirmed_s4[[i]] <-  glmer(
    Confirmed>0 ~
      n_child + Gender_f + 
      Race_White + Edu + SexOrient_Straight + 
      scale(Kessler_Score) + 
      scale(AH_Total) + 
      scale(Total_FW_Score) + 
      Wave + 
      GROUP:Wave + 
      COUNTY + 
      (1|ID/Child),
    family = "binomial",
    data = temp,
    glmerControl(optimizer = "bobyqa"))
}

# complete case model
Confirmed_s3 <- glmer(
  Confirmed>0 ~
    Wave + 
    GROUP:Wave + 
    COUNTY + 
    (1|ID/Child),
  family = "binomial",
  data = c_long_imp |> 
    filter(.imp==0),
  glmerControl(optimizer = "bobyqa"))

# pooled output
pool(Confirmed) |> 
  summary() |> 
  write_csv("./output/m_Confirmed.csv")
sink("./output/Confirmed.txt")
print(Confirmed)
sink()

tidy(Confirmed_s3) |> 
  write_csv("./output/m_Confirmed_s3.csv")
pool(Confirmed_s4) |> 
  summary() |> 
  write_csv("./output/m_Confirmed_s4.csv")



# Preventive services -------------------------------------------------------
print("estimating preventive services models")
# empty lists for model output
Prev <- list()
Prev_s4 <- list()
# estimation
for(i in 1:n_imps){
  print(paste("estimating models for imputation", i))
  # full data
  temp <- c_long_imp |> 
    filter(.imp==i)

    Prev[[i]] <-  glmer(
    Prev>0 ~
      Wave + 
      GROUP:Wave + 
      COUNTY + 
      (1|ID/Child),
    family = "binomial",
    data = temp,
    glmerControl(optimizer = "bobyqa"))
  
  Prev_s4[[i]] <-  glmer(
    Prev>0 ~
      n_child + Gender_f + 
      Race_White + Edu + SexOrient_Straight + 
      scale(Kessler_Score) + 
      scale(AH_Total) + 
      scale(Total_FW_Score) + 
      Wave + 
      GROUP:Wave + 
      COUNTY + 
      (1|ID/Child),
    family = "binomial",
    data = temp,
    glmerControl(optimizer = "bobyqa"))
}

# complete case model
Prev_s3 <- glmer(
  Prev>0 ~
    Wave + 
    GROUP:Wave + 
    COUNTY + 
    (1|ID/Child),
  family = "binomial",
  data = c_long_imp |> 
    filter(.imp==0),
  glmerControl(optimizer = "bobyqa"))

# pooled output
pool(Prev) |> 
  summary() |> 
  write_csv("./output/m_Prev.csv")

sink("./output/Prev.txt")
print(Prev)
sink()

tidy(Prev_s3) |> 
  write_csv("./output/m_Prev_s3.csv")
pool(Prev_s4) |> 
  summary() |> 
  write_csv("./output/m_Prev_s4.csv")
# FC  -------------------------------------------------------
print("estimating foster care models")
# empty lists for model output
FC <- list()
FC_s4 <- list()
# estimation
for(i in 1:n_imps){
  print(paste("estimating models for imputation", i))
  # full data
  temp <- c_long_imp |> 
    filter(.imp==i)
  # original randomization, all cases

  
  FC[[i]] <-  glmer(
    FC>0 ~ Wave + GROUP:Wave + 
      COUNTY + 
      (1|ID/Child),
    family = "binomial",
    data = temp,
    glmerControl(optimizer = "bobyqa"))
  

  
  FC_s4[[i]] <-  glmer(
    FC>0 ~
      n_child + Gender_f + 
      Race_White + Edu + SexOrient_Straight + 
      scale(Kessler_Score) + 
      scale(AH_Total) + 
      scale(Total_FW_Score) + 
      Wave + 
      GROUP:Wave + 
      COUNTY + 
      (1|ID/Child),
    family = "binomial",
    data = temp,
    glmerControl(optimizer = "bobyqa"))
}

# complete case model
FC_s3 <- glmer(
  FC>0 ~
    Wave + 
    GROUP:Wave + 
    COUNTY + 
    (1|ID/Child),
  family = "binomial",
  data = c_long_imp |> 
    filter(.imp==0),
  glmerControl(optimizer = "bobyqa"))

# pooled output
pool(FC) |> 
  summary() |> 
  write_csv("./output/m_FC.csv")
sink("./output/FC.txt")
print(FC)
sink()


tidy(FC_s3) |> 
  write_csv("./output/m_FC_s3.csv")
pool(FC_s4) |> 
  summary() |> 
  write_csv("./output/m_FC_s4.csv")
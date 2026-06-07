# parse w1 survey data, ID drops, extract n children
library(tidyverse)
library(readxl)
drops <- read_csv("./consort_data/drops.csv")


w1 <- read_xlsx("./data/Baseline.xlsx") |> 
  mutate(n_child = 6 - 
           (is.na(ChildDOB1_1_1) + is.na(ChildDOB1_2_1) + is.na(ChildDOB1_3_1) + 
              is.na(ChildDOB1_4_1) + is.na(ChildDOB1_5_1) + is.na(ChildDOB1_6_1))) |> 
  rename(ID = `unit id`) 


# parse instruments -------------------------------------------------------
# in codebook order: 

# age
w1 <- w1 |> 
  mutate(DOB = paste(
    Q6_1, Q6_2, Q6_3)) |> 
  select(-Q6_1, -Q6_2, -Q6_3) %>% 
  mutate(Age_c = floor(interval(mdy(DOB), mdy_hm(StartDate)) %/%  years(1)))

# gender
w1<-w1 |> 
  mutate(Gender_s = Gender,
         Gender = case_when(Gender == "Male" ~ 1,
                            Gender == "Female" ~ 2,
                            Gender == "Other (Non-binary, agender, gender-fluid, etc):" ~ 3))

# Marital / partner stat
w1 <- w1 |> 
  rename(MaritalStat = MaritalStatus,
         PartnerStat = PartnerStatus) |> 
  mutate(
    MaritalStat = case_when(
      MaritalStat == "Single" ~ 3,
      MaritalStat == "Partnered/In a Relationship" ~ 2,
      MaritalStat == "Married" ~ 1),
    PartnerStat = case_when(
      PartnerStat == "Yes" ~ 1,
      PartnerStat == "No" ~ 0),
    PartnerGender = case_when(
      PartnerGender == "Male" ~ 1,
      PartnerGender == "Female" ~ 2,
      PartnerGender == "Other (Non-binary, agender, gender-fluid, etc):" ~ 3,
    )
  )

# race/ethnicity
w1 <- w1 |> 
  mutate(Ethnicity = case_when(
    Ethnicity == "Yes" ~ 1,
    Ethnicity == "No" ~ 0),
    Race_White = Race == "White",
    Race_Black = Race == "Black or African American",
    Race_AIAN = Race == "American Indian or Alaska Native or First Nations",
    Race_Asian = Race == "Asian",
    Race_HawPI = Race == "Native Hawaiian and Other Pacific Islander",
    Race_TwoOrMore = (Race == "Two or more races") | 
      str_detect(Race, ","),
    Race_Other = Race == "Some other race:") 

# education
w1 <- w1 |> 
  rename(Edu_s = Edu) |> 
  mutate(Edu = case_when(
    Edu_s == "No formal education" ~ 1,
    Edu_s == "Elementary school (through Grade 5)" ~ 2,
    Edu_s == "Middle school (6th grade - 9th grade)" ~ 3,
    Edu_s == "High school (10th - 12th grades)" ~ 4,
    Edu_s == "High school diploma" ~ 5,
    Edu_s == "GED (diploma equivalency test)" ~ 6,
    Edu_s == "Trade or technical school" ~ 7,
    Edu_s == "Associate's Degree (2-year college degree)" ~ 8,
    Edu_s == "Some college" ~ 9,
    Edu_s == "Bachelor's Degree (4-year college degree)" ~ 10,
    Edu_s == "Other post-graduate degree" ~ 11,
    Edu_s == "Other education choice not listed" ~ 12))

# language
w1 <- w1 |> 
  mutate(
    LangOther = ifelse(
      !LangHome%in%c("Spanish", "English"),
      LangHome,
      NA),
    LangHome = case_when(
      LangHome == "Spanish" ~ 2,
      LangHome == "English" ~ 1,
      is.na(LangHome) ~ NA,
      !LangHome%in%c("Spanish", "English") ~ 99))

# sexual orientation
w1 <- w1 |> 
  rename(SexOrient = Orientation) |> 
  mutate(SexOrient_Straight = str_detect(SexOrient, "Straight"),
         SexOrient_Gay = str_detect(SexOrient, "Gay"),
         SexOrient_Queer = str_detect(SexOrient, "Queer"),
         SexOrient_Lesbian = str_detect(SexOrient, "Lesbian"),
         SexOrient_Bisexual = str_detect(SexOrient, "Bisexual"),
         SexOrient_Questioning = str_detect(SexOrient, "Questioning"))

# - insurance 
w1<-w1 %>% 
  mutate(ParentInsurance = ParentInsurance == "Yes",
         ChildHealthInsurance = ChildHealthInsurance == "No",
         ChildInsuranceNo = as.numeric(ifelse(
           is.na(ChildInsuranceNo),
           0,
           ChildInsuranceNo))) 

# - SF36
w1<-w1 %>% 
  rename(SF_HealthGen = HealthGeneral,
         SF_HealthLim_1 = HealthLimits_1,
         SF_HealthLim_2 = HealthLimits_2,
         SF_HealthLim_3 = HealthLimits_3,
         SF_HealthLim_4 = HealthLimits_4,
         SF_HealthLim_5 = HealthLimits_5,
         SF_HealthLim_6 = HealthLimits_6,
         SF_HealthLim_7 = HealthLimits_7,
         SF_HealthLim_8 = HealthLimits_8,
         SF_HealthLim_9 = HealthLimits_9,
         SF_HealthLim_10 = HealthLimits_10,
         SF_Phys_1 = PhysicalProbs_1,
         SF_Phys_2 = PhysicalProbs_2,
         SF_Phys_3 = PhysicalProbs_3,
         SF_Phys_4 = PhysicalProbs_4,
         SF_HealthState_1 = HealthStatements_1,
         SF_HealthState_2 = HealthStatements_2,
         SF_HealthState_3 = HealthStatements_3,
         SF_HealthState_4 = HealthStatements_4) %>% 
  mutate(
    SF_HealthGen= case_when(
      SF_HealthGen== "Excellent" ~ 100,
      SF_HealthGen== "Very good" ~ 75,
      SF_HealthGen== "Good" ~ 50,
      SF_HealthGen== "Fair" ~ 25,
      SF_HealthGen== "Poor" ~ 0,
      is.na(SF_HealthGen) ~ NA),
    across(SF_HealthLim_1:SF_HealthLim_10,
           ~ case_when(
             . == "Yes, limited a lot" ~ 0,
             . == "Yes, limited a little" ~ 50,
             . == "No, not limited at all" ~ 100,
             is.na(.) ~ NA)),
    across(SF_Phys_1:SF_Phys_4,
           ~ case_when(
             . == "Yes" ~ 0,
             . == "No" ~ 100,
             is.na(.) ~ NA)),
    SF_HealthState_1 = case_when(
      SF_HealthState_1 == "Definitely True" ~ 0,
      SF_HealthState_1 == "Mostly true" ~ 25,
      SF_HealthState_1 == "Don't know" ~ 50,
      SF_HealthState_1 == "Mostly false" ~ 75,
      SF_HealthState_1 == "Definitely false" ~ 100,
      is.na(SF_HealthState_1) ~ NA),
    SF_HealthState_2 = case_when(
      SF_HealthState_2 == "Definitely True" ~ 100,
      SF_HealthState_2 == "Mostly true" ~ 75,
      SF_HealthState_2 == "Don't know" ~ 50,
      SF_HealthState_2 == "Mostly false" ~ 25,
      SF_HealthState_2 == "Definitely false" ~ 0,
      is.na(SF_HealthState_2) ~ NA),
    SF_HealthState_3 = case_when(
      SF_HealthState_3 == "Definitely True" ~ 0,
      SF_HealthState_3 == "Mostly true" ~ 25,
      SF_HealthState_3 == "Don't know" ~ 50,
      SF_HealthState_3 == "Mostly false" ~ 75,
      SF_HealthState_3 == "Definitely false" ~ 100,
      is.na(SF_HealthState_3) ~ NA),
    SF_HealthState_4 = case_when(
      SF_HealthState_4 == "Definitely True" ~ 100,
      SF_HealthState_4 == "Mostly true" ~ 75,
      SF_HealthState_4 == "Don't know" ~ 50,
      SF_HealthState_4 == "Mostly false" ~ 25,
      SF_HealthState_4 == "Definitely false" ~ 0,
      is.na(SF_HealthState_4) ~ NA)) |> 
  mutate(across(SF_HealthGen:SF_HealthState_4, as.numeric)) %>% 
  mutate(SF_PhysicalFunctioning = 
           (SF_HealthLim_1 + SF_HealthLim_2 + SF_HealthLim_3 + 
              SF_HealthLim_4 + SF_HealthLim_5 + SF_HealthLim_6 + 
              SF_HealthLim_7 + SF_HealthLim_8 + SF_HealthLim_9 + 
              SF_HealthLim_10)/10,
         SF_RoleLimitationsPhys = 
           (SF_Phys_1 + SF_Phys_2 + SF_Phys_3 + SF_Phys_4)/4,
         SF_GenHealth = 
           (SF_HealthGen + SF_HealthState_1 + SF_HealthState_2 + 
              SF_HealthState_3 + SF_HealthState_4) / 5) 

# - Kessler 10+
w1<-w1 %>%
  mutate(across(K10_1:K10_10,
                ~ case_when(
                  . == "None of the time" ~ 1,
                  . == "A little of the time" ~ 2,
                  . == "Some of the time" ~ 3,
                  . == "Most of the time" ~ 4,
                  . == "All of the time" ~ 5,
                  is.na(.) ~ NA)),
         K10physical_1 = case_when(
           K10physical_1 == "None of the time" ~ 1,
           K10physical_1 == "A little of the time" ~ 2,
           K10physical_1 == "Some of the time" ~ 3,
           K10physical_1 == "Most of the time" ~ 4,
           K10physical_1 == "All of the time" ~ 5)) %>% 
  rename(K10phys = K10physical_1,
         K10med = K10med_1) |> 
  mutate(Kessler_Score = rowSums(across(K10_1:K10_10)))

# - Perceived Stress Scale
# 0 - never, 1 - almost never, 2 - sometimes, 3 - fairly often, 4 - very often
# reverse code 4, 5, 7, 8
# retain 4 qs in survey: q2 (1); q4 (2); q5 (3); q10 (4)

w1<-w1 %>%
  mutate(across(c(PerceivedStress_1, PerceivedStress_4),
                ~ case_when(
                  . == "Never" ~ 0,
                  . == "Almost Never" ~ 1,
                  . == "Sometimes" ~ 2,
                  . == "Fairly Often" ~ 3,
                  . == "Very Often" ~ 4,
                  is.na(.) ~ NA)),
         across(c(PerceivedStress_2, PerceivedStress_3),
                ~ case_when(
                  . == "Never" ~ 4,
                  . == "Almost Never" ~ 3,
                  . == "Sometimes" ~ 2,
                  . == "Fairly Often" ~ 1,
                  . == "Very Often" ~ 0,
                  is.na(.) ~ NA))) %>% 
  rename(PS_2 = PerceivedStress_1,
         PS_4 = PerceivedStress_2,
         PS_5 = PerceivedStress_3,
         PS_10 = PerceivedStress_4) |> 
  mutate(across(PS_2:PS_10, as.numeric),
         PS_ShortTotal = rowSums(across(PS_2:PS_10)))

# - CHAOS scale
w1<-w1 %>% 
  mutate(across(c(CHAOS_1, CHAOS_2,
                  CHAOS_4, CHAOS_7,
                  CHAOS_12, CHAOS_14,
                  CHAOS_15),
                ~ case_when(
                  . == "Very much" ~ 1,
                  . == "Somewhat" ~ 2,
                  . == "A little bit" ~ 3,
                  . == "Not at all" ~ 4)),
         across(c(CHAOS_3, CHAOS_5,
                  CHAOS_6, CHAOS_8,
                  CHAOS_9, CHAOS_10,
                  CHAOS_11, CHAOS_13),
                ~ case_when(
                  . == "Very much" ~ 4,
                  . == "Somewhat" ~ 3,
                  . == "A little bit" ~ 2,
                  . == "Not at all" ~ 1))) %>% 
  rename_with(str_to_title, starts_with("CHAOS")) |> 
  mutate(across(Chaos_1:Chaos_15, as.numeric),
         Chaos_Score = rowSums(across(Chaos_1:Chaos_15)))

# - Household Food Insecurity
w1<-w1 %>%
  mutate(across(FoodInsecurity1:FoodInsecurityUtil,
                ~ case_when(
                  . == "Yes" ~ 1,
                  . == "No" ~ 0,
                  is.na(.) ~ NA
                ))) %>% 
  rename(HHFI_Insuf = FoodInsecurity1,
         HHFI_Pref = FoodInsecurity2,
         HHFI_NotPref = FoodInsecurity4,
         HHFI_Less = FoodInsecurity6,
         HHFI_Util = FoodInsecurityUtil)

# - Adult Hope Scale
w1<-w1 %>% 
  mutate(across(AdultHope_1:AdultHope_12,
                ~ case_when(
                  . == "Definitely False" ~ 1,
                  . == "Mostly False" ~ 2,
                  . == "Somewhat False" ~ 3,
                  . == "Slightly False" ~ 4,
                  . == "Slightly True" ~ 5,
                  . == "Somewhat True" ~ 6,
                  . == "Mostly True" ~ 7,
                  . == "Definitely True" ~ 8,
                  is.na(.) ~ NA)))
# handle names
pos<-which(grepl("AdultHope" , names(w1)))
nums<-1:12
names(w1)[pos]<-paste("AH", nums, sep = "_")

w1 <- w1 |> 
  mutate(across(AH_1:AH_12, as.numeric),
         AH_Agency = AH_2 + AH_9 + AH_10 + AH_12,
         AH_Pathway = AH_1 + AH_4 + AH_6 + AH_8,
         AH_Total = AH_Agency + AH_Pathway)

# - Financial Well-Being Scale
w1<-w1 %>% 
  mutate(across(c(FinWell1Statement_1:FinWell1Statement_2,
                  FinWell1Statement_4),
                ~ case_when(
                  . == "Completely" ~ 4,
                  . == "Very well" ~ 3,
                  . == "Somewhat" ~ 2,
                  . == "Very little" ~ 1,
                  . == "Not at all" ~ 0,
                  is.na(.) ~ NA)),
         across(c(FinWell1Statement_3,FinWell1Statement_5,
                  FinWell1Statement_6),
                ~ case_when(
                  . == "Completely" ~ 0,
                  . == "Very well" ~ 1,
                  . == "Somewhat" ~ 2,
                  . == "Very little" ~ 3,
                  . == "Not at all" ~ 4,
                  is.na(.) ~ NA)),
         across(c(FinWell2Statment_1, FinWell2Statment_3,
                  FinWell2Statment_4),
                ~ case_when(
                  . == "Always" ~ 0,
                  . == "Often" ~ 1,
                  . == "Sometimes" ~ 2,
                  . == "Rarely" ~ 3,
                  . == "Never" ~ 4,
                  is.na(.) ~ NA)),
         FinWell2Statment_2 =
           case_when(FinWell2Statment_2 == "Always" ~ 4,
                     FinWell2Statment_2 == "Often" ~ 3,
                     FinWell2Statment_2 == "Sometimes" ~ 2,
                     FinWell2Statment_2 == "Rarely" ~ 1,
                     FinWell2Statment_2 == "Never" ~ 0,
                     is.na(FinWell2Statment_2) ~ NA)) %>% 
  rename(FW_1_1 = FinWell1Statement_1,
         FW_1_2 = FinWell1Statement_2,
         FW_1_3 = FinWell1Statement_3,
         FW_1_4 = FinWell1Statement_4,
         FW_1_5 = FinWell1Statement_5,
         FW_1_6 = FinWell1Statement_6,
         FW_2_1 = FinWell2Statment_1,
         FW_2_2 = FinWell2Statment_2,
         FW_2_3 = FinWell2Statment_3,
         FW_2_4 = FinWell2Statment_4) |> 
  mutate(across(FW_1_1:FW_2_4, as.numeric),
         Total_FW_Score = FW_1_1 + FW_1_2 + FW_1_3 + 
           FW_1_4 + FW_1_5 + FW_1_6 + FW_2_1 + 
           FW_2_2 + FW_2_3 + FW_2_4)

# - medical care 
w1<-w1 %>% 
  mutate(MedicalAccess = (MedicalAccess == "Yes")) %>%
  mutate(MedicalAccess_t_Medical = 
           ifelse(
             !(is.na(MedicalAccessNo)),
             grepl("Medical Care", MedicalAccessNo),
             NA),
         MedicalAccess_t_Dental = 
           ifelse(
             !(is.na(MedicalAccessNo)),
             grepl("Dental Care", MedicalAccessNo),
             NA),
         MedicalAccess_t_Vision = 
           ifelse(
             !(is.na(MedicalAccessNo)),
             grepl("Vision Care", MedicalAccessNo),
             NA),
         MedicalAccess_t_Hearing = 
           ifelse(
             !(is.na(MedicalAccessNo)),
             grepl("Hearing Care", MedicalAccessNo),
             NA),
         MedicalAccess_t_Mental = 
           ifelse(
             !(is.na(MedicalAccessNo)),
             grepl("Mental Health Services", MedicalAccessNo),
             NA),
         MedicalAccess_t_Other = 
           ifelse(
             !(is.na(MedicalAccessNo)),
             grepl("Other: please specify", MedicalAccessNo),
             NA))

# - Parental Stress Scale
w1<-w1 %>%
  mutate(across(c(ParentalStressScale_1, ParentalStressScale_2,
                  ParentalStressScale_5:ParentalStressScale_8,
                  ParentalStressScale_17:ParentalStressScale_18),
                ~ case_when(
                  . == "Strongly Agree" ~ 1,
                  . == "Agree" ~ 2,
                  . == "Undecided" ~ 3,
                  . == "Disagree" ~ 4,
                  . == "Strongly Disagree" ~ 5,
                  is.na(.) ~ NA)),
         across(c(ParentalStressScale_3:ParentalStressScale_4,
                  ParentalStressScale_9:ParentalStressScale_16),
                ~ case_when(
                  . == "Strongly Agree" ~ 5,
                  . == "Agree" ~ 4,
                  . == "Undecided" ~ 3,
                  . == "Disagree" ~ 2,
                  . == "Strongly Disagree" ~ 1,
                  is.na(.) ~ NA))) %>% 
  mutate(across(ParentalStressScale_1:ParentalStressScale_18, as.numeric),
         ParentalStressTotal = rowSums(across(ParentalStressScale_1:ParentalStressScale_18)))

w1 <- w1 |> 
  select(ID, County, unit, n_child,
         Age_c:Total_FW_Score, ParentalStressTotal)
# output
write_csv(w1, "./data/w1_reduced.csv")

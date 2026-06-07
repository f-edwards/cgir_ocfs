# Make all inputs and outputs for CGIR admin data analysis
# log: updated 3/11/26 for first major revision
# instructions: source me to run everything needed
# caution: Bayesian models will take a looong time because of imputed data
# packages ----------------------------------------------------------------
library(tidyverse)
library(lme4)
library(brms)
library(patchwork)
library(broom.mixed)
library(mice)
# use install.packages("package-name") if any fail to load
# prepare output directory ------------------------------------------------
# email contents of output to FE when complete

# data input --------------------------------------------------------------
# for FE, simulation, for HT read_csv
source("sim_ocfs_data.R")
# make_c_long will pivot wide to long
source("make_c_long.r")

# imputation --------------------------------------------------------------
# first time, source to generate imputed data from withdrawals. 
# will add 19 x 4 rows to c_long for missing cases
# after one pass, use readRDS to save time
if(!(file.exists("imputed_data_DO_NOT_SEND_ME.rds"))){
  source("./impute_drops.R")
} 

c_imp <- readRDS("imputed_data_DO_NOT_SEND_ME.rds")
c_long_imp <- mice::complete(c_imp,
                              action = "long",
                              include = T)

# descriptives ------------------------------------------------------------
source("make_descriptives.R")

### ADD A BIVAR THING, MAKE SOME SCATTERPLOTS OR SOMETHING
### FOR prognostics:outcomes, demographics:outcomes

# model estimation --------------------------------------------------------

# frequentist models
source("fit_models_R1_freq.R")

# if frequentist models throw warnings, go Bayesian
# can handle the more complex multilevel structure with child:hh nesting
# bayesian models
# will take ~6 hours to execute
source("fit_models_R1_bayes.R")


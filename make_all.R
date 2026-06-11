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
# for FE simulation
source("sim_ocfs_data.R")

# FOR OCFS DATA make_c_long will pivot wide to long
# cdat <-  read_csv("FILENAME.csv")
# source("make_c_long.r")

# imputation --------------------------------------------------------------
# first time, source to generate imputed data from withdrawals.
# will add 19 x 4 rows to c_long for missing cases
# after one pass, use readRDS to save time
source("./impute_drops.R")

# descriptives ------------------------------------------------------------
source("make_descriptives.R")

# model estimation --------------------------------------------------------
source("fit_models.R")

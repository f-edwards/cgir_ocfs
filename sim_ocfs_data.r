set.seed(1)
library(tidyverse)
### define inverse inverse logit
inv.logit <- function(x){
  return(
    exp(x)/(1 + exp(x))
  )
}

# read in empirical distributions
# scale down w1 to six months
# delete after adjusting descriptive tables
dat <- read_csv("./ocfs_output/child_period_bin.csv") 
# pull IDs

ID_list <- read_csv("./consort_data/group_assignment.csv") |> 
  rename(GROUP = Group)

trt_ids <- ID_list |> 
  filter(GROUP=="T")
ctrl_ids <- ID_list |> 
  filter(GROUP=="C")

### set up fake data for model diagnostics
counties <- c("Monroe", "Onondaga", "Westchester")
# sim child and hh 
## county error
county_error <- data.frame(
  County = counties,
  county_error = rnorm(3, 0, 2))
## household error
hh_error <- ID_list |> 
  mutate(hh_error = rnorm(nrow(ID_list), 0, 2),
         County = sample(counties, nrow(ID_list), replace = T)) 

## child error and ID
trt_child <- data.frame(Child = 1:350,
                        GROUP = "T",
                        ID = sample(trt_ids$ID, 350, replace = T),
                        child_error = rnorm(350, 0, 2)) |> 
  left_join(hh_error) |> 
  left_join(county_error) |> 
  mutate(total_error = county_error + hh_error + child_error)

c_child <- data.frame(Child = 350:1000,
                      GROUP = "C",
                      ID = sample(ctrl_ids$ID, 651, replace = T),
                      child_error = rnorm(651, 0, 2)) |> 
  left_join(hh_error) |> 
  left_join(county_error) |> 
  mutate(total_error = county_error + hh_error + child_error)

sim_dat <- bind_rows(trt_child, c_child)

# allocation ------------------------------------------------------------------
# subset to wave
sims <- list()
for(i in 1:4){
  dat_w <- dat |> 
    filter(Wave == i) |> 
    filter(.imp==1)
  # sim total reports bounded by empirical
  total_trt <- sample(trt_child$Child, 
                         as.numeric(dat_w[dat_w$GROUP=="T", "TotalCPS_bin"]), 
                         replace = F,
                         prob = inv.logit(trt_child$total_error)) 
  total_c <- sample(c_child$Child, 
                       as.numeric(dat_w[dat_w$GROUP=="C", "TotalCPS_bin"]), 
                       replace = F,
                       prob = inv.logit(c_child$total_error)) 
  total <- c(total_trt, total_c)
  # sim confirmed 
  confirmed_trt <- sample(trt_child$Child, 
                      as.numeric(dat_w[dat_w$GROUP=="T", "Confirmed_bin"]), 
                      replace = F,
                      prob = inv.logit(trt_child$total_error)) 
  confirmed_c <- sample(c_child$Child, 
                    as.numeric(dat_w[dat_w$GROUP=="C", "Confirmed_bin"]), 
                    replace = F,
                    prob = inv.logit(c_child$total_error)) 
  confirmed <- c(confirmed_trt, confirmed_c)
  # sim prev
  prev_trt <- sample(trt_child$Child, 
                      as.numeric(dat_w[dat_w$GROUP=="T", "Prev_bin"]), 
                      replace = F,
                      prob = inv.logit(trt_child$total_error)) 
  prev_c <- sample(c_child$Child, 
                    as.numeric(dat_w[dat_w$GROUP=="C", "Prev_bin"]), 
                    replace = F,
                    prob = inv.logit(c_child$total_error)) 
  prev <- c(prev_trt, prev_c)
  # sim FC
  FC_trt <- sample(trt_child$Child, 
                     as.numeric(dat_w[dat_w$GROUP=="T", "FC_bin"]), 
                     replace = F,
                     prob = inv.logit(trt_child$total_error)) 
  FC_c <- sample(c_child$Child, 
                   as.numeric(dat_w[dat_w$GROUP=="C", "FC_bin"]), 
                   replace = F,
                   prob = inv.logit(c_child$total_error)) 
  FC <- c(FC_trt, FC_c)
  
  sims[[i]] <- sim_dat |> 
    mutate(Wave = i,
           TotalCPS = Child%in%total,
           Confirmed = Child%in%confirmed,
           Prev = Child%in%prev,
           FC = Child%in%FC)
}

c_long <- bind_rows(sims) |> 
  rename(COUNTY = County)

### check variance of errors
# c_long |> 
#   summarize(c_sd = sd(county_error))
# c_long |> 
#   group_by(COUNTY) |> 
#   summarize(hh_sd = sd(hh_error),
#             child_sd = sd(child_error))
# c_long |> 
#   group_by(ID) |> 
#   summarize(child_sd = sd(child_error)) |> 
#   summarize(mean_sd = mean(child_sd))
# looks reasonable

# remove errors, output data in admin format
c_long <- c_long |> 
  select(Child:ID, COUNTY, Wave:FC)

## add no_baseline_flag in to 6 cases
ids_no_baseline <- sample(ID_list$ID, 6)
c_long <- c_long |> 
  mutate(NoBaseline_FLAG = ID%in%ids_no_baseline)

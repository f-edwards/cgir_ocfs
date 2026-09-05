# power simulation under varying effect sizes, fixed sample
# monte carlo simulation
# fixing event incidence at empirical, with hh, child and county effects
# post-hoc
# revision for amarel run, public release

# simulate data -----------------------------------------------------------
library(tidyverse)
library(brms)
library(tidybayes)
library(rstan)
set.seed(1)
# read anonymized hh data
rstan_options(auto_write = TRUE)
dat <- read_csv("./data/anon_sim_dat.csv")

# allocation ------------------------------------------------------------------
# define lambda
# \lambda_t - \lambda_c = SATE; e^\lambda_t/e^\lambda_c = OR SATE
# set e(p) = c(0.01)

lambda <- data.frame(GROUP = c(rep("C", 4), rep("T", 4)),
                     Wave = 1:4,
                     n = c(rep(643, 4), rep(343, 4)),
                     FC_l = 0.01)

# effects (\theta)
# iterate from small to large
# effect sizes on log scale, given OR target
# so -0.1 = OR 0.9, -0.2 = OR 0.82, -0.5 = OR 0.6, -1 = OR 0.37
# -2 = OR 0.14, -5 = 0.007, -10 = 4*10e-5
# theta <- c(0, -0.1, -0.5, -1, -5)

# to add null scenario
theta <- c(0, -0.1, -0.5, -1, -2, -5)

sims_out <- list()

n_sims <- 100
# index
j <- 0

# monte carlo simulation of foster care entries with county, hh, and child error
# assume sigma_county = 0.25, sigma_hh = 0.3, sigma_child = 0.25
for (t in theta) {
  for (i in 1:n_sims) {
    j <- j + 1
    print(j)

    ### county error, on approximate scale of observed
    county_error <- data.frame(
      County = 1:3,
      county_error = rnorm(3, 0, 0.25)
    )

    # loop over each hh, populate child id, then randomly sample to n
    child_list <- list()
    child_index <- 1
    for (i in unique(dat$ID)) {
      temp <- dat |>
        filter(ID == i)
      if (temp$n_child > 0) {
        child_ids <- data.frame(
          Child = child_index:(child_index + temp$n_child - 1),
          ID = temp$ID,
          County = temp$COUNTY,
          GROUP = temp$GROUP,
          hh_error = rnorm(1, 0, 0.3)
        )
        child_index <- max(child_ids$Child) + 1
        child_list[[i]] <- child_ids
      }
    }
    child_ids <- bind_rows(child_list)

    sim_dat <- child_ids |>
      mutate(child_error = rnorm(nrow(child_ids), 0, 0.25)) |>
      left_join(county_error, by = "County") |>
      mutate(total_error = county_error + hh_error + child_error) |>
      expand_grid(Wave = 1:4) 

    # add treatment effect on lambda scale \lambda = exp(Xb+\theta)
    # SATE as \theta

    sim_dat <- sim_dat |>
      left_join(lambda, b = c("GROUP", "Wave")) |>
      mutate(
        trt_t = (GROUP == "T") * ((Wave == "2") | (Wave == "3")),
        FC_l = exp(log(FC_l) + total_error + t * trt_t)
      )
    
    # diagnostics on error
    # sd of total error ~ 0.4
    # sd(sim_dat$total_error)
    # hist(sim_dat$total_error)
    # confirm lambda looks reasonable
    # sim_dat |>
    #   group_by(GROUP, Wave) |>
    #   summarize(FC_l = mean(FC_l))

    # simulate
    sim_dat <- sim_dat |>
      mutate(
        FC = rpois(
          n = nrow(sim_dat),
          lambda = FC_l
        )
      )
    
    # confirm sim rate for theta = 0 ~ 0.01
    # mean(sim_dat$FC)

    # remove errors, output data in admin format
    sims_out[[j]] <- sim_dat |>
      select(Child:GROUP, Wave, FC) |>
      rename(COUNTY = County) |>
      mutate(theta = t, sim = i) |> 
      mutate(Wave = factor(Wave))
  }
}

## evaluate MC sims for treatment periods
sims_out |> 
  bind_rows() |> 
  group_by(GROUP, Wave, theta) |> 
  summarize(FC_mn = mean(FC)) |> 
  filter(Wave == 2 | Wave == 3)

# model -------------------------------------------------------------------
# estimate models as specified in manuscript, pre-reg
# weakly informative cauchy priors on betas, theta
# weakly informative t priors on variance terms

### settings for brms
cores <- parallel::detectCores()
model_controls <- list(
  adapt_delta = 0.999,
  stepsize = 0.01,
  max_treedepth = 15
)

iter <- 1e3

# priors
priorsL <- priorsL_FC <- c(
  set_prior("cauchy(0, 10)", class = "b"),
  set_prior("cauchy(0, 10)", class = "Intercept"),
  set_prior("student_t(3, 0, 1)", class = "sd")
)

# fit models over each MC sim
fc_post_out <- list()
for (i in 1:length(sims_out)) {
  print(i)

    m_fc <- brm(
    FC > 0 ~ Wave + GROUP:Wave + COUNTY + (1 | Child),
    family = bernoulli(),
    data = sims_out[[i]],
    control = model_controls,
    cores = 8,
  )

  # take posterior draws for betas and subset to SATE w2-w4
  temp <- posterior_samples(m_fc, pars = "^b")
  temp <- temp |>
    select(`b_Wave2:GROUPT`:`b_Wave4:GROUPT`)
  # attach theta and sim to output
  temp <- temp |>
    mutate(theta = sims_out[[i]]$theta[1], sim = sims_out[[i]]$sim[1])

  temp <- temp |>
    pivot_longer(
      cols = `b_Wave2:GROUPT`:`b_Wave4:GROUPT`,
      names_to = "var",
      values_to = "value"
    )

  temp <- temp |>
    group_by(theta, sim, var) |>
    summarize(
      post_e = exp(mean(value)),
      post_lwr = exp(quantile(value, 0.025)),
      post_upr = exp(quantile(value, 0.975))
    )
  
  filename <- paste("./output/fc_sim", i, ".csv", sep = "")
  
  write_csv(temp, filename)

  fc_post_out[[i]] <- temp
  
}

#output

fc_post_out |>
  bind_rows() |>
  write_csv("./data/power_fc_mc_sim.csv")

# visualize
# 
# post_df <- fc_post_out |>
#   bind_rows() |>
#   mutate(
#     var = case_when(
#       var == "b_Wave2:GROUPT" ~ "Treatment period 1",
#       var == "b_Wave3:GROUPT" ~ "Treatment period 2",
#       var == "b_Wave4:GROUPT" ~ "Post-treatment"
#     ),
#     theta_fct = case_when(
#       theta == 0 ~ "Theta = 0 (Null)"
#       theta == -0.1 ~ "Theta = -0.1",
#       theta == -0.5 ~ "Theta = -0.5",
#       theta == -1 ~ "Theta = -1",
#       theta == -5 ~ "Theta = -5"
#     )
#   ) |>
#   mutate(
#     var = factor(
#       var,
#       levels = c("Treatment period 1", "Treatment period 2", "Post-treatment")
#     ),
#     theta_fct = factor(
#       theta_fct,
#       levels = c("Theta = 0 (Null)", "Theta = -0.1", "Theta = -0.5", "Theta = -1", "Theta = -5")
#     )
#   )

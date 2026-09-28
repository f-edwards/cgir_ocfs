# power simulation under varying effect sizes, fixed sample
# monte carlo simulation
# fixing event incidence at empirical, with hh, child and county effects
# post-hoc
# revision for amarel run, public release

# simulate data -----------------------------------------------------------
library(tidyverse)
library(tidybayes)
library(rstan)
library(rstanarm)
set.seed(1)
# read anonymized hh data
rstan_options(auto_write = TRUE)


# parameters --------------------------------------------------------------

theta_scen <- 0
b0 <- -8
n_sims <- 100

# allocation ------------------------------------------------------------------
iter <- 0
for (theta in theta_scen) {
  for (sim in 1:n_sims) {
    iter <- iter + 1
    set.seed(iter)
    child <- 1:1000
    hh <- 1:450
    sim_dat <- data.frame(child = child, hh = NA)
    # assign hh, at least 1 kid per
    sim_dat$hh[1:450] <- hh
    sim_dat$hh[451:1000] <- sample(hh, size = length(451:1000), replace = T)
    hist(table(sim_dat$hh))
    # this looks fine
    # then allocate to T / C and Wave
    trt <- sample(hh, 150, replace = F)
    sim_dat$Group <- ifelse(sim_dat$hh %in% trt, "T", "C")
    # table(sim_dat$Group)
    # sim_dat |> group_by(Group) |> summarize(n_hh = n_distinct(hh))

    # create clustering structure
    # counties, 50 hh trt each, 100 hh ctrl
    cnty_hh_trt <- sim_dat |>
      select(Group, hh) |>
      distinct() |>
      filter(Group == "T") |>
      mutate(cnty = rep(1:3, each = 50))

    cnty_hh_ctrl <- sim_dat |>
      select(Group, hh) |>
      distinct() |>
      filter(Group == "C") |>
      mutate(cnty = rep(1:3, each = 100))

    cnty_hh <- bind_rows(cnty_hh_trt, cnty_hh_ctrl)
    # attach
    sim_dat <- sim_dat |>
      left_join(cnty_hh)

    ### assume normal error for county, lognormal for hh, normal for child
    cnty_error <- data.frame(cnty = 1:3, cnty_error = rnorm(3, 0, 1))
    hh_error <- data.frame(hh = 1:450, hh_error = rlnorm(450, 0, 1))
    chld_error <- data.frame(child = 1:1000, child_error = rnorm(1000, 0, 1))

    ## attach
    sim_dat <- sim_dat |>
      left_join(cnty_error) |>
      left_join(hh_error) |>
      left_join(chld_error)

    # now stack waves
    sim_dat <- sim_dat |>
      mutate(Wave = 1) |>
      bind_rows(
        sim_dat |>
          mutate(Wave = 2)
      ) |>
      bind_rows(
        sim_dat |>
          mutate(Wave = 3)
      ) |>
      bind_rows(
        sim_dat |>
          mutate(Wave = 4)
      )

    # turn on SATE for treatment periods in group=T
    sim_dat <- sim_dat |>
      mutate(trt = ifelse((Group == "T") & ((Wave == 2) | Wave == 3), T, F))

    ### convert error to probability with inverse logit

    inv.logit <- function(x) {
      return(exp(x) / (1 + exp(x)))
    }
    # linear predictor b0 + alpha_child + gamma_hh + delta_cnty + theta * T
    sim_dat <- sim_dat |>
      mutate(
        p_chld = inv.logit(
          b0 + cnty_error + hh_error + child_error + trt * theta
        ),
        p_hh = inv.logit(b0 + cnty_error + hh_error + trt * theta)
      )

    # confirm we get long tails with non-zero mass.
    # p doesn't need to be empirically calibrated; it is a sampling weight
    # hist(sim_dat$p_hh)
    # hist(sim_dat$p_chld)

    # allocate ----------------------------------------------------------------
    sim_dat$row_n <- 1:nrow(sim_dat)
    # select 20 random households, no duplicates in waves
    sim_random_hh <- sim_dat |>
      select(Wave, hh, p_hh) |>
      distinct() |>
      slice_sample(n = 20, weight_by = p_hh)

    # allocate 1 entry per hh
    entries_1 <- sim_random_hh |>
      left_join(sim_dat) |>
      group_by(hh, Wave) |>
      slice_sample(n = 1, weight_by = p_hh, replace = F)

    # then allocate remaining 15
    entries_2 <- sim_random_hh |>
      left_join(sim_dat) |>
      anti_join(entries_1) |>
      slice_sample(n = 15, weight_by = p_chld, replace = F)

    entries <- bind_rows(entries_1, entries_2)
    table(entries$hh)

    entries <- entries |>
      select(Wave, hh, child, Group, cnty) |>
      mutate(FC = 1)
    # attach to sim_dat
    sim_dat <- sim_dat |>
      left_join(entries) |>
      mutate(FC = ifelse(is.na(FC), 0, FC)) |>
      mutate(Wave = factor(Wave), cnty = factor(cnty)) |>
      mutate(theta = theta)

    ## evaluate MC sims for treatment periods
    sim_dat |>
      bind_rows() |>
      group_by(Group, Wave, theta) |>
      summarize(FC_mn = mean(FC), p_chld = mean(p_chld)) |>
      filter(Wave == 2 | Wave == 3)

    # model -------------------------------------------------------------------
    # weakly informative cauchy priors on betas, theta
    print(iter)
    m_fc <- stan_glmer(
      FC > 0 ~ Wave + Group:Wave + cnty + (1 | hh / child),
      family = "binomial",
      prior = cauchy(0, 2.5),
      prior_intercept = cauchy(0, 10),
      data = sim_dat,
      cores = 4,
      adapt_delta = 0.999,
      QR = T
    )
    # extract parameters, pivot,
    post_beta <- data.frame(m_fc) |>
      select("Wave1.GroupT":"Wave4.GroupT") |>
      pivot_longer(cols = everything())

    post_beta <- post_beta |>
      group_by(name) |>
      summarize(
        post_e = exp(mean(value)),
        post_lwr = exp(quantile(value, 0.025)),
        post_upr = exp(quantile(value, 0.975))
      ) |>
      mutate(theta = theta, sim = sim, seed = iter)

    filename <- paste("./power_output/fc_sim", iter, ".csv", sep = "")
    write_csv(post_beta, filename)
  }
}

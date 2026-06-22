library(tidyverse)
library(bayestestR)
library(tidybayes)
library(patchwork)

theme_set(theme_bw())
# table 1 inputs ----------------------------------------------------------
group <- read_csv("./data/group_assignment.csv")

datw1 <- read_csv("./data/w1_reduced.csv") |>
  left_join(group) |>
  rename(GROUP = Group) |>
  mutate(GROUP = ifelse(is.na(GROUP), "C", GROUP))

datw1 |>
  group_by(GROUP) |>
  summarize(n = n()) |>
  mutate(prop = n / sum(n))

datw1 |>
  group_by(GROUP, County) |>
  summarize(n = n()) |>
  mutate(prop = n / sum(n))


datw1 |>
  group_by(GROUP) |>
  summarize(n_child = sum(n_child))

datw1 |>
  summarize(mn = mean(n_child), sd = sd(n_child))

datw1 |>
  group_by(GROUP) |>
  summarize(Age_c = mean(Age_c))

datw1 |>
  group_by(GROUP) |>
  summarize(
    GenderF = mean(Gender_s == "Female", na.rm = T)
  )

datw1 |>
  group_by(GROUP) |>
  summarize(across(Race_White:Race_Other, mean, na.rm = T))

datw1 |>
  group_by(GROUP) |>
  summarize(across(Race_White:Race_Other, sum, na.rm = T))


datw1 |>
  group_by(GROUP, County) |>
  count()

t <- datw1 |>
  group_by(GROUP, Edu) |>
  count() |>
  group_by(GROUP) |>
  mutate(prop = n / sum(n))


# figure 2 ----------------------------------------------------------------
c <- read_csv("./output/child_period_bin.csv")
### callouts for abstract
c_abs <- c |>
  mutate(
    Confirmed_pct = Confirmed_bin / n_child * 100,
    TotalCPS_pct = TotalCPS_bin / n_child * 100,
    Prev_pct = Prev_bin / n_child * 100,
    FC_pct = FC_bin / n_child * 100
  ) |>
  select(GROUP:Prev_bin, Confirmed_pct:FC_pct)

pdat_c <- c |>
  pivot_longer(
    cols = TotalCPS_bin:Prev_bin,
    names_to = "var",
    values_to = "y"
  ) |>
  mutate(rate = y / n_child) |>
  mutate(GROUP = ifelse(GROUP == "C", "Control", "Treatment"))

pdat_c <- pdat_c |>
  mutate(
    var = case_when(
      var == "Confirmed_bin" ~ "Confirmed\nmaltreatment",
      var == "FC_bin" ~ "Foster care",
      var == "TotalCPS_bin" ~ "Maltreatment\nreport",
      var == "Prev_bin" ~ "Preventive\nservices"
    )
  )

## create intervals
pdat_c <- pdat_c |>
  filter(.imp > 0) |>
  group_by(GROUP, Wave, var) |>
  summarize(rate_mn = mean(rate), rate_min = min(rate), rate_max = max(rate))

pdat_c <- pdat_c |>
  mutate(
    var = factor(
      var,
      levels = c(
        "Maltreatment\nreport",
        "Confirmed\nmaltreatment",
        "Preventive\nservices",
        "Foster care"
      )
    )
  )

trt_block <- data.frame(
  var = factor(
    "Maltreatment\nreport",
    levels = c(
      "Maltreatment\nreport",
      "Confirmed\nmaltreatment",
      "Preventive\nservices",
      "Foster care"
    )
  ),
  label = c("Treatment period"),
  x = c(2),
  y = c(6.5),
  GROUP = "Treatment"
)


ggplot(
  pdat_c,
  aes(
    x = Wave,
    y = rate_mn * 100,
    color = GROUP
  )
) +
  geom_point() +
  geom_line() +
  geom_ribbon(
    aes(ymin = rate_min * 100, ymax = rate_max * 100, fill = GROUP),
    alpha = 0.5,
    color = NULL
  ) +
  annotate(
    'rect',
    xmin = 1.05,
    xmax = 3,
    ymin = -Inf,
    ymax = Inf,
    fill = "gray",
    alpha = 0.3
  ) +
  geom_text(
    data = trt_block,
    aes(x = x, y = y, label = label),
    color = "gray60"
  ) +
  labs(
    x = "Period",
    y = "Percent of children",
    color = "Group",
    fill = "Group"
  ) +
  theme_bw() +
  expand_limits(y = 0) +
  scale_x_continuous(
    breaks = c(-1, 1, 2, 3, 4),
    labels = c(
      "6 months before baseline",
      "Baseline",
      "6 months",
      "12 months",
      "18 months"
    )
  ) +
  facet_wrap(~var, scales = "free", nrow = 4, strip.position = "top")

ggsave("./vis/fig2.png", width = 8, height = 6, units = "in", dpi = 300)

# posterior inference ---------------------------------------------------------
# pull parameter estimates from .csv files directly
# compute p direction and BH adjusted tests

models <- data.frame(
  path = c(
    "./output/TotalCPS_b.csv",
    "./output/Confirmed_b.csv",
    "./output/Prev_b.csv",
    "./output/FC_b.csv"
  ),
  variable = c(
    "Maltreatment report",
    "Confirmed maltreatment",
    "Preventive services",
    "Foster care"
  )
)

out_joint <- list()
for (i in 1:nrow(models)) {
  mod <- read_csv(models$path[i]) |>
    select(-.chain, -.iteration, -.draw)
  out <- ci(mod, method = "ETI") |>
    as_tibble()
  pe <- point_estimate(mod, centrality = "median")
  pd <- p_direction(mod)
  pd <- pd |>
    mutate(p = 2 * (1 - pd))
  out$Median <- pe$Median
  out$pd <- pd$pd
  out$p <- pd$p
  out$variable <- models$variable[i]

  out_joint[[i]] <- out
}
out_joint <- bind_rows(out_joint)
out_joint$OR <- exp(out_joint$Median)
out_joint$OR_low <- exp(out_joint$CI_low)
out_joint$OR_high <- exp(out_joint$CI_high)
## BH adjustment
### following Makowski et al 2019;
# https://doi.org/10.3389/fpsyg.2019.02767
# pd ~ p ; ptwo−sided = 2×(1−pd))
# implemented in bayestestR
# bejnamini hochberg
# adjust for FDR, FWER (https://rss.onlinelibrary.wiley.com/doi/epdf/10.1111/j.2517-6161.1995.tb02031.x)
# https://journals.sagepub.com/doi/epdf/10.3102/10769986024001042
# https://journals.sagepub.com/doi/epdf/10.3102/10769986027001077
# https://www.stat.cmu.edu/~genovese/talks/hannover1-04.pdf
out_joint_b <- out_joint |>
  mutate(q = p.adjust(p, method = "BH")) |>
  filter(
    Parameter %in% c("b_Wave2:GROUPT", "b_Wave3:GROUPT", "b_Wave4:GROUPT")
  ) |>
  mutate(
    across(CI:p, ~ round(.x, 2)),
    across(OR:OR_high, ~ round(.x, 2))
  )

write_csv(out_joint_b, file = "./vis/reg_table_main.csv")

# complete case (s1) -----------------------------------------------------------

models <- data.frame(
  path = c(
    "./output/TotalCPS_b_s1.csv",
    "./output/Confirmed_b_s1.csv",
    "./output/Prev_b_s1.csv",
    "./output/FC_b_s1.csv"
  ),
  variable = c(
    "Maltreatment report",
    "Confirmed maltreatment",
    "Preventive services",
    "Foster care"
  )
)

out_joint <- list()
for (i in 1:nrow(models)) {
  mod <- read_csv(models$path[i]) |>
    select(-.chain, -.iteration, -.draw)
  out <- ci(mod, method = "ETI") |>
    as_tibble()
  pe <- point_estimate(mod, centrality = "median")
  pd <- p_direction(mod)
  pd <- pd |>
    mutate(p = 2 * (1 - pd))
  out$Median <- pe$Median
  out$pd <- pd$pd
  out$p <- pd$p
  out$variable <- models$variable[i]
  out_joint[[i]] <- out
}
out_joint <- bind_rows(out_joint)
out_joint$OR <- exp(out_joint$Median)
out_joint$OR_low <- exp(out_joint$CI_low)
out_joint$OR_high <- exp(out_joint$CI_high)
## BH adjustment
### following Makowski et al 2019;
# https://doi.org/10.3389/fpsyg.2019.02767
# pd ~ p ; ptwo−sided = 2×(1−pd))
# implemented in bayestestR
# bejnamini hochberg
# adjust for FDR, FWER (https://rss.onlinelibrary.wiley.com/doi/epdf/10.1111/j.2517-6161.1995.tb02031.x)
# https://journals.sagepub.com/doi/epdf/10.3102/10769986024001042
# https://journals.sagepub.com/doi/epdf/10.3102/10769986027001077
# https://www.stat.cmu.edu/~genovese/talks/hannover1-04.pdf
out_joint_b_s1 <- out_joint |>
  mutate(q = p.adjust(p, method = "BH")) |>
  filter(
    Parameter %in% c("b_Wave2:GROUPT", "b_Wave3:GROUPT", "b_Wave4:GROUPT")
  ) |>
  mutate(
    across(CI:p, ~ round(.x, 2)),
    across(OR:OR_high, ~ round(.x, 2))
  )

write_csv(out_joint_b_s1, file = "./vis/reg_table_complete_case.csv")
# controls (s2) ----------------------------------------------------------------

models <- data.frame(
  path = c(
    "./output/TotalCPS_b_s2.csv",
    "./output/Confirmed_b_s2.csv",
    "./output/Prev_b_s2.csv",
    "./output/FC_b_s2.csv"
  ),
  variable = c(
    "Maltreatment report",
    "Confirmed maltreatment",
    "Preventive services",
    "Foster care"
  )
)

out_joint <- list()
for (i in 1:nrow(models)) {
  mod <- read_csv(models$path[i]) |>
    select(-.chain, -.iteration, -.draw)
  out <- ci(mod, method = "ETI") |>
    as_tibble()
  pe <- point_estimate(mod, centrality = "median")
  pd <- p_direction(mod)
  pd <- pd |>
    mutate(p = 2 * (1 - pd))
  out$Median <- pe$Median
  out$pd <- pd$pd
  out$p <- pd$p
  out$variable <- models$variable[i]
  out_joint[[i]] <- out
}
out_joint <- bind_rows(out_joint)
out_joint$OR <- exp(out_joint$Median)
out_joint$OR_low <- exp(out_joint$CI_low)
out_joint$OR_high <- exp(out_joint$CI_high)

out_joint_b_s2 <- out_joint |>
  mutate(q = p.adjust(p, method = "BH")) |>
  filter(
    Parameter %in% c("b_Wave2:GROUPT", "b_Wave3:GROUPT", "b_Wave4:GROUPT")
  ) |>
  mutate(
    across(CI:p, ~ round(.x, 2)),
    across(OR:OR_high, ~ round(.x, 2))
  )

write_csv(out_joint_b_s2, file = "./vis/reg_table_controls.csv")
# combined treatment period (s3) ---------------------------------------------

models <- data.frame(
  path = c(
    "./output/TotalCPS_b_s3.csv",
    "./output/Confirmed_b_s3.csv",
    "./output/Prev_b_s3.csv",
    "./output/FC_b_s3.csv"
  ),
  variable = c(
    "Maltreatment report",
    "Confirmed maltreatment",
    "Preventive services",
    "Foster care"
  )
)

out_joint <- list()
for (i in 1:nrow(models)) {
  mod <- read_csv(models$path[i]) |>
    select(-.chain, -.iteration, -.draw)
  out <- ci(mod, method = "ETI") |>
    as_tibble()
  pe <- point_estimate(mod, centrality = "median")
  pd <- p_direction(mod)
  pd <- pd |>
    mutate(p = 2 * (1 - pd))
  out$Median <- pe$Median
  out$pd <- pd$pd
  out$p <- pd$p
  out$variable <- models$variable[i]
  out_joint[[i]] <- out
}
out_joint <- bind_rows(out_joint)
out_joint$OR <- exp(out_joint$Median)
out_joint$OR_low <- exp(out_joint$CI_low)
out_joint$OR_high <- exp(out_joint$CI_high)

out_joint_b_s3 <- out_joint |>
  mutate(q = p.adjust(p, method = "BH")) |>
  filter(
    Parameter %in%
      c("b_Wave_collapsedTreatment:GROUPT", "b_Wave_collapsedPost:GROUPT ")
  ) |>
  mutate(
    across(CI:p, ~ round(.x, 2)),
    across(OR:OR_high, ~ round(.x, 2))
  )

write_csv(out_joint_b_s3, file = "./vis/reg_table_collapsed_periods.csv")

# LPM ---------------------------------------------------------------------

models <- data.frame(
  path = c(
    "./output/TotalCPS_lpm.csv",
    "./output/Confirmed_lpm.csv",
    "./output/Prev_lpm.csv",
    "./output/FC_lpm.csv"
  ),
  variable = c(
    "Maltreatment report",
    "Confirmed maltreatment",
    "Preventive services",
    "Foster care"
  )
)

out_joint <- list()
for (i in 1:nrow(models)) {
  out <- read_csv(models$path[i])
  out$variable <- models$variable[i]
  out$CI_low <- out$estimate - 1.96 * out$std.error
  out$CI_high <- out$estimate + 1.96 * out$std.error
  out_joint[[i]] <- out
}

out_joint <- bind_rows(out_joint)

out_joint_lpm <- out_joint |>
  mutate(q = p.adjust(p.value, method = "BH")) |>
  filter(
    term %in% c("Wave2:GROUPT", "Wave3:GROUPT", "Wave4:GROUPT")
  )

out_joint <- bind_rows(out_joint)

write_csv(out_joint_lpm, file = "./vis/reg_table_lpm.csv")


# posterior visuals -------------------------------------------------------

models <- data.frame(
  path = c(
    "./output/TotalCPS_b.csv",
    "./output/Confirmed_b.csv",
    "./output/Prev_b.csv",
    "./output/FC_b.csv",
    "./output/TotalCPS_b_s1.csv",
    "./output/Confirmed_b_s1.csv",
    "./output/Prev_b_s1.csv",
    "./output/FC_b_s1.csv",
    "./output/TotalCPS_b_s2.csv",
    "./output/Confirmed_b_s2.csv",
    "./output/Prev_b_s2.csv",
    "./output/FC_b_s2.csv"
  ),
  variable = rep(
    c(
      "Maltreatment report",
      "Confirmed maltreatment",
      "Preventive services",
      "Foster care"
    )
  ),
  type = c(
    rep("1. Primary", 4),
    rep("2. Complete case", 4),
    rep("3. Demographic controls", 4)
  )
)

p_dat <- map(models$path, read_csv)
p_out <- list()
for (i in 1:length(p_dat)) {
  temp <- p_dat[[i]] |>
    select(`b_Wave2:GROUPT`:`b_Wave4:GROUPT`) |>
    pivot_longer(
      cols = everything(),
      names_to = "var",
      values_to = "estimate"
    ) |>
    mutate(
      var = case_when(
        var == "b_Wave2:GROUPT" ~ "1. Treatment period 1",
        var == "b_Wave3:GROUPT" ~ "2. Treatment period 2",
        var == "b_Wave4:GROUPT" ~ "3. Post-treatment"
      )
    ) |>
    mutate(type = models$type[i], variable = models$variable[i])
  p_out[[i]] <- temp
}

p_out <- bind_rows(p_out)


p1 <- p_out |>
  filter(variable == "Maltreatment report") |>
  ggplot(aes(x = estimate)) +
  stat_halfeye(
    .width = c(0, 0, 0.95),
    fatten_point = 0.5
  ) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(y = "", x = "Odds ratio", subtitle = "Maltreatment report") +
  facet_grid(type ~ var, scales = "free_y")
p2 <- p_out |>
  filter(variable == "Confirmed maltreatment") |>
  ggplot(aes(x = estimate)) +
  stat_halfeye(
    .width = c(0, 0, 0.95),
    fatten_point = 0.5
  ) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(y = "", x = "Odds ratio", subtitle = "Confirmed maltreatment") +
  facet_grid(type ~ var)
p3 <- p_out |>
  filter(variable == "Preventive services") |>
  ggplot(aes(x = estimate)) +
  stat_halfeye(
    .width = c(0, 0, 0.95),
    fatten_point = 0.5
  ) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(y = "", x = "Beta", subtitle = "3. Preventive services") +
  facet_grid(type ~ var, scales = "free")

p4 <- p_out |>
  filter(variable == "Foster care") |>
  ggplot(aes(x = estimate)) +
  stat_halfeye(
    .width = c(0, 0, 0.95),
    fatten_point = 0.5
  ) +
  geom_vline(xintercept = 0, lty = 2) +
  coord_cartesian(xlim = c(-35, 2)) +
  labs(y = "", x = "Beta", subtitle = "4. Foster care") +
  facet_grid(type ~ var, scales = "free")


p1 + p2 + plot_layout(axes = "collect_y")
ggsave("./vis/posteriors_1.pdf", width = 12, height = 7)
p3 + p4 + plot_layout(axes = "collect_y")
ggsave("./vis/posteriors_2.pdf", width = 12, height = 7)
# SPLIT INTO TWO PLOTS

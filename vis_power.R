# visualize using output from sim_ocfs type 1 and 2
library(tidyverse)
library(tidybayes)
options(digits = 7)
options(pillar.sigfig = 7)


# visualize type 1 ---------------------------------------------------------------
# weighted sampling, x = 35
files <- list.files(path = "./power_output", full.names = T)
post_df <- files |>
  map(read.csv) |>
  bind_rows() |>
  mutate(sig = post_upr < 1)

# compute proportion with two false positives in trt
temp <- post_df |>
  filter(name == "Wave2.GroupT" | name == "Wave3.GroupT") |>
  group_by(sim) |>
  summarize(sig_1 = sum(sig) >= 1, sig_2 = sum(sig) == 2) |>
  ungroup() |>
  summarize(sig_1_rt = mean(sig_1), sig_2_rt = mean(sig_2)) |>
  mutate(sig_marginal = mean(post_df$sig))

# plot type 1 errors
post_df |>
  mutate(prop_sig = round(mean(sig), 2), row_n = 1:n()) |>
  ggplot(aes(x = row_n, ymin = post_lwr, ymax = post_upr, color = sig)) +
  geom_linerange(alpha = 0.5) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_text(
    aes(
      label = paste("P(reject null", "=", prop_sig, ")", sep = ""),
      x = 375,
      y = 6
    ),
    color = "black"
  ) +
  coord_cartesian(ylim = c(0, 6)) +
  scale_y_sqrt(breaks = c(0, 0.25, 1, 2.5, 5, 10, 20)) +
  labs(
    x = "Iteration",
    y = "Odds Ratio",
    color = expression(paste("Reject ", H[0], sep = "")),
    caption = "Note: square root scale on y-axis"
  ) +
  theme_tidybayes()

ggsave("vis/power_type1.png", width = 12)

# visualize power ---------------------------------------------------------------
files <- list.files(path = "./power_output2", full.names = T)
post_df <- files |>
  map(read.csv) |>
  bind_rows() |>
  filter((name == "Wave2.GroupT") | (name == "Wave3.GroupT")) |>
  mutate(
    theta = factor(
      theta,
      levels = c(0, -0.5, -1, -2, -5),
      labels = c(
        "1. theta = 0",
        "2. theta = -0.5",
        "2. theta = -1",
        "3. theta = -2",
        "4. theta = -5"
      )
    )
  ) |>
  mutate(sig = post_upr < 1)

temp <- post_df |>
  group_by(theta, sim) |>
  summarize(sig_1 = sum(sig) >= 1, sig_2 = sum(sig) == 2) |>
  group_by(theta) |>
  summarize(sig_1_rt = mean(sig_1), sig_2_rt = mean(sig_2)) |>
  left_join(
    post_df |>
      group_by(theta) |>
      summarize(sig_marginal = mean(sig))
  )


# compute proportion significant in one or two treatment periods

# power
post_df |>
  group_by(name, theta) |>
  left_join(temp) |>
  mutate(
    name = case_when(
      name == "Wave2.GroupT" ~ "Treatment period 1",
      name == "Wave3.GroupT" ~ "Treatment period 2"
    )
  ) |>
  ggplot(aes(x = sim, ymin = post_lwr, ymax = post_upr, color = sig)) +
  geom_linerange(alpha = 0.5) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_text(
    aes(
      label = paste("P(reject null", "=", sig_marginal, ")", sep = ""),
      x = 65,
      y = 6
    ),
    color = "black"
  ) +
  coord_cartesian(ylim = c(0, 6)) +
  scale_y_sqrt(breaks = c(0, 0.25, 1, 2.5, 5, 10, 20)) +
  labs(
    x = "Simulation seed",
    y = "Odds Ratio",
    color = expression(paste("Reject ", H[0], sep = "")),
    caption = "Note: square root scale on y-axis"
  ) +
  facet_grid(~theta) +
  theme_tidybayes()

ggsave("vis/power_type2.png", width = 12)

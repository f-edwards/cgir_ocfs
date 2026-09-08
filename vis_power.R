library(tidyverse)
library(tidybayes)

files <- list.files("./output", full.names = T)

input <- list()
for (i in files) {
  temp <- read.csv(i)
  input[[i]] <- read.csv(i)
}
# format df
post_df <- input |>
  bind_rows() |>
  mutate(
    var = case_when(
      var == "b_Wave2:GROUPT" ~ "Treatment period 1",
      var == "b_Wave3:GROUPT" ~ "Treatment period 2",
      var == "b_Wave4:GROUPT" ~ "Post-treatment"
    ),
    theta_fct = case_when(
      theta == 0 ~ "Theta = 0 (Null)",
      theta == -0.1 ~ "Theta = -0.1",
      theta == -0.5 ~ "Theta = -0.5",
      theta == -1 ~ "Theta = -1",
      theta == -1.5 ~ "Theta = -1.5",
      theta == -2 ~ "Theta = -2"
    )
  ) |>
  mutate(
    var = factor(
      var,
      levels = c("Treatment period 1", "Treatment period 2", "Post-treatment")
    ),
    theta_fct = factor(
      theta_fct,
      levels = c(
        "Theta = 0 (Null)",
        "Theta = -0.1",
        "Theta = -0.5",
        "Theta = -1",
        "Theta = -1.5",
        "Theta = -2"
      )
    )
  )

# type 1 error rate

post_df <- post_df |>
  mutate(
    sig = post_upr < 1,
    type1 = post_upr < 1,
    type2 = post_upr > 1
  )

post_df |>
  group_by(theta_fct, var) |>
  summarize(prop_sig = mean(sig), prop_t1 = mean(type1), prop_t2 = mean(type2))

# plot type 1 errors
post_df |>
  filter(theta_fct == "Theta = 0 (Null)") |>
  group_by(var) |>
  mutate(prop_sig = round(mean(sig), 2)) |>
  ggplot(aes(x = sim, ymin = post_lwr, ymax = post_upr, color = sig)) +
  geom_linerange(alpha = 0.5) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_text(
    aes(label = prop_sig, x = 90, y = 8),
    color = "black"
  ) +
  facet_grid(var ~ theta_fct) +
  coord_cartesian(ylim = c(0, 9), xlim = c(0, 100)) +
  scale_y_sqrt(breaks = c(0, 0.25, 1, 2.5, 5)) +
  labs(
    x = "",
    y = "Odds Ratio",
    color = expression(paste("Reject ", H[0], sep = "")),
    title = "Monte Carlo simulation of foster care entries under no treatment effect scenario",
    subtitle = "95 percent posterior intervals",
    caption = "Note square root scale on y-axis"
  ) +
  theme_tidybayes()

ggsave("./vis/power_sim_type1.png", width = 12, height = 8)

# plot type 2 errors
post_df |>
  filter(theta_fct != "Theta = 0 (Null)") |>
  group_by(var, theta_fct) |>
  mutate(
    prop_sig = round(mean(sig), 2)
  ) |>
  ggplot(aes(x = sim, ymin = post_lwr, ymax = post_upr, color = sig)) +
  geom_linerange(alpha = 0.5) +
  geom_hline(yintercept = 1, lty = 2) +
  facet_grid(var ~ theta_fct) +
  coord_cartesian(ylim = c(0, 9), xlim = c(0, 100)) +
  scale_y_sqrt(breaks = c(0, 0.25, 1, 2.5, 5)) +
  geom_text(
    aes(label = prop_sig, x = 90, y = 8),
    color = "black"
  ) +
  labs(
    x = "",
    y = "Odds Ratio",
    color = expression(paste("Reject ", H[0], sep = "")),
    title = "Monte Carlo simulation of foster care entries under varying treatment effect scenarios",
    subtitle = "95 percent posterior intervals",
    caption = "Note square root scale on y-axis"
  ) +
  theme_tidybayes()

### maybe just change to reject H_0

ggsave("./vis/power_sim_type2.png", width = 12, height = 8)

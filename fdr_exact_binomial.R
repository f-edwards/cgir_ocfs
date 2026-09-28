# parameters
y <- 35
y_t <- 6
y_c <- 29
n_t <- 343
n_c <- 643
r_t <- y_t / (n_t * 4)
r_c <- y_c / (n_c * 4)

r_grand <- (y_t + y_c) / ((n_t + n_c) * 4)
# estimate likelihood of event counts under
# p = r_grand
# for treatment and control sample sizes
y_grid <- data.frame(fc = 0:15) |>
  mutate(p_t = dbinom(fc, n_t, r_grand), p_c = dbinom(fc, n_c, r_grand))

p1_1 <- ggplot(y_grid, aes(x = fc, y = p_t)) +
  geom_col() +
  labs(
    x = "",
    y = "Probability",
    subtitle = "Treatment group, n = 343",
    title = "1. Single treatment period"
  )
p2_1 <- ggplot(y_grid, aes(x = fc, y = p_c)) +
  geom_col() +
  labs(
    x = "Foster care entries",
    y = "Probability",
    subtitle = "Control group, n = 643"
  )


# for two treatment periods
y_grid <- data.frame(fc = 0:25) |>
  mutate(p_t = dbinom(fc, n_t * 2, r_grand), p_c = dbinom(fc, n_c * 2, r_grand))

p1_2 <- ggplot(y_grid, aes(x = fc, y = p_t)) +
  geom_col() +
  labs(
    x = "",
    y = "Probability",
    subtitle = "Treatment group, n = 343",
    title = "2. Two treatment periods"
  )
p2_2 <- ggplot(y_grid, aes(x = fc, y = p_c)) +
  geom_col() +
  labs(
    x = "Foster care entries",
    y = "Probability",
    subtitle = "Control group, n = 643"
  )

p3 <- gridExtra::grid.arrange(p1_1, p1_2, p2_1, p2_2)
ggsave(
  "./vis/pmf_binomial_fc_grand_two.png",
  width = 12,
  height = 6,
  plot = p3
)

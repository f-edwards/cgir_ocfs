# descriptive tables ------------------------------------------------------
# frequencies
# child-level
c_long_imp |>
  group_by(GROUP, Wave, .imp) |>
  filter(!(is.na(TotalCPS))) |>
  summarize(
    n_child = n(),
    TotalCPS = sum(TotalCPS, na.rm = T),
    Confirmed = sum(Confirmed, na.rm = T),
    FC = sum(FC, na.rm = T),
    Prev = sum(Prev, na.rm = T)
  ) |>
  write_csv(file = "./output/child_period_freq.csv")

#household-level
c_long_imp |>
  group_by(GROUP, Wave, .imp, ID) |>
  filter(!(is.na(TotalCPS))) |>
  summarize(
    TotalCPS = sum(TotalCPS, na.rm = T),
    Confirmed = sum(Confirmed, na.rm = T),
    FC = sum(FC, na.rm = T),
    Prev = sum(Prev, na.rm = T)
  ) |>
  group_by(GROUP, Wave, .imp) |>
  summarize(
    n_hh = n(),
    TotalCPS = sum(TotalCPS),
    Confirmed = sum(Confirmed),
    FC = sum(FC),
    Prev = sum(Prev)
  ) |>
  write_csv(file = "./output/hh_period_freq.csv")

# event binaries
# child-level
c_long_imp |>
  filter(!(is.na(TotalCPS))) |>
  group_by(GROUP, Wave, .imp, Child) |>
  summarize(
    TotalCPS = sum(TotalCPS, na.rm = T) > 0,
    Confirmed = sum(Confirmed, na.rm = T) > 0,
    FC = sum(FC, na.rm = T) > 0,
    Prev = sum(Prev, na.rm = T) > 0
  ) |>
  group_by(GROUP, Wave, .imp) |>
  summarize(
    n_child = n(),
    TotalCPS_bin = sum(TotalCPS),
    Confirmed_bin = sum(Confirmed),
    FC_bin = sum(FC),
    Prev_bin = sum(Prev)
  ) |>
  write_csv(file = "./output/child_period_bin.csv")

#household-level
c_long_imp |>
  group_by(GROUP, Wave, .imp, ID) |>
  summarize(
    TotalCPS = sum(TotalCPS, na.rm = T) > 0,
    Confirmed = sum(Confirmed, na.rm = T) > 0,
    FC = sum(FC, na.rm = T) > 0,
    Prev = sum(Prev, na.rm = T) > 0
  ) |>
  group_by(GROUP, Wave, .imp) |>
  summarize(
    n_hh = n(),
    TotalCPS_bin = sum(TotalCPS),
    Confirmed_bin = sum(Confirmed),
    FC_bin = sum(FC),
    Prev_bin = sum(Prev)
  ) |>
  write_csv(file = "./output/hh_period_bin.csv")

# prognostic associations with survey features ----------------------------
# n_child, gender, race, edu, sexorient, kessler, ah total, fw score
pdat <- c_long_imp |>
  filter(.imp == 0, Wave == 1) |>
  filter(!(is.na(TotalCPS)))

# totalCPS ----------------------------------------------------------------
p1 <- ggplot(pdat, aes(x = TotalCPS > 0, y = n_child)) +
  geom_boxplot() +
  labs(x = "CPS Report at baseline", y = "Number of children in household")

p2 <- ggplot(pdat, aes(x = TotalCPS > 0, y = as.numeric(Edu))) +
  geom_boxplot() +
  labs(x = "CPS Report at baseline", y = "Education level")

p3 <- ggplot(pdat, aes(x = TotalCPS > 0, y = Kessler_Score)) +
  geom_boxplot() +
  labs(x = "CPS Report at baseline", y = "Kessler score")

p4 <- ggplot(pdat, aes(x = TotalCPS > 0, y = AH_Total)) +
  geom_boxplot() +
  labs(x = "CPS Report at baseline", y = "Adult hope total score")

p5 <- ggplot(pdat, aes(x = TotalCPS > 0, y = Total_FW_Score)) +
  geom_boxplot() +
  labs(x = "CPS Report at baseline", y = "Financial wellness index score")

p6 <- pdat |>
  group_by(Race_White, TotalCPS) |>
  summarize(n = n()) |>
  mutate(prop = n / sum(n), se = prop * (1 - prop) / sqrt(n)) |>
  ggplot(aes(
    x = TotalCPS > 0,
    y = prop,
    ymin = prop - 1.96 * se,
    ymax = prop + 1.96 * se,
    color = factor(Race_White)
  )) +
  geom_pointrange(position = position_dodge(width = 0.1)) +
  labs(x = "CPS Report at baseline", y = "Proportion", color = "Race\nwhite")

p7 <- pdat |>
  group_by(Gender_f, TotalCPS) |>
  summarize(n = n()) |>
  mutate(prop = n / sum(n), se = prop * (1 - prop) / sqrt(n)) |>
  ggplot(aes(
    x = TotalCPS > 0,
    y = prop,
    ymin = prop - 1.96 * se,
    ymax = prop + 1.96 * se,
    color = factor(Gender_f)
  )) +
  geom_pointrange(position = position_dodge(width = 0.1)) +
  labs(x = "CPS Report at baseline", y = "Proportion", color = "Gender\nfemale")

p8 <- pdat |>
  group_by(SexOrient_Straight, TotalCPS) |>
  summarize(n = n()) |>
  mutate(prop = n / sum(n), se = prop * (1 - prop) / sqrt(n)) |>
  ggplot(aes(
    x = TotalCPS > 0,
    y = prop,
    ymin = prop - 1.96 * se,
    ymax = prop + 1.96 * se,
    color = factor(SexOrient_Straight)
  )) +
  geom_pointrange(position = position_dodge(width = 0.1)) +
  labs(
    x = "CPS Report at baseline",
    y = "Proportion",
    color = "Sex orient\nstraight"
  )

pdf("./output/TotalCPS_bivars.pdf", width = 18, height = 12)
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 2)
dev.off()

# Confirmed ----------------------------------------------------------------
p1 <- ggplot(pdat, aes(x = Confirmed > 0, y = n_child)) +
  geom_boxplot() +
  labs(x = "Confirmed case at baseline", y = "Number of children in household")

p2 <- ggplot(pdat, aes(x = Confirmed > 0, y = as.numeric(Edu))) +
  geom_boxplot() +
  labs(x = "Confirmed case at baseline", y = "Education level")

p3 <- ggplot(pdat, aes(x = Confirmed > 0, y = Kessler_Score)) +
  geom_boxplot() +
  labs(x = "Confirmed case at baseline", y = "Kessler score")

p4 <- ggplot(pdat, aes(x = Confirmed > 0, y = AH_Total)) +
  geom_boxplot() +
  labs(x = "Confirmed case at baseline", y = "Adult hope total score")

p5 <- ggplot(pdat, aes(x = Confirmed > 0, y = Total_FW_Score)) +
  geom_boxplot() +
  labs(x = "Confirmed case at baseline", y = "Financial wellness index score")

p6 <- pdat |>
  group_by(Race_White, Confirmed) |>
  summarize(n = n()) |>
  mutate(prop = n / sum(n), se = prop * (1 - prop) / sqrt(n)) |>
  ggplot(aes(
    x = Confirmed > 0,
    y = prop,
    ymin = prop - 1.96 * se,
    ymax = prop + 1.96 * se,
    color = factor(Race_White)
  )) +
  geom_pointrange(position = position_dodge(width = 0.1)) +
  labs(
    x = "Confirmed case at baseline",
    y = "Proportion",
    color = "Race\nwhite"
  )

p7 <- pdat |>
  group_by(Gender_f, Confirmed) |>
  summarize(n = n()) |>
  mutate(prop = n / sum(n), se = prop * (1 - prop) / sqrt(n)) |>
  ggplot(aes(
    x = Confirmed > 0,
    y = prop,
    ymin = prop - 1.96 * se,
    ymax = prop + 1.96 * se,
    color = factor(Gender_f)
  )) +
  geom_pointrange(position = position_dodge(width = 0.1)) +
  labs(
    x = "Confirmed case at baseline",
    y = "Proportion",
    color = "Gender\nfemale"
  )

p8 <- pdat |>
  group_by(SexOrient_Straight, Confirmed) |>
  summarize(n = n()) |>
  mutate(prop = n / sum(n), se = prop * (1 - prop) / sqrt(n)) |>
  ggplot(aes(
    x = Confirmed > 0,
    y = prop,
    ymin = prop - 1.96 * se,
    ymax = prop + 1.96 * se,
    color = factor(SexOrient_Straight)
  )) +
  geom_pointrange(position = position_dodge(width = 0.1)) +
  labs(
    x = "Confirmed case at baseline",
    y = "Proportion",
    color = "Sex orient\nstraight"
  )

pdf("./output/Confirmed_bivars.pdf", width = 18, height = 12)
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 2)
dev.off()

# Prev ----------------------------------------------------------------
p1 <- ggplot(pdat, aes(x = Prev > 0, y = n_child)) +
  geom_boxplot() +
  labs(
    x = "Preventive services at baseline",
    y = "Number of children in household"
  )

p2 <- ggplot(pdat, aes(x = Prev > 0, y = as.numeric(Edu))) +
  geom_boxplot() +
  labs(x = "Preventive services at baseline", y = "Education level")

p3 <- ggplot(pdat, aes(x = Prev > 0, y = Kessler_Score)) +
  geom_boxplot() +
  labs(x = "Preventive services at baseline", y = "Kessler score")

p4 <- ggplot(pdat, aes(x = Prev > 0, y = AH_Total)) +
  geom_boxplot() +
  labs(x = "Preventive services at baseline", y = "Adult hope total score")

p5 <- ggplot(pdat, aes(x = Prev > 0, y = Total_FW_Score)) +
  geom_boxplot() +
  labs(
    x = "Preventive services at baseline",
    y = "Financial wellness index score"
  )

p6 <- pdat |>
  group_by(Race_White, Prev) |>
  summarize(n = n()) |>
  mutate(prop = n / sum(n), se = prop * (1 - prop) / sqrt(n)) |>
  ggplot(aes(
    x = Prev > 0,
    y = prop,
    ymin = prop - 1.96 * se,
    ymax = prop + 1.96 * se,
    color = factor(Race_White)
  )) +
  geom_pointrange(position = position_dodge(width = 0.1)) +
  labs(
    x = "Preventive services at baseline",
    y = "Proportion",
    color = "Race\nwhite"
  )

p7 <- pdat |>
  group_by(Gender_f, Prev) |>
  summarize(n = n()) |>
  mutate(prop = n / sum(n), se = prop * (1 - prop) / sqrt(n)) |>
  ggplot(aes(
    x = Prev > 0,
    y = prop,
    ymin = prop - 1.96 * se,
    ymax = prop + 1.96 * se,
    color = factor(Gender_f)
  )) +
  geom_pointrange(position = position_dodge(width = 0.1)) +
  labs(
    x = "Preventive services at baseline",
    y = "Proportion",
    color = "Gender\nfemale"
  )

p8 <- pdat |>
  group_by(SexOrient_Straight, Prev) |>
  summarize(n = n()) |>
  mutate(prop = n / sum(n), se = prop * (1 - prop) / sqrt(n)) |>
  ggplot(aes(
    x = Prev > 0,
    y = prop,
    ymin = prop - 1.96 * se,
    ymax = prop + 1.96 * se,
    color = factor(SexOrient_Straight)
  )) +
  geom_pointrange(position = position_dodge(width = 0.1)) +
  labs(
    x = "Preventive services at baseline",
    y = "Proportion",
    color = "Sex orient\nstraight"
  )

pdf("./output/Prev_bivars.pdf", width = 18, height = 12)
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 2)
dev.off()
# FC ----------------------------------------------------------------
p1 <- ggplot(pdat, aes(x = FC > 0, y = n_child)) +
  geom_boxplot() +
  labs(x = "Foster care at baseline", y = "Number of children in household")

p2 <- ggplot(pdat, aes(x = FC > 0, y = as.numeric(Edu))) +
  geom_boxplot() +
  labs(x = "Foster care at baseline", y = "Education level")

p3 <- ggplot(pdat, aes(x = FC > 0, y = Kessler_Score)) +
  geom_boxplot() +
  labs(x = "Foster care at baseline", y = "Kessler score")

p4 <- ggplot(pdat, aes(x = FC > 0, y = AH_Total)) +
  geom_boxplot() +
  labs(x = "Foster care at baseline", y = "Adult hope total score")

p5 <- ggplot(pdat, aes(x = FC > 0, y = Total_FW_Score)) +
  geom_boxplot() +
  labs(x = "Foster care at baseline", y = "Financial wellness index score")

p6 <- pdat |>
  group_by(Race_White, FC) |>
  summarize(n = n()) |>
  mutate(prop = n / sum(n), se = prop * (1 - prop) / sqrt(n)) |>
  ggplot(aes(
    x = FC > 0,
    y = prop,
    ymin = prop - 1.96 * se,
    ymax = prop + 1.96 * se,
    color = factor(Race_White)
  )) +
  geom_pointrange(position = position_dodge(width = 0.1)) +
  labs(x = "Foster care at baseline", y = "Proportion", color = "Race\nwhite")

p7 <- pdat |>
  group_by(Gender_f, FC) |>
  summarize(n = n()) |>
  mutate(prop = n / sum(n), se = prop * (1 - prop) / sqrt(n)) |>
  ggplot(aes(
    x = FC > 0,
    y = prop,
    ymin = prop - 1.96 * se,
    ymax = prop + 1.96 * se,
    color = factor(Gender_f)
  )) +
  geom_pointrange(position = position_dodge(width = 0.1)) +
  labs(
    x = "Foster care at baseline",
    y = "Proportion",
    color = "Gender\nfemale"
  )

p8 <- pdat |>
  group_by(SexOrient_Straight, FC) |>
  summarize(n = n()) |>
  mutate(prop = n / sum(n), se = prop * (1 - prop) / sqrt(n)) |>
  ggplot(aes(
    x = FC > 0,
    y = prop,
    ymin = prop - 1.96 * se,
    ymax = prop + 1.96 * se,
    color = factor(SexOrient_Straight)
  )) +
  geom_pointrange(position = position_dodge(width = 0.1)) +
  labs(
    x = "Foster care at baseline",
    y = "Proportion",
    color = "Sex orient\nstraight"
  )

pdf("./output/FC_bivars.pdf", width = 18, height = 12)
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 2)
dev.off()

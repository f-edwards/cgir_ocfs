# take cdat, make c_long
#pivot to long -----------------------------------------------------------
## child pivot


c_Confirmed <- cdat |>
  select(ID, GROUP, Child, COUNTY, NoBaseline_FLAG,
         BaselineConfirmed:PostConfirmed) |>
  pivot_longer(cols = BaselineConfirmed:PostConfirmed,
               names_to = "Wave",
               values_to = "Confirmed") |>
  mutate(Wave = case_when(
    str_detect(Wave, "Baseline") ~ 1,
    str_detect(Wave, "Wave1") ~ 2,
    str_detect(Wave, "Wave2") ~ 3,
    str_detect(Wave, "Post") ~ 4))

c_TotalCPS <- cdat |>
  select(ID, GROUP, Child, COUNTY, NoBaseline_FLAG,
         BaselineTotalCPS:PostTotalCPS) |>
  pivot_longer(cols = BaselineTotalCPS:PostTotalCPS,
               names_to = "Wave",
               values_to = "TotalCPS") |>
  mutate(Wave = case_when(
    str_detect(Wave, "Baseline") ~ 1,
    str_detect(Wave, "Wave1") ~ 2,
    str_detect(Wave, "Wave2") ~ 3,
    str_detect(Wave, "Post") ~ 4))

c_FC <- cdat |>
  select(ID, GROUP, Child, COUNTY, NoBaseline_FLAG,
         BaselineFCEntries:PostFCEntries) |>
  pivot_longer(cols = BaselineFCEntries:PostFCEntries,
               names_to = "Wave",
               values_to = "FC") |>
  mutate(Wave = case_when(
    str_detect(Wave, "Baseline") ~ 1,
    str_detect(Wave, "Wave1") ~ 2,
    str_detect(Wave, "Wave2") ~ 3,
    str_detect(Wave, "Post") ~ 4))

c_Prev <- cdat |>
  select(ID, GROUP, Child, COUNTY, NoBaseline_FLAG,
         BaselinePreventive:PostPreventive) |>
  pivot_longer(cols = BaselinePreventive:PostPreventive,
               names_to = "Wave",
               values_to = "Prev") |>
  mutate(Wave = case_when(
    str_detect(Wave, "Baseline") ~ 1,
    str_detect(Wave, "Wave1") ~ 2,
    str_detect(Wave, "Wave2") ~ 3,
    str_detect(Wave, "Post") ~ 4))

c_long <- c_Confirmed |>
  left_join(c_TotalCPS) |>
  left_join(c_FC) |>
  left_join(c_Prev)
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
library(stringr)

# Read all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Select and rename variables
wave1_sel <- wave1 %>%
  select(NSID, W1alceverYP, W1alcmonYP) %>%
  rename(age14_alcever = W1alceverYP, age14_alcmon = W1alcmonYP)

wave2_sel <- wave2 %>%
  select(NSID, W2alceverYP) %>%
  rename(age15_alcever = W2alceverYP)

wave3_sel <- wave3 %>%
  select(NSID, W3alceverYP) %>%
  rename(age16_alcever = W3alceverYP)

wave4_sel <- wave4 %>%
  select(NSID, W4AlcEverYP) %>%
  rename(age17_alcever = W4AlcEverYP)

wave6_sel <- wave6 %>%
  select(NSID, W6AlcEverYP) %>%
  rename(age19_alcever = W6AlcEverYP)

wave7_sel <- wave7 %>%
  select(NSID, W7AlcEverYP) %>%
  rename(age20_alcever = W7AlcEverYP)

wave8_sel <- wave8 %>%
  select(NSID, W8AUDIT1, W8AUDIT2, W8AUDIT6) %>%
  rename(age25_AUDIT1 = W8AUDIT1, age25_AUDIT2 = W8AUDIT2, age25_AUDIT6 = W8AUDIT6)

wave9_sel <- wave9 %>%
  select(NSID, W9AUDIT1) %>%
  rename(age32_AUDIT1 = W9AUDIT1)

# Merge all datasets
merged_data <- wave1_sel %>%
  full_join(wave2_sel, by = "NSID") %>%
  full_join(wave3_sel, by = "NSID") %>%
  full_join(wave4_sel, by = "NSID") %>%
  full_join(wave6_sel, by = "NSID") %>%
  full_join(wave7_sel, by = "NSID") %>%
  full_join(wave8_sel, by = "NSID") %>%
  full_join(wave9_sel, by = "NSID")

# Clean variables and create drinker status
merged_data <- merged_data %>%
  mutate(
    # Age 14
    age14_alcever_clean = ifelse(is.na(age14_alcever) | age14_alcever <= -1, NA, age14_alcever),
    age14_alcmon_clean = ifelse(is.na(age14_alcmon) | age14_alcmon <= -1, NA, age14_alcmon),
    age14_drinker = case_when(
      age14_alcever_clean == 1 & age14_alcmon_clean == 1 ~ 1,
      age14_alcever_clean == 2 | age14_alcmon_clean == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    # Age 15
    age15_alcever_clean = ifelse(is.na(age15_alcever) | age15_alcever <= -1, NA, age15_alcever),
    age15_drinker = case_when(
      age15_alcever_clean == 1 ~ 1,
      age15_alcever_clean == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    # Age 16
    age16_alcever_clean = ifelse(is.na(age16_alcever) | age16_alcever <= -1, NA, age16_alcever),
    age16_drinker = case_when(
      age16_alcever_clean == 1 ~ 1,
      age16_alcever_clean == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    # Age 17
    age17_alcever_clean = ifelse(is.na(age17_alcever) | age17_alcever <= -1, NA, age17_alcever),
    age17_drinker = case_when(
      age17_alcever_clean == 1 ~ 1,
      age17_alcever_clean == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    # Age 19
    age19_alcever_clean = ifelse(is.na(age19_alcever) | age19_alcever <= -1, NA, age19_alcever),
    age19_drinker = case_when(
      age19_alcever_clean == 1 ~ 1,
      age19_alcever_clean == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    # Age 20
    age20_alcever_clean = ifelse(is.na(age20_alcever) | age20_alcever <= -1, NA, age20_alcever),
    age20_drinker = case_when(
      age20_alcever_clean == 1 ~ 1,
      age20_alcever_clean == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    # Age 25
    age25_AUDIT1_clean = ifelse(is.na(age25_AUDIT1) | age25_AUDIT1 <= -1, NA, age25_AUDIT1),
    age25_drinker = case_when(
      age25_AUDIT1_clean == 1 ~ 0,
      age25_AUDIT1_clean >= 2 ~ 1,
      TRUE ~ NA_real_
    ),
    # Age 32
    age32_AUDIT1_clean = ifelse(is.na(age32_AUDIT1) | age32_AUDIT1 <= -1, NA, age32_AUDIT1),
    age32_drinker = case_when(
      age32_AUDIT1_clean == 1 ~ 0,
      age32_AUDIT1_clean >= 2 ~ 1,
      TRUE ~ NA_real_
    )
  )

# Compute alcfst
merged_data <- merged_data %>%
  rowwise() %>%
  mutate(
    alcfst = {
      ages <- c(14, 15, 16, 17, 19, 20, 25, 32)
      status_vec <- c(
        age14_drinker,
        age15_drinker,
        age16_drinker,
        age17_drinker,
        age19_drinker,
        age20_drinker,
        age25_drinker,
        age32_drinker
      )
      min_age <- min(ages[status_vec == 1], na.rm = TRUE)
      if (min_age != Inf) {
        min_age
      } else {
        if (all(status_vec == 0) && all(!is.na(status_vec))) {
          99
        } else {
          -8
        }
      }
    }
  )

# Convert to factor
merged_data <- merged_data %>%
  mutate(
    alcfst = factor(alcfst,
      levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
      labels = c("14", "15", "16", "17", "19", "20", "25", "32", "Never had alcohol", "Don't know/insufficient information")
    )
  )

# Output to CSV
merged_data %>%
  select(NSID, alcfst) %>%
  write_csv("data/output/cleaned_data.csv")
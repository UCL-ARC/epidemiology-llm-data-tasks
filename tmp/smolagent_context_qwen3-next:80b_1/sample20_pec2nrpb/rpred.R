library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
library(stringr)

# Create output directory
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Load wave 1
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t") %>%
  select(NSID, W1alceverYP, W1alcmonYP, W1alcfreqYP) %>%
  rename(
    alcever_14 = W1alceverYP,
    alcmon_14 = W1alcmonYP,
    alcfreq_14 = W1alcfreqYP
  )

# Wave 2
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t") %>%
  select(NSID, W2alceverYP, W2alcfreqYP) %>%
  rename(
    alcever_15 = W2alceverYP,
    alcfreq_15 = W2alcfreqYP
  )

# Wave 3
wave3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t") %>%
  select(NSID, W3alceverYP, W3alcfreqYP) %>%
  rename(
    alcever_16 = W3alceverYP,
    alcfreq_16 = W3alcfreqYP
  )

# Wave 4
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t") %>%
  select(NSID, W4AlcEverYP, W4AlcFreqYP) %>%
  rename(
    alcever_17 = W4AlcEverYP,
    alcfreq_17 = W4AlcFreqYP
  )

# Wave 6
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t") %>%
  select(NSID, W6AlcEverYP, W6AlcFreqYP) %>%
  rename(
    alcever_19 = W6AlcEverYP,
    alcfreq_19 = W6AlcFreqYP
  )

# Wave 7
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t") %>%
  select(NSID, W7AlcEverYP, W7AlcFreqYP) %>%
  rename(
    alcever_20 = W7AlcEverYP,
    alcfreq_20 = W7AlcFreqYP
  )

# Wave 8
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t") %>%
  select(NSID, W8AUDIT2, W8AUDIT6) %>%
  rename(
    audit2_25 = W8AUDIT2,
    audit6_25 = W8AUDIT6
  )

# Wave 9
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t") %>%
  select(NSID, W9AUDIT1, W9AUDIT2, W9AUDIT3) %>%
  rename(
    audit1_32 = W9AUDIT1,
    audit2_32 = W9AUDIT2,
    audit3_32 = W9AUDIT3
  )

# Merge all
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define drinker functions (vectorized)
is_drinker_14 <- function(alcever, alcmon) {
  na_check <- is.na(alcever) | is.na(alcmon)
  result <- ifelse(na_check, NA, ifelse(alcever == 1 & alcmon == 1, TRUE, FALSE))
  return(result)
}

is_drinker_15 <- function(alcever) {
  na_check <- is.na(alcever)
  result <- ifelse(na_check, NA, ifelse(alcever == 1, TRUE, FALSE))
  return(result)
}

is_drinker_25 <- function(audit6) {
  na_check <- is.na(audit6)
  result <- ifelse(na_check, NA, ifelse(audit6 != 1, TRUE, FALSE))
  return(result)
}

is_drinker_32 <- function(audit1) {
  na_check <- is.na(audit1)
  result <- ifelse(na_check, NA, ifelse(audit1 != 1, TRUE, FALSE))
  return(result)
}

# Compute drinker status for each age
merged_data <- merged_data %>%
  mutate(
    drinker_14 = is_drinker_14(alcever_14, alcmon_14),
    drinker_15 = is_drinker_15(alcever_15),
    drinker_16 = is_drinker_15(alcever_16),
    drinker_17 = is_drinker_15(alcever_17),
    drinker_19 = is_drinker_15(alcever_19),
    drinker_20 = is_drinker_15(alcever_20),
    drinker_25 = is_drinker_25(audit6_25),
    drinker_32 = is_drinker_32(audit1_32)
  )

# Determine alcfst
merged_data <- merged_data %>%
  mutate(
    has_missing = rowSums(is.na(across(starts_with("drinker_")))) > 0,
    all_false = rowSums(across(starts_with("drinker_")), na.rm = TRUE) == 0,
    confirmed_never = !has_missing & all_false
  ) %>%
  mutate(
    alcfst = case_when(
      drinker_14 == TRUE ~ 14,
      drinker_15 == TRUE ~ 15,
      drinker_16 == TRUE ~ 16,
      drinker_17 == TRUE ~ 17,
      drinker_19 == TRUE ~ 19,
      drinker_20 == TRUE ~ 20,
      drinker_25 == TRUE ~ 25,
      drinker_32 == TRUE ~ 32,
      confirmed_never ~ 99,
      TRUE ~ -8
    )
  ) %>%
  mutate(
    alcfst = factor(
      alcfst,
      levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
      labels = c("14", "15", "16", "17", "19", "20", "25", "32", "Never had alcohol", "Don't know/insufficient information")
    )
  )

# Output only NSID and alcfst
output <- merged_data %>% select(NSID, alcfst)
write_csv(output, "data/output/cleaned_data.csv")
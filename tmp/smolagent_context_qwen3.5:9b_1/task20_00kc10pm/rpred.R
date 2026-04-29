library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
library(stringr)

# Set directories
input_dir <- "data/input/"
output_dir <- "data/output/"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Load all data files
wave1 <- read_delim(paste0(input_dir, "wave_one_lsype_young_person_2020.tab"), delim = "\t")
wave2 <- read_delim(paste0(input_dir, "wave_two_lsype_young_person_2020.tab"), delim = "\t")
wave3 <- read_delim(paste0(input_dir, "wave_three_lsype_young_person_2020.tab"), delim = "\t")
wave4 <- read_delim(paste0(input_dir, "wave_four_lsype_young_person_2020.tab"), delim = "\t")
wave6 <- read_delim(paste0(input_dir, "wave_six_lsype_young_person_2020.tab"), delim = "\t")
wave7 <- read_delim(paste0(input_dir, "wave_seven_lsype_young_person_2020.tab"), delim = "\t")
wave8 <- read_delim(paste0(input_dir, "ns8_2015_self_completion.tab"), delim = "\t")
wave9 <- read_delim(paste0(input_dir, "ns9_2022_main_interview.tab"), delim = "\t")

# Process each wave to create a data frame with NSID, age, and drinker indicator
# Wave 1 (Age 14): drinker if BOTH W1alceverYP = 1 AND W1alcmonYP = 1
drinker_wave1 <- wave1 %>%
  select(NSID, W1alceverYP, W1alcmonYP) %>%
  mutate(drinker = case_when(
    W1alceverYP == 1 & W1alcmonYP == 1 ~ 1,
    TRUE ~ NA_real_
  )) %>%
  mutate(age = 14)

# Wave 2 (Age 15): drinker if W2alceverYP = 1
drinker_wave2 <- wave2 %>%
  select(NSID, W2alceverYP) %>%
  mutate(drinker = case_when(
    W2alceverYP == 1 ~ 1,
    W2alceverYP == 2 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(age = 15)

# Wave 3 (Age 16): drinker if W3alceverYP = 1
drinker_wave3 <- wave3 %>%
  select(NSID, W3alceverYP) %>%
  mutate(drinker = case_when(
    W3alceverYP == 1 ~ 1,
    W3alceverYP == 2 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(age = 16)

# Wave 4 (Age 17): drinker if W4AlcEverYP = 1
drinker_wave4 <- wave4 %>%
  select(NSID, W4AlcEverYP) %>%
  mutate(drinker = case_when(
    W4AlcEverYP == 1 ~ 1,
    W4AlcEverYP == 2 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(age = 17)

# Wave 6 (Age 19): drinker if W6AlcEverYP = 1
drinker_wave6 <- wave6 %>%
  select(NSID, W6AlcEverYP) %>%
  mutate(drinker = case_when(
    W6AlcEverYP == 1 ~ 1,
    W6AlcEverYP == 2 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(age = 19)

# Wave 7 (Age 20): drinker if W7AlcEverYP = 1
drinker_wave7 <- wave7 %>%
  select(NSID, W7AlcEverYP) %>%
  mutate(drinker = case_when(
    W7AlcEverYP == 1 ~ 1,
    W7AlcEverYP == 2 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(age = 20)

# Wave 8 (Age 25): drinker if W8AUDIT6 > 1 (i.e., not "Never")
drinker_wave8 <- wave8 %>%
  select(NSID, W8AUDIT6) %>%
  mutate(drinker = case_when(
    W8AUDIT6 > 1 ~ 1,
    W8AUDIT6 == 1 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(age = 25)

# Wave 9 (Age 32): drinker if W9AUDIT1 > 1 (i.e., not "Never")
drinker_wave9 <- wave9 %>%
  select(NSID, W9AUDIT1) %>%
  mutate(drinker = case_when(
    W9AUDIT1 > 1 ~ 1,
    W9AUDIT1 == 1 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(age = 32)

# Combine all waves
all_drinker_data <- bind_rows(
  drinker_wave1, drinker_wave2, drinker_wave3, drinker_wave4, 
  drinker_wave6, drinker_wave7, drinker_wave8, drinker_wave9
)

# Sort by NSID and age
all_drinker_data <- all_drinker_data %>%
  arrange(NSID, age)

# Calculate alcfst for each individual
alcfst <- all_drinker_data %>%
  group_by(NSID) %>%
  summarise(
    has_drinker = any(drinker == 1),
    all_non_drinker = all(!is.na(drinker) & drinker == 0),
    has_missing = any(is.na(drinker)),
    first_age = ifelse(has_drinker, min(age[drinker == 1]), NA),
    .groups = "drop"
  ) %>%
  mutate(
    alcfst = case_when(
      has_drinker ~ first_age,
      all_non_drinker & !has_missing ~ 99,
      TRUE ~ -8
    )
  ) %>%
  select(NSID, alcfst)

# Write output
write_csv(alcfst, paste0(output_dir, "cleaned_data.csv"))

# Print summary for verification
print("Summary of alcfst values:")
print(table(alcfst$alcfst))

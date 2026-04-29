library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load each dataset
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave_three <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Rename variables to match the required format
wave_one <- wave_one %>% rename(lang_S1 = W1englangYP)
wave_two <- wave_two %>% rename(lang_S2 = W2EnglangYP)
wave_three <- wave_three %>% rename(lang_S3 = W3englangHH)
wave_four <- wave_four %>% rename(lang_S4 = W4EngLangHH)

# Merge datasets using full_join by NSID
merged_data <- full_join(wave_one, wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID")

# Harmonize missing value codes
merged_data <- merged_data %>%
  mutate(across(c(lang_S1, lang_S2, lang_S3, lang_S4), ~ case_when(
    .x == -999 ~ -2,
    .x == -998 ~ -2,
    .x == -997 ~ -2,
    .x == -995 ~ -2,
    .x == -99 ~ -3,
    .x == -92 ~ -9,
    .x == -91 ~ -1,
    .x == -1 ~ -8,
    is.na(.x) ~ -3,
    TRUE ~ .x
  )))

# Create consolidated variable for lang
merged_data <- merged_data %>%
  mutate(lang = case_when(
    !is.na(lang_S1) & lang_S1 > 0 ~ lang_S1,
    !is.na(lang_S2) & lang_S2 > 0 ~ lang_S2,
    !is.na(lang_S3) & lang_S3 > 0 ~ lang_S3,
    !is.na(lang_S4) & lang_S4 > 0 ~ lang_S4,
    !is.na(lang_S1) ~ lang_S1,
    !is.na(lang_S2) ~ lang_S2,
    !is.na(lang_S3) ~ lang_S3,
    !is.na(lang_S4) ~ lang_S4,
    TRUE ~ -3
  ))

# Select only the ID variable and the final consolidated variables
cleaned_data <- merged_data %>%
  select(NSID, lang)

# Write the cleaned data to a CSV file
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)
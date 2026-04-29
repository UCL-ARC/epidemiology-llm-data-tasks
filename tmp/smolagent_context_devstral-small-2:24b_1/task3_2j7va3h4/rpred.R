library(dplyr)
library(purrr)
library(readr)
library(labelled)

# Load datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Rename variables to match naming conventions
wave_one <- wave_one %>% rename(lang_S1 = W1englangYP)
wave_two <- wave_two %>% rename(lang_S2 = W2EnglangYP)
wave_three <- wave_three %>% rename(lang_S3 = W3englangHH)
wave_four <- wave_four %>% rename(lang_S4 = W4EngLangHH)

# Harmonize missing value codes
wave_one <- wave_one %>% mutate(lang_S1 = case_when(
  lang_S1 == -999.0 ~ -2,
  lang_S1 == -99.0 ~ -3,
  lang_S1 == -92.0 ~ -9,
  lang_S1 == -91.0 ~ -1,
  lang_S1 == -1.0 ~ -8,
  TRUE ~ lang_S1
))

wave_two <- wave_two %>% mutate(lang_S2 = case_when(
  lang_S2 == -998.0 ~ -2,
  lang_S2 == -997.0 ~ -2,
  lang_S2 == -995.0 ~ -2,
  lang_S2 == -99.0 ~ -3,
  lang_S2 == -92.0 ~ -9,
  lang_S2 == -91.0 ~ -1,
  lang_S2 == -1.0 ~ -8,
  TRUE ~ lang_S2
))

wave_three <- wave_three %>% mutate(lang_S3 = case_when(
  lang_S3 == -999.0 ~ -2,
  lang_S3 == -997.0 ~ -2,
  lang_S3 == -99.0 ~ -3,
  lang_S3 == -92.0 ~ -9,
  lang_S3 == -91.0 ~ -1,
  lang_S3 == -1.0 ~ -8,
  TRUE ~ lang_S3
))

wave_four <- wave_four %>% mutate(lang_S4 = case_when(
  lang_S4 == -999.0 ~ -2,
  lang_S4 == -997.0 ~ -2,
  lang_S4 == -92.0 ~ -9,
  lang_S4 == -91.0 ~ -1,
  lang_S4 == -1.0 ~ -8,
  TRUE ~ lang_S4
))

# Merge datasets
merged_data <- full_join(wave_one, wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID")

# Create consolidated variable
merged_data <- merged_data %>% mutate(lang = case_when(
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

# Select only the ID and consolidated variables
cleaned_data <- merged_data %>% select(NSID, lang)

# Write to output file
write_csv(cleaned_data, "data/output/cleaned_data.csv")
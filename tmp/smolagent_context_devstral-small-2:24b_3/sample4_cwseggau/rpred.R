library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
ns8_2015 <- readr::read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
ns9_2022 <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(ns8_2015, by = "NSID") %>%
  full_join(ns9_2022, by = "NSID")

# Standard missing value codes
standard_missing_codes <- list(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-7" = "Prefer not to say",
  "-1" = "Item not applicable",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable"
)

# Harmonize missing value codes
merged_data <- merged_data %>%
  mutate(across(where(is.numeric), ~ case_when(
    .x %in% c(-999, -998, -997, -94, -92, -91, -99) ~ -3,
    TRUE ~ .x
  )))

# Create age-specific variables for sexuality
merged_data <- merged_data %>%
  mutate(
    sori14 = NA_real_,
    sori17 = NA_real_,
    sori19 = NA_real_,
    sori20 = NA_real_,
    sori32 = NA_real_
  )

# Map sexuality variables to age-specific variables
merged_data <- merged_data %>%
  mutate(
    sori14 = case_when(
      !is.na(W6SexualityYP) ~ W6SexualityYP,
      TRUE ~ -3
    ),
    sori17 = case_when(
      !is.na(W7SexualityYP) ~ W7SexualityYP,
      TRUE ~ -3
    ),
    sori19 = case_when(
      !is.na(W8SEXUALITY) ~ W8SEXUALITY,
      TRUE ~ -3
    ),
    sori20 = case_when(
      !is.na(W9SORI) ~ W9SORI,
      TRUE ~ -3
    ),
    sori32 = case_when(
      !is.na(W9SORI) ~ W9SORI,
      TRUE ~ -3
    )
  )

# Create consolidated sexuality variable
merged_data <- merged_data %>%
  mutate(
    sori = case_when(
      !is.na(sori14) ~ sori14,
      !is.na(sori17) ~ sori17,
      !is.na(sori19) ~ sori19,
      !is.na(sori20) ~ sori20,
      !is.na(sori32) ~ sori32,
      TRUE ~ -3
    )
  )

# Convert to factor with labels
merged_data <- merged_data %>%
  mutate(
    sori = factor(sori, levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4),
                   labels = c("Refusal", "Don't know/insufficient information", "Prefer not to say", 
                             "Not asked at the fieldwork stage/participated/interviewed", 
                             "Schedule not applicable", "Item not applicable", 
                             "Heterosexual / Straight", "Gay / Lesbian", "Bisexual", "Other"))
  )

# Select final variables
final_data <- merged_data %>%
  select(NSID, sori14, sori17, sori19, sori20, sori32, sori)

# Save to CSV
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)
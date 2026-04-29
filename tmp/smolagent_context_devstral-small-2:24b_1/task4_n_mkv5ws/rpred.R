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
missing_codes <- list(
  "-999.0" = -9, "-998.0" = -9, "-997.0" = -9, "-94.0" = -9, "-92.0" = -9, "-91.0" = -9, "-99.0" = -9,
  "-9.0" = -9, "-8.0" = -8, "-7.0" = -7, "-1.0" = -1, "-3.0" = -3, "-2.0" = -2
)

# Harmonize missing values for specific variables
merged_data <- merged_data %>%
  mutate(
    W6SexualityYP = ifelse(as.character(W6SexualityYP) %in% names(missing_codes), 
                            missing_codes[[as.character(W6SexualityYP)]], W6SexualityYP),
    W7SexualityYP = ifelse(as.character(W7SexualityYP) %in% names(missing_codes), 
                            missing_codes[[as.character(W7SexualityYP)]], W7SexualityYP),
    W8SEXUALITY = ifelse(as.character(W8SEXUALITY) %in% names(missing_codes), 
                          missing_codes[[as.character(W8SEXUALITY)]], W8SEXUALITY),
    W9SORI = ifelse(as.character(W9SORI) %in% names(missing_codes), 
                    missing_codes[[as.character(W9SORI)]], W9SORI)
  )

# Create age-specific variables for sexuality
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

# Convert to factors with labels
merged_data <- merged_data %>%
  mutate(
    sori14 = factor(sori14, levels = c(-9, -8, -7, -1, -3, -2, 1, 2, 3, 4),
                     labels = c("Refusal", "Don't know", "Prefer not to say", "Item not applicable", 
                               "Not asked", "Schedule not applicable", "Heterosexual / Straight", 
                               "Gay / Lesbian", "Bisexual", "Other")),
    sori17 = factor(sori17, levels = c(-9, -8, -7, -1, -3, -2, 1, 2, 3, 4),
                     labels = c("Refusal", "Don't know", "Prefer not to say", "Item not applicable", 
                               "Not asked", "Schedule not applicable", "Heterosexual / Straight", 
                               "Gay / Lesbian", "Bisexual", "Other")),
    sori19 = factor(sori19, levels = c(-9, -8, -7, -1, -3, -2, 1, 2, 3, 4),
                     labels = c("Refusal", "Don't know", "Prefer not to say", "Item not applicable", 
                               "Not asked", "Schedule not applicable", "Heterosexual / Straight", 
                               "Gay / Lesbian", "Bisexual", "Other")),
    sori20 = factor(sori20, levels = c(-9, -8, -7, -1, -3, -2, 1, 2, 3, 4),
                     labels = c("Refusal", "Don't know", "Prefer not to say", "Item not applicable", 
                               "Not asked", "Schedule not applicable", "Heterosexual / Straight", 
                               "Gay / Lesbian", "Bisexual", "Other")),
    sori32 = factor(sori32, levels = c(-9, -8, -7, -1, -3, -2, 1, 2, 3, 4),
                     labels = c("Refusal", "Don't know", "Prefer not to say", "Item not applicable", 
                               "Not asked", "Schedule not applicable", "Heterosexual / Straight", 
                               "Gay / Lesbian", "Bisexual", "Other")),
    sori = factor(sori, levels = c(-9, -8, -7, -1, -3, -2, 1, 2, 3, 4),
                  labels = c("Refusal", "Don't know", "Prefer not to say", "Item not applicable", 
                            "Not asked", "Schedule not applicable", "Heterosexual / Straight", 
                            "Gay / Lesbian", "Bisexual", "Other"))
  )

# Select final variables
final_data <- merged_data %>%
  select(NSID, sori14, sori17, sori19, sori20, sori32, sori)

# Write output
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)
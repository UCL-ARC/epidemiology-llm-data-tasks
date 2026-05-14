library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load each dataset
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
ns8_2015 <- readr::read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
ns9_2022 <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(ns8_2015, by = "NSID") %>%
  full_join(ns9_2022, by = "NSID")

# Standard missing-value codes
standard_missing_codes <- list(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-7" = "Prefer not to say",
  "-1" = "Item not applicable",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable"
)

# Harmonize missing value codes for sexuality variables
merged_data <- merged_data %>%
  mutate(
    sori14 = case_when(
      W6SexualityYP == -999.0 ~ -9,
      W6SexualityYP == -998.0 ~ -8,
      W6SexualityYP == -997.0 ~ -7,
      W6SexualityYP == -94.0 ~ -3,
      W6SexualityYP == -92.0 ~ -9,
      W6SexualityYP == -91.0 ~ -1,
      W6SexualityYP == -1.0 ~ -8,
      TRUE ~ W6SexualityYP
    ),
    sori17 = case_when(
      W7SexualityYP == -999.0 ~ -9,
      W7SexualityYP == -998.0 ~ -8,
      W7SexualityYP == -997.0 ~ -7,
      W7SexualityYP == -94.0 ~ -3,
      W7SexualityYP == -92.0 ~ -9,
      W7SexualityYP == -91.0 ~ -1,
      W7SexualityYP == -1.0 ~ -8,
      TRUE ~ W7SexualityYP
    ),
    sori20 = case_when(
      W8SEXUALITY == -9.0 ~ -9,
      W8SEXUALITY == -8.0 ~ -8,
      W8SEXUALITY == -1.0 ~ -1,
      TRUE ~ W8SEXUALITY
    ),
    sori32 = case_when(
      W9SORI == -9.0 ~ -9,
      W9SORI == -8.0 ~ -8,
      W9SORI == -3.0 ~ -3,
      W9SORI == -1.0 ~ -1,
      W9SORI == 5.0 ~ -7,
      TRUE ~ W9SORI
    )
  )

# Create a consolidated sexuality variable prioritizing the earliest valid response
merged_data <- merged_data %>%
  mutate(
    sori = case_when(
      !is.na(sori14) & sori14 > 0 ~ sori14,
      !is.na(sori17) & sori17 > 0 ~ sori17,
      !is.na(sori20) & sori20 > 0 ~ sori20,
      !is.na(sori32) & sori32 > 0 ~ sori32,
      TRUE ~ NA_real_
    )
  )

# Convert to factor with labels
merged_data <- merged_data %>%
  mutate(
    sori = factor(sori, levels = c(1:4), labels = c("Heterosexual / Straight", "Gay / Lesbian", "Bisexual", "Other")),
    sori14 = factor(sori14, levels = c(-9, -8, -7, -1, -3, -2, 1:4), labels = c("Refusal", "Don't know/insufficient information", "Prefer not to say", "Item not applicable", "Not asked at the fieldwork stage/participated/interviewed", "Schedule not applicable", "Heterosexual / Straight", "Gay / Lesbian", "Bisexual", "Other")),
    sori17 = factor(sori17, levels = c(-9, -8, -7, -1, -3, -2, 1:4), labels = c("Refusal", "Don't know/insufficient information", "Prefer not to say", "Item not applicable", "Not asked at the fieldwork stage/participated/interviewed", "Schedule not applicable", "Heterosexual / Straight", "Gay / Lesbian", "Bisexual", "Other")),
    sori20 = factor(sori20, levels = c(-9, -8, -7, -1, -3, -2, 1:4), labels = c("Refusal", "Don't know/insufficient information", "Prefer not to say", "Item not applicable", "Not asked at the fieldwork stage/participated/interviewed", "Schedule not applicable", "Heterosexual / Straight", "Gay / Lesbian", "Bisexual", "Other")),
    sori32 = factor(sori32, levels = c(-9, -8, -7, -1, -3, -2, 1:4), labels = c("Refusal", "Don't know/insufficient information", "Prefer not to say", "Item not applicable", "Not asked at the fieldwork stage/participated/interviewed", "Schedule not applicable", "Heterosexual / Straight", "Gay / Lesbian", "Bisexual", "Other"))
  )

# Select only the derived variables for output
output_data <- merged_data %>%
  select(NSID, sori, sori14, sori17, sori20, sori32)

# Write the cleaned data to a CSV file
write.csv(output_data, "data/output/cleaned_data.csv", row.names = FALSE)
library(haven)
library(dplyr)
library(purrr)
library(readr)

# Load each dataset
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_2015_derived <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_2022_derived <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
ns9_2022_main <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8_2015_derived, by = "NSID") %>%
  full_join(ns9_2022_derived, by = "NSID") %>%
  full_join(ns9_2022_main, by = "NSID")

# Standard missing-value codes
standard_missing_codes <- list(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-1" = "Item not applicable",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-7" = "Prefer not to say"
)

# Convert wave-specific negative codes to standard missing-value codes
merged_data <- merged_data %>%
  mutate(across(where(is.numeric), ~ case_when(
    .x %in% c(-100, -97, -999, -998, -997, -995, -94, -92, -91, -99) ~ -3,
    TRUE ~ .x
  )))

# Check for the existence of variables before using them
if ("urbind" %in% names(merged_data)) {
  merged_data <- merged_data %>%
    mutate(
      regub14 = ifelse(urbind %in% c(1:8), as.factor(urbind), NA_integer_),
      regub15 = ifelse(urbind %in% c(1:8), as.factor(urbind), NA_integer_),
      regub16 = ifelse(urbind %in% c(1:8), as.factor(urbind), NA_integer_),
      regub17 = ifelse(urbind %in% c(1:8), as.factor(urbind), NA_integer_)
    )
}

if ("gor" %in% names(merged_data)) {
  merged_data <- merged_data %>%
    mutate(
      regov14 = ifelse(gor %in% c(1:9), as.factor(gor), NA_integer_),
      regov15 = ifelse(gor %in% c(1:9), as.factor(gor), NA_integer_),
      regov16 = ifelse(gor %in% c(1:9), as.factor(gor), NA_integer_),
      regov17 = ifelse(gor %in% c(1:9), as.factor(gor), NA_integer_)
    )
}

if ("W9DRGN" %in% names(merged_data)) {
  merged_data <- merged_data %>%
    mutate(
      regov32 = ifelse(W9DRGN %in% c(1:13), as.factor(W9DRGN), NA_integer_)
    )
}

if ("W9NATIONRES" %in% names(merged_data)) {
  merged_data <- merged_data %>%
    mutate(
      regint32 = ifelse(W9NATIONRES %in% c(1:5), as.factor(W9NATIONRES), NA_integer_)
    )
}

# Save the cleaned data to a single CSV file
write.csv(merged_data, "data/output/cleaned_data.csv", row.names = FALSE)

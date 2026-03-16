library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave_nine <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Function to recode missing values for numeric columns only
recode_missing <- function(x) {
  x %>%
    mutate(across(where(is.numeric), ~ case_when(
      .x %in% c(-999, -998, -997, -996, -995, -994, -993, -992, -991, -990, -989, -988, -987, -986, -985, -984, -983, -982, -981, -980) ~ -2,
      .x == -94 ~ -2,
      .x == -91 ~ -1,
      .x == -9 ~ -9,
      .x == -8 ~ -8,
      .x == -3 ~ -3,
      .x == -1 ~ -1,
      TRUE ~ .x
    )))
}

# Recode missing values for all datasets
merged_data <- recode_missing(merged_data)

# Function to harmonize education variables
harmonize_education <- function(data) {
  # Age 17
  data <- data %>%
    mutate(
      educaim17 = case_when(
        w4saim %in% c(1, 5, 9) ~ 2,  # NVQ 1-3
        w4saim %in% c(2, 3, 4, 6, 7, 8, 10, 11) ~ 1,  # NVQ 1-3 (other)
        w4saim == 12 ~ 3,  # Other
        w4saim == 13 ~ 4,  # No detail
        w4saim == 14 ~ 5,  # Not studying
        TRUE ~ -3  # Not asked or missing
      )
    )

  # Age 19
  data <- data %>%
    mutate(
      educaim19 = case_when(
        W6Saim %in% c(1, 3, 5, 9, 11, 12) ~ 2,  # NVQ 1-3
        W6Saim %in% c(2, 4, 6, 7, 8, 10, 13, 14) ~ 1,  # NVQ 1-3 (other)
        W6Saim == 15 ~ 3,  # Other (level unknown)
        W6Saim == 16 ~ 5,  # Not studying
        TRUE ~ -3  # Not asked or missing
      )
    )

  # Age 20
  data <- data %>%
    mutate(
      educaim20 = case_when(
        W7SAim %in% c(1, 3, 5, 7, 9, 11, 13) ~ 2,  # NVQ 1-3
        W7SAim %in% c(2, 4, 6, 8, 10, 12, 14) ~ 1,  # NVQ 1-3 (other)
        W7SAim == -94 ~ -2,  # Insufficient information
        W7SAim == -91 ~ -1,  # Not applicable (not studying)
        TRUE ~ -3  # Not asked or missing
      )
    )

  # Age 25
  data <- data %>%
    mutate(
      is_studying = W8ACTIVITY05 == 1,
      has_high_qual = any(c(W8ACQUC0A, W8ACQUC0B, W8ACQUC0C, W8ACQUC0D, W8ACQUC0E, W8VCQUC0A, W8VCQUC0K) == 1),
      has_mid_qual = any(c(W8ACQUC0F, W8ACQUC0G, W8ACQUC0H, W8ACQUC0I, W8ACQUC0J, W8ACQUC0L, W8VCQUC0B, W8VCQUC0C, W8VCQUC0D, W8VCQUC0E, W8VCQUC0F, W8VCQUC0J) == 1),
      educaim25 = case_when(
        is_studying == FALSE ~ 5,  # Not studying
        has_high_qual == TRUE ~ 0,  # NVQ 4-5
        has_mid_qual == TRUE ~ 1,  # NVQ 1-3
        TRUE ~ -3  # Not asked or missing
      )
    )

  # Age 32
  data <- data %>%
    mutate(
      is_studying = W9ECONACT2 %in% c(6, 7),  # Full-time or part-time education
      has_high_qual = any(c(W9ACQUC0A, W9ACQUC0B, W9ACQUC0C, W9ACQUC0D, W9ACQUC0E, W9VCQUC0A, W9VCQUC0C, W9VCQUCAA) == 1),
      has_mid_qual = any(c(W9ACQUC0F, W9ACQUC0G, W9ACQUC0H, W9ACQUC0I, W9ACQUC0J, W9ACQUC0K, W9ACQUC0L, W9ACQUC0M, W9ACQUC0N, W9ACQUC0O, W9ACQUC0P, W9ACQUC0Q,
                          W9VCQUC0B, W9VCQUC0D, W9VCQUC0E, W9VCQUC0F, W9VCQUC0G, W9VCQUC0H, W9VCQUC0I, W9VCQUC0J, W9VCQUC0K, W9VCQUC0L, W9VCQUC0M, W9VCQUC0N,
                          W9VCQUC0O, W9VCQUC0P, W9VCQUC0Q, W9VCQUC0R, W9VCQUC0S, W9VCQUC0T, W9VCQUC0U, W9VCQUC0V, W9VCQUC0W, W9VCQUC0X, W9VCQUC0Y, W9VCQUC0Z,
                          W9VCQUCAA, W9VCQUCAB, W9VCQUCAC, W9VCQUCAD) == 1),
      educaim32 = case_when(
        is_studying == FALSE ~ 5,  # Not studying
        has_high_qual == TRUE ~ 0,  # NVQ 4-5
        has_mid_qual == TRUE ~ 1,  # NVQ 1-3
        TRUE ~ -3  # Not asked or missing
      )
    )

  return(data)
}

# Apply harmonization for each wave
merged_data <- harmonize_education(merged_data)

# Select only NSID and the harmonized education variables
final_data <- merged_data %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write the final output
write_csv(final_data, "data/output/cleaned_data.csv")
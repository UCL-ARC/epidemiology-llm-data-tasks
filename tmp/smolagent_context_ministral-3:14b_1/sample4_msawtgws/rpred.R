
# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Step 1: Load datasets
files <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "data/input/wave_seven_lsype_young_person_2020.tab",
  ns8_self_completion = "data/input/ns8_2015_self_completion.tab",
  ns9_main_interview = "data/input/ns9_2022_main_interview.tab"
)

# Load each file into a separate dataframe
load_data <- function(file) {
  read_delim(file, delim = "\t")
}

datasets <- map(files, load_data)

# Name each dataset for clarity
names(datasets) <- c("wave_one", "wave_four", "wave_six", "wave_seven", "ns8_self_completion", "ns9_main_interview")

# Step 2: Merge datasets using NSID
merged_data <- reduce(datasets, full_join, by = "NSID")

# Step 3: Define missing value harmonization function (only for numeric columns)
harmonize_missing <- function(x) {
  x %>%
    mutate(across(
      where(is.numeric),
      ~ case_when(
        .x %in% c(-999, -998, -997, -94, -92, -91, -99) ~
          case_when(
            .x == -999 ~ -9,  # Refusal
            .x == -998 ~ -8,  # Don't know
            .x == -997 ~ -7,  # Prefer not to say
            .x == -94 ~ -2,   # Schedule not applicable
            .x == -92 ~ -9,   # Refused
            .x == -91 ~ -1,   # Not applicable
            .x == -99 ~ -3,   # Not asked
            TRUE ~ .x
          ),
        .x == -100 ~ -9,     # Refused (specific to W7SexualityYP)
        .x == -3 ~ -3,       # Not asked
        is.na(.x) ~ -3,      # Null values
        TRUE ~ .x
      )
    ))
}

# Step 4: Harmonize missing values for numeric columns
merged_data <- harmonize_missing(merged_data)

# Step 5: Extract and consolidate sexual orientation variables
consolidate_sori <- function(df) {
  # Check which sexual orientation columns exist and select them
  sori_cols <- c("W6SexualityYP", "W7SexualityYP", "W8SEXUALITY", "W9SORI")
  existing_cols <- sapply(sori_cols, function(col) col %in% names(df))

  sori_vars <- df %>%
    select(NSID, all_of(sori_cols[existing_cols])) %>%
    rename(
      sori_yp_6 = W6SexualityYP,
      sori_yp_7 = W7SexualityYP,
      sori_8 = W8SEXUALITY,
      sori_9 = W9SORI
    ) %>%
    mutate(
      sori = case_when(
        !is.na(sori_9) & sori_9 != -3 & sori_9 != -9 & sori_9 != -8 & sori_9 != -7 & sori_9 != -1 & sori_9 != -2 ~ sori_9,
        !is.na(sori_8) & sori_8 != -3 & sori_8 != -9 & sori_8 != -8 & sori_8 != -7 & sori_8 != -1 & sori_8 != -2 ~ sori_8,
        !is.na(sori_yp_7) & sori_yp_7 != -3 & sori_yp_7 != -9 & sori_yp_7 != -8 & sori_yp_7 != -7 & sori_yp_7 != -1 & sori_yp_7 != -2 ~ sori_yp_7,
        !is.na(sori_yp_6) & sori_yp_6 != -3 & sori_yp_6 != -9 & sori_yp_6 != -8 & sori_yp_6 != -7 & sori_yp_6 != -1 & sori_yp_6 != -2 ~ sori_yp_6,
        TRUE ~ -3  # Fallback to -3 if no valid response
      )
    ) %>%
    select(NSID, sori)
}

# Step 6: Consolidate sexual orientation
sori_consolidated <- consolidate_sori(merged_data)

# Step 7: Recode sexual orientation variable with labels
sori_labels <- c(
  `-9` = "Refused",
  `-8` = "Don't know",
  `-7` = "Prefer not to say",
  `-3` = "Not asked",
  `-2` = "Schedule not applicable",
  `-1` = "Not applicable",
  `1` = "Heterosexual / Straight",
  `2` = "Gay / Lesbian",
  `3` = "Bisexual",
  `4` = "Other"
)

sori_consolidated$sori <- factor(sori_consolidated$sori, levels = as.character(names(sori_labels)), labels = sori_labels)

# Step 8: Merge consolidated variable back into main dataset
merged_data <- merged_data %>%
  left_join(sori_consolidated, by = "NSID")

# Step 9: Select only the ID and derived variables for output
derived_vars <- merged_data %>%
  select(NSID, sori)

# Step 10: Write output to CSV
write_csv(derived_vars, "data/output/cleaned_data.csv")

# Print confirmation
message("Cleaned data has been saved to data/output/cleaned_data.csv")


# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
file_paths <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "data/input/wave_seven_lsype_young_person_2020.tab",
  ns8 = "data/input/ns8_2015_self_completion.tab",
  ns9 = "data/input/ns9_2022_main_interview.tab"
)

# Load datasets
load_dataset <- function(file_path) {
  read_delim(file_path, delim = "\t")
}

datasets <- map(file_paths, load_dataset)

# Name datasets for clarity
wave_one <- datasets[[1]]
wave_four <- datasets[[2]]
wave_six <- datasets[[3]]
wave_seven <- datasets[[4]]
ns8 <- datasets[[5]]
ns9 <- datasets[[6]]

# Merge datasets by NSID
merged_data <- full_join(
  full_join(wave_one, wave_four, by = "NSID"),
  full_join(wave_six, wave_seven, by = "NSID"),
  by = "NSID"
) %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Function to harmonize missing values
harmonize_missing_values <- function(x) {
  if (is.numeric(x)) {
    x <- replace(x, x == -999 | x == -998 | x == -997 | x == -94 | x == -92 | x == -91, -3)
    x <- replace(x, x == -9, -9)
    x <- replace(x, x == -8, -8)
    x <- replace(x, x == -1, -1)
    x <- replace(x, x == -100, -7)
    x <- replace(x, is.na(x), -3)
  }
  return(x)
}

# Define standard factor levels for sexual orientation
sori_levels <- c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5)
sori_labels <- c(
  "Refusal", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Not applicable",
  "Heterosexual/Straight", "Gay/Lesbian", "Bisexual", "Other", "Prefer not to say (W9)"
)

# Harmonize missing values for sexual orientation variables
sori_vars <- c(
  "W6SexualityYP" = "sori19",  # Age 19
  "W7SexualityYP" = "sori20",  # Age 20
  "W8SEXUALITY" = "sori25",    # Age 25
  "W9SORI" = "sori32"          # Age 32
)

# Rename and harmonize sexual orientation variables
for (var in names(sori_vars)) {
  if (var %in% colnames(merged_data)) {
    merged_data[[sori_vars[var]]] <- harmonize_missing_values(merged_data[[var]])
    merged_data[[sori_vars[var]]] <- factor(
      merged_data[[sori_vars[var]]],
      levels = sori_levels,
      labels = sori_labels
    )
  }
}

# Consolidate sexual orientation variable (time-invariant)
# Convert -3 to a factor with the same levels before coalescing
merged_data$sori <- coalesce(
  merged_data$sori32,
  merged_data$sori25,
  merged_data$sori20,
  merged_data$sori19,
  factor(-3, levels = sori_levels, labels = sori_labels)
)

# Remove intermediate variables
merged_data <- merged_data %>%
  select(-c("W6SexualityYP", "W7SexualityYP", "W8SEXUALITY", "W9SORI", "sori19", "sori20", "sori25", "sori32"))

# Write output to CSV
write_csv(merged_data, "data/output/cleaned_data.csv")

# Print confirmation
message("Data cleaning and preprocessing complete. Output saved to data/output/cleaned_data.csv")

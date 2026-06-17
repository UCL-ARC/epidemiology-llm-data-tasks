
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Suppress warnings for cleaner output
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(labelled)
  library(readr)
})

# Step 1: Load all relevant datasets
wave6 <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- readr::read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave9 <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Step 2: Merge datasets by NSID
merged_data <- full_join(wave6, wave7, by = "NSID")
merged_data <- full_join(merged_data, wave8, by = "NSID")
merged_data <- full_join(merged_data, wave9, by = "NSID")

# Step 3: Define missing value mapping function
missing_value_mapping <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- NA_real_
    x[x %in% c(-999, -998, -997, -995, -100, -97)] <- -2
    x[x == -94] <- -8
    x[x == -92] <- -9
    x[x == -91] <- -1
    x[x == -99] <- -3
    x
  } else {
    x
  }
}

# Apply mapping to each variable
merged_data$W6SexualityYP <- missing_value_mapping(as.numeric(merged_data$W6SexualityYP))
merged_data$W7SexualityYP <- missing_value_mapping(as.numeric(merged_data$W7SexualityYP))
merged_data$W8SEXUALITY <- missing_value_mapping(as.numeric(merged_data$W8SEXUALITY))
merged_data$W9SORI <- missing_value_mapping(as.numeric(merged_data$W9SORI))

# Step 4: Create labelled factors
merged_data$sori19 <- factor(
  merged_data$W6SexualityYP,
  levels = c(-9, -8, -3, -2, -1, 1, 2, 3, 4),
  labels = c("Refused", "Don't know", "Not asked", "Schedule not applicable", "Not applicable", "Heterosexual / Straight", "Gay / Lesbian", "Bisexual", "Other")
)

merged_data$sori20 <- factor(
  merged_data$W7SexualityYP,
  levels = c(-9, -8, -3, -2, -1, 1, 2, 3, 4),
  labels = c("Refused", "Don't know", "Not asked", "Schedule not applicable", "Not applicable", "Heterosexual / Straight", "Gay / Lesbian", "Bisexual", "Other")
)

merged_data$sori25 <- factor(
  merged_data$W8SEXUALITY,
  levels = c(-9, -8, -3, -2, -1, 1, 2, 3, 4),
  labels = c("Refused", "Don't know", "Not asked", "Schedule not applicable", "Not applicable", "Heterosexual / Straight", "Gay / Lesbian", "Bisexual", "Other")
)

merged_data$sori32 <- factor(
  merged_data$W9SORI,
  levels = c(-9, -8, -3, -2, -1, 1, 2, 3, 4, 5),
  labels = c("Refused", "Don't know", "Not asked", "Schedule not applicable", "Not applicable", "Heterosexual / Straight", "Gay / Lesbian", "Bisexual", "Other", "Prefer not to say")
)

# Step 5: Remove raw source variables and keep only final derived variables
final_data <- merged_data %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Step 6: Write final output
readr::write_csv(final_data, "data/output/cleaned_data.csv")

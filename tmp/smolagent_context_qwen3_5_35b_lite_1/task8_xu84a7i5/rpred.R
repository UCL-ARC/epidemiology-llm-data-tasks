# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "ns8_2015_main_interview.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_main_interview.tab",
  "ns9_2022_derived_variables.tab"
)

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets
full_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(ns8_main, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

cat("Data loaded and merged. Dimensions:", nrow(full_data), "rows,", ncol(full_data), "columns\n")

# =============================================================================
# EDUCATION VARIABLES - Age 25 (Wave 8) and Age 32 (Wave 9)
# =============================================================================

# ---- EDUCATION AT AGE 25 (WAVE 8) ----
# Use W8DHANVQH from derived file
# W8DHANVQH value_labels:
#   -9: Refused, -8: Insufficient information, -1: Not applicable
#   1: NVQ Level 1, 2: NVQ Level 2, 3: NVQ Level 3, 4: NVQ Level 4, 5: NVQ Level 5
#   95: Other academic qualification, 96: None of these qualifications

educ25 <- full_data %>%
  mutate(
    educ25_raw = W8DHANVQH
  ) %>%
  mutate(
    # Map to 5-level NVQ scheme
    # Level 1 = Entry Level + Level 1
    # Level 2 = Level 2
    # Level 3 = Level 3
    # Level 4 = Level 4 + Level 5
    # Level 5 = Other + None
    educ25 = case_when(
      educ25_raw %in% c(-9, -8, -1) ~ educ25_raw,  # preserve missing codes
      educ25_raw %in% c(0, 1) ~ 1,  # Entry Level + Level 1
      educ25_raw == 2 ~ 2,  # Level 2
      educ25_raw == 3 ~ 3,  # Level 3
      educ25_raw %in% c(4, 5) ~ 4,  # Level 4 + Level 5
      educ25_raw %in% c(95, 96) ~ 5,  # Other + None
      TRUE ~ NA_real_
    )
  ) %>%
  select(NSID, educ25)

cat("educ25 created.\n")

# ---- EDUCATION AT AGE 32 (WAVE 9) ----
# Use W9DANVQH (academic) and W9DVNVQH (vocational) from derived file
# W9DANVQH value_labels:
#   -9: Refused, -8: Missing information, -1: Not applicable
#   0: NVQ Entry Level, 1: NVQ Level 1, 2: NVQ Level 2, 3: NVQ Level 3
#   4: NVQ Level 4, 5: NVQ Level 5, 95: Other academic qualification, 96: None
# W9DVNVQH value_labels:
#   -9: Refused, -8: Missing information, -1: Not applicable
#   0: NVQ Entry Level, 1: NVQ Level 1, 2: NVQ Level 2, 3: NVQ Level 3
#   4: NVQ Level 4, 5: NVQ Level 5, 95: Other vocational qualification, 96: None

# First, create harmonised educ32 (highest level from either academic or vocational)
educ32 <- full_data %>%
  mutate(
    # Get highest academic NVQ level
    academic_nvq = case_when(
      W9DANVQH %in% c(-9, -8, -1) ~ W9DANVQH,
      W9DANVQH %in% c(0, 1) ~ 1,
      W9DANVQH == 2 ~ 2,
      W9DANVQH == 3 ~ 3,
      W9DANVQH %in% c(4, 5) ~ 4,
      W9DANVQH %in% c(95, 96) ~ 5,
      TRUE ~ NA_real_
    ),
    # Get highest vocational NVQ level
    vocational_nvq = case_when(
      W9DVNVQH %in% c(-9, -8, -1) ~ W9DVNVQH,
      W9DVNVQH %in% c(0, 1) ~ 1,
      W9DVNVQH == 2 ~ 2,
      W9DVNVQH == 3 ~ 3,
      W9DVNVQH %in% c(4, 5) ~ 4,
      W9DVNVQH %in% c(95, 96) ~ 5,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    # Take maximum of academic and vocational (earlier valid first)
    # Use coalesce to get first non-missing value
    educ32 = case_when(
      !is.na(academic_nvq) & !is.na(vocational_nvq) ~ pmax(academic_nvq, vocational_nvq, na.rm = TRUE),
      !is.na(academic_nvq) ~ academic_nvq,
      !is.na(vocational_nvq) ~ vocational_nvq,
      TRUE ~ NA_real_
    )
  ) %>%
  select(NSID, educ32)

cat("educ32 created.\n")

# ---- DETAILED ACADEMIC QUALIFICATIONS AT AGE 32 (educadtl32) ----
# Use W9ACQU0A through W9ACQU0V (academic qualification indicators)
# Convert to a binary indicator (1 = Yes, 2 = No) with appropriate missing codes

# Define academic qualification variables
academic_vars <- c(
  "W9ACQU0A", "W9ACQU0B", "W9ACQU0C", "W9ACQU0D", "W9ACQU0E", "W9ACQU0F",
  "W9ACQU0G", "W9ACQU0H", "W9ACQU0I", "W9ACQU0J", "W9ACQU0K", "W9ACQU0L",
  "W9ACQU0M", "W9ACQU0N", "W9ACQU0O", "W9ACQU0P", "W9ACQU0Q", "W9ACQU0R",
  "W9ACQU0S", "W9ACQU0T", "W9ACQU0U", "W9ACQU0V"
)

# Create detailed academic qualifications variable
# This will be a collapsed/summary variable indicating presence of academic qualifications
educadtl32 <- full_data %>%
  select(NSID, all_of(academic_vars)) %>%
  mutate(
    # Count number of academic qualifications (1 = Yes)
    # Treat 2 = No, and missing codes appropriately
    num_acad_qual = rowSums(select(., all_of(academic_vars)) == 1, na.rm = TRUE),
    # Create summary: 0 = no qualifications, 1 = has qualifications
    educadtl32 = case_when(
      num_acad_qual == 0 ~ 0,
      num_acad_qual > 0 ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  select(NSID, educadtl32)

cat("educadtl32 created.\n")

# ---- DETAILED VOCATIONAL QUALIFICATIONS AT AGE 32 (educvdtl32) ----
# Use W9VCQU0A through W9VCQUAI (vocational qualification indicators)

# Define vocational qualification variables
vocational_vars <- c(
  "W9VCQU0A", "W9VCQU0B", "W9VCQU0C", "W9VCQU0D", "W9VCQU0E", "W9VCQU0F",
  "W9VCQU0G", "W9VCQU0H", "W9VCQU0I", "W9VCQU0J", "W9VCQU0K", "W9VCQU0L",
  "W9VCQU0M", "W9VCQU0N", "W9VCQU0O", "W9VCQU0P", "W9VCQU0Q", "W9VCQU0R",
  "W9VCQU0S", "W9VCQU0T", "W9VCQU0U", "W9VCQU0V", "W9VCQU0W", "W9VCQU0X",
  "W9VCQU0Y", "W9VCQU0Z", "W9VCQUAA", "W9VCQUAB", "W9VCQUAC", "W9VCQUAD",
  "W9VCQUAE", "W9VCQUAF", "W9VCQUAG", "W9VCQUAH", "W9VCQUAI"
)

# Create detailed vocational qualifications variable
educvdtl32 <- full_data %>%
  select(NSID, all_of(vocational_vars)) %>%
  mutate(
    # Count number of vocational qualifications (1 = Yes)
    num_voc_qual = rowSums(select(., all_of(vocational_vars)) == 1, na.rm = TRUE),
    # Create summary: 0 = no qualifications, 1 = has qualifications
    educvdtl32 = case_when(
      num_voc_qual == 0 ~ 0,
      num_voc_qual > 0 ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  select(NSID, educvdtl32)

cat("educvdtl32 created.\n")

# =============================================================================
# FINAL OUTPUT
# =============================================================================

# Combine all education variables
output <- educ25 %>%
  full_join(educ32, by = "NSID") %>%
  full_join(educadtl32, by = "NSID") %>%
  full_join(educvdtl32, by = "NSID")

# Check dimensions
cat("Output dimensions:", nrow(output), "rows,", ncol(output), "columns\n")
cat("Output columns:", paste(names(output), collapse = ", "), "\n")

# Write output
target_path <- "data/output/cleaned_data.csv"
readr::write_csv(output, target_path, na = "")

cat("Output written to:", target_path, "\n")
cat("Done!\n")

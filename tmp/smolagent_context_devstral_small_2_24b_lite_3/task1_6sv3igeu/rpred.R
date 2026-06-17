library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets from the metadata
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets using NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to harmonize missing values
harmonize_missing <- function(var) {
  var <- as.numeric(var)
  case_when(
    var %in% c(-92, -92.0) ~ -9,  # Refused
    var %in% c(-91, -91.0) ~ -1,  # Not applicable
    var %in% c(-99, -99.0) ~ -3,  # Not interviewed
    var %in% c(-998, -998.0, -997, -997.0, -995, -995.0) ~ -2,  # Script error/information lost
    var %in% c(-1, -1.0) ~ -8,  # Don't know
    TRUE ~ var
  )
}

# Apply harmonization to each sex variable
merged_data$W1sexYP <- harmonize_missing(merged_data$W1sexYP)
merged_data$W2SexYP <- harmonize_missing(merged_data$W2SexYP)
merged_data$W3sexYP <- harmonize_missing(merged_data$W3sexYP)
merged_data$W4SexYP <- harmonize_missing(merged_data$W4SexYP)
merged_data$W5SexYP <- harmonize_missing(merged_data$W5SexYP)
merged_data$W6Sex <- harmonize_missing(merged_data$W6Sex)
merged_data$W7Sex <- harmonize_missing(merged_data$W7Sex)
merged_data$W8CMSEX <- harmonize_missing(merged_data$W8CMSEX)
merged_data$W9DSEX <- harmonize_missing(merged_data$W9DSEX)

# Derive consolidated sex variable using most-recent-valid-first rule
# Replace missing values with NA for coalesce to work properly
merged_data$sex <- coalesce(
  ifelse(merged_data$W9DSEX %in% c(1, 2), merged_data$W9DSEX, NA_real_),
  ifelse(merged_data$W8CMSEX %in% c(1, 2), merged_data$W8CMSEX, NA_real_),
  ifelse(merged_data$W7Sex %in% c(1, 2), merged_data$W7Sex, NA_real_),
  ifelse(merged_data$W6Sex %in% c(1, 2), merged_data$W6Sex, NA_real_),
  ifelse(merged_data$W5SexYP %in% c(1, 2), merged_data$W5SexYP, NA_real_),
  ifelse(merged_data$W4SexYP %in% c(1, 2), merged_data$W4SexYP, NA_real_),
  ifelse(merged_data$W3sexYP %in% c(1, 2), merged_data$W3sexYP, NA_real_),
  ifelse(merged_data$W2SexYP %in% c(1, 2), merged_data$W2SexYP, NA_real_),
  ifelse(merged_data$W1sexYP %in% c(1, 2), merged_data$W1sexYP, NA_real_)
)

# Convert NA to -3 (Not asked)
merged_data$sex[is.na(merged_data$sex)] <- -3

# Convert to labelled factor
merged_data$sex <- factor(merged_data$sex, levels = c(1, 2, -9, -8, -7, -3, -2, -1), labels = c("Male", "Female", "Refusal", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Not applicable"))

# Select only NSID and the derived sex variable
output_data <- merged_data %>%
  select(NSID, sex)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")
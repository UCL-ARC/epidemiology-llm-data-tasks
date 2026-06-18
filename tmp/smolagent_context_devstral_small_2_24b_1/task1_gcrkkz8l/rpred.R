library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(var, metadata) {
  if (is.na(var)) {
    return(-3)
  }
  
  value_labels <- metadata$value_labels
  
  if (var %in% names(value_labels)) {
    label <- value_labels[[as.character(var)]]
    
    if (grepl("Refusal", label, ignore.case = TRUE)) {
      return(-9)
    } else if (grepl("Don't know|insufficient information", label, ignore.case = TRUE)) {
      return(-8)
    } else if (grepl("Prefer not to say", label, ignore.case = TRUE)) {
      return(-7)
    } else if (grepl("Not asked|not interviewed", label, ignore.case = TRUE)) {
      return(-3)
    } else if (grepl("Schedule not applicable|script error|information lost", label, ignore.case = TRUE)) {
      return(-2)
    } else if (grepl("Not applicable", label, ignore.case = TRUE)) {
      return(-1)
    }
  }
  
  # Check for common numeric patterns
  if (var %in% c(-999, -998, -997, -995)) {
    return(-2)
  } else if (var == -94) {
    return(-8)
  } else if (var == -92) {
    return(-9)
  } else if (var == -91) {
    return(-1)
  } else if (var == -99) {
    return(-3)
  } else if (var %in% c(-100, -97)) {
    if (var == -100) {
      return(-3)
    } else if (var == -97) {
      return(-2)
    }
  }
  
  return(var)
}

# Derive the consolidated 'sex' variable
# Start with W9DSEX (most recent)
merged_data <- merged_data %>%
  mutate(
    sex = case_when(
      !is.na(W9DSEX) & W9DSEX %in% c(1, 2) ~ W9DSEX,
      !is.na(W8CMSEX) & W8CMSEX %in% c(1, 2) ~ W8CMSEX,
      !is.na(W7Sex) & W7Sex %in% c(1, 2) ~ W7Sex,
      !is.na(W6Sex) & W6Sex %in% c(1, 2) ~ W6Sex,
      !is.na(W5SexYP) & W5SexYP %in% c(1, 2) ~ W5SexYP,
      !is.na(W4SexYP) & W4SexYP %in% c(1, 2) ~ W4SexYP,
      !is.na(W3sexYP) & W3sexYP %in% c(1, 2) ~ W3sexYP,
      !is.na(W2SexYP) & W2SexYP %in% c(1, 2) ~ W2SexYP,
      !is.na(W1sexYP) & W1sexYP %in% c(1, 2) ~ W1sexYP,
      TRUE ~ NA_real_
    )
  )

# Apply missing value mapping
merged_data$sex <- sapply(merged_data$sex, function(x) map_missing(x, wave9$variables$W9DSEX))

# Convert NA to -3
merged_data$sex[is.na(merged_data$sex)] <- -3

# Create a labelled factor for the 'sex' variable
merged_data$sex <- factor(merged_data$sex, levels = c(1, 2, -9, -8, -7, -3, -2, -1), labels = c("Male", "Female", "Refusal", "Don't know", "Prefer not to say", "Not asked / not interviewed", "Schedule not applicable / script error / information lost", "Not applicable"))

# Select only NSID and the derived 'sex' variable
output_data <- merged_data %>%
  select(NSID, sex)

# Write the output file
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"
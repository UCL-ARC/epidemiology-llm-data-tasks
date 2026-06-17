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

# Function to harmonize missing values for a given variable
harmonize_missing <- function(var, wave) {
  if (wave == "wave1") {
    var <- na_if(var, -99)
    var <- na_if(var, -92)
    var <- na_if(var, -91)
  } else if (wave == "wave2") {
    var <- na_if(var, -998)
    var <- na_if(var, -997)
    var <- na_if(var, -995)
    var <- na_if(var, -99)
    var <- na_if(var, -92)
    var <- na_if(var, -91)
    var <- na_if(var, -1)
  } else if (wave == "wave3") {
    var <- na_if(var, -99)
    var <- na_if(var, -92)
    var <- na_if(var, -91)
  } else if (wave == "wave4") {
    var <- na_if(var, -99)
    var <- na_if(var, -92)
    var <- na_if(var, -91)
    var <- na_if(var, -1)
  } else if (wave == "wave5") {
    var <- na_if(var, -1)
  } else if (wave == "wave6") {
    var <- na_if(var, -92)
    var <- na_if(var, -91)
  } else if (wave == "wave7") {
    var <- na_if(var, -91)
  } else if (wave == "wave8") {
    var <- na_if(var, -9)
    var <- na_if(var, -8)
    var <- na_if(var, -1)
  }
  return(var)
}

# Apply harmonization to each sex variable
merged_data$W1sexYP <- harmonize_missing(merged_data$W1sexYP, "wave1")
merged_data$W2SexYP <- harmonize_missing(merged_data$W2SexYP, "wave2")
merged_data$W3sexYP <- harmonize_missing(merged_data$W3sexYP, "wave3")
merged_data$W4SexYP <- harmonize_missing(merged_data$W4SexYP, "wave4")
merged_data$W5SexYP <- harmonize_missing(merged_data$W5SexYP, "wave5")
merged_data$W6Sex <- harmonize_missing(merged_data$W6Sex, "wave6")
merged_data$W7Sex <- harmonize_missing(merged_data$W7Sex, "wave7")
merged_data$W8CMSEX <- harmonize_missing(merged_data$W8CMSEX, "wave8")

# Derive the consolidated sex variable using most recent valid response first
merged_data$sex <- NA

# Use W9DSEX if available
merged_data$sex[!is.na(merged_data$W9DSEX)] <- merged_data$W9DSEX[!is.na(merged_data$W9DSEX)]

# Fall back to earlier waves if W9DSEX is missing
remaining <- is.na(merged_data$sex)
merged_data$sex[remaining & !is.na(merged_data$W8CMSEX)] <- merged_data$W8CMSEX[remaining & !is.na(merged_data$W8CMSEX)]
remaining <- is.na(merged_data$sex)
merged_data$sex[remaining & !is.na(merged_data$W7Sex)] <- merged_data$W7Sex[remaining & !is.na(merged_data$W7Sex)]
remaining <- is.na(merged_data$sex)
merged_data$sex[remaining & !is.na(merged_data$W6Sex)] <- merged_data$W6Sex[remaining & !is.na(merged_data$W6Sex)]
remaining <- is.na(merged_data$sex)
merged_data$sex[remaining & !is.na(merged_data$W5SexYP)] <- merged_data$W5SexYP[remaining & !is.na(merged_data$W5SexYP)]
remaining <- is.na(merged_data$sex)
merged_data$sex[remaining & !is.na(merged_data$W4SexYP)] <- merged_data$W4SexYP[remaining & !is.na(merged_data$W4SexYP)]
remaining <- is.na(merged_data$sex)
merged_data$sex[remaining & !is.na(merged_data$W3sexYP)] <- merged_data$W3sexYP[remaining & !is.na(merged_data$W3sexYP)]
remaining <- is.na(merged_data$sex)
merged_data$sex[remaining & !is.na(merged_data$W2SexYP)] <- merged_data$W2SexYP[remaining & !is.na(merged_data$W2SexYP)]
remaining <- is.na(merged_data$sex)
merged_data$sex[remaining & !is.na(merged_data$W1sexYP)] <- merged_data$W1sexYP[remaining & !is.na(merged_data$W1sexYP)]

# Convert NA to -3 for missing values
merged_data$sex[is.na(merged_data$sex)] <- -3

# Ensure the sex variable is numeric
merged_data$sex <- as.numeric(merged_data$sex)

# Select only NSID and the derived sex variable
output_data <- merged_data %>%
  select(NSID, sex)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"
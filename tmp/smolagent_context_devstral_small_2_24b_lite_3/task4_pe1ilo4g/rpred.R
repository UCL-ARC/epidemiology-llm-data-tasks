library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define a function to harmonize missing values based on metadata rules
harmonize_missing <- function(var) {
  var <- as.numeric(var)
  
  # Replace missing values based on metadata rules for sexual orientation variables
  # Wave 6 (W6SexualityYP): -97 → Refused, -92 → Refused, -91 → Not applicable, -1 → Don't know
  var[var == -97] <- -9  # Refused
  var[var == -92] <- -9  # Refused
  var[var == -91] <- -1  # Not applicable
  var[var == -1] <- -8   # Don't know
  
  # Wave 7 (W7SexualityYP): -100 → Refused, -97 → Refused, -92 → Refused, -91 → Not applicable, -1 → Don't know
  var[var == -100] <- -9 # Refused
  var[var == -97] <- -9  # Refused
  var[var == -92] <- -9  # Refused
  var[var == -91] <- -1  # Not applicable
  var[var == -1] <- -8   # Don't know
  
  # Wave 8 (W8SEXUALITY): -9 → Refused, -8 → Don't know, -1 → Not applicable
  var[var == -9] <- -9   # Refused
  var[var == -8] <- -8   # Don't know
  var[var == -1] <- -1   # Not applicable
  
  # Wave 9 (W9SORI): -9 → Refused, -8 → Don't know, -3 → Not asked, -1 → Not applicable, 5 → Prefer not to say
  var[var == -9] <- -9   # Refused
  var[var == -8] <- -8   # Don't know
  var[var == -3] <- -3   # Not asked
  var[var == -1] <- -1   # Not applicable
  var[var == 5] <- -7    # Prefer not to say
  
  # Replace NA values with -3 (Not asked at the fieldwork stage / not interviewed)
  var[is.na(var)] <- -3
  
  return(var)
}

# Harmonize sexual orientation variables for each wave
if ("W6SexualityYP" %in% colnames(merged_data)) {
  merged_data$W6SexualityYP <- harmonize_missing(merged_data$W6SexualityYP)
}

if ("W7SexualityYP" %in% colnames(merged_data)) {
  merged_data$W7SexualityYP <- harmonize_missing(merged_data$W7SexualityYP)
}

if ("W8SEXUALITY" %in% colnames(merged_data)) {
  merged_data$W8SEXUALITY <- harmonize_missing(merged_data$W8SEXUALITY)
}

if ("W9SORI" %in% colnames(merged_data)) {
  merged_data$W9SORI <- harmonize_missing(merged_data$W9SORI)
}

# Create derived variables for sexual orientation at each age
merged_data$sori19 <- ifelse(merged_data$W6SexualityYP %in% c(1, 2, 3, 4), 
                             merged_data$W6SexualityYP, 
                             ifelse(merged_data$W6SexualityYP < 0, merged_data$W6SexualityYP, -3))

merged_data$sori20 <- ifelse(merged_data$W7SexualityYP %in% c(1, 2, 3, 4), 
                             merged_data$W7SexualityYP, 
                             ifelse(merged_data$W7SexualityYP < 0, merged_data$W7SexualityYP, -3))

merged_data$sori25 <- ifelse(merged_data$W8SEXUALITY %in% c(1, 2, 3, 4), 
                             merged_data$W8SEXUALITY, 
                             ifelse(merged_data$W8SEXUALITY < 0, merged_data$W8SEXUALITY, -3))

merged_data$sori32 <- ifelse(merged_data$W9SORI %in% c(1, 2, 3, 4), 
                             merged_data$W9SORI, 
                             ifelse(merged_data$W9SORI < 0, merged_data$W9SORI, -3))

# Convert derived variables to labelled factors
merged_data$sori19 <- labelled::to_factor(merged_data$sori19, 
                                          labels = c("Heterosexual / Straight" = 1, 
                                                     "Gay / Lesbian" = 2, 
                                                     "Bisexual" = 3, 
                                                     "Other" = 4,
                                                     "Refusal" = -9,
                                                     "Don't know / insufficient information" = -8,
                                                     "Prefer not to say" = -7,
                                                     "Not asked at the fieldwork stage / not interviewed" = -3,
                                                     "Schedule not applicable / script error / information lost" = -2,
                                                     "Item not applicable" = -1))

merged_data$sori20 <- labelled::to_factor(merged_data$sori20, 
                                          labels = c("Heterosexual / Straight" = 1, 
                                                     "Gay / Lesbian" = 2, 
                                                     "Bisexual" = 3, 
                                                     "Other" = 4,
                                                     "Refusal" = -9,
                                                     "Don't know / insufficient information" = -8,
                                                     "Prefer not to say" = -7,
                                                     "Not asked at the fieldwork stage / not interviewed" = -3,
                                                     "Schedule not applicable / script error / information lost" = -2,
                                                     "Item not applicable" = -1))

merged_data$sori25 <- labelled::to_factor(merged_data$sori25, 
                                          labels = c("Heterosexual / Straight" = 1, 
                                                     "Gay / Lesbian" = 2, 
                                                     "Bisexual" = 3, 
                                                     "Other" = 4,
                                                     "Refusal" = -9,
                                                     "Don't know / insufficient information" = -8,
                                                     "Prefer not to say" = -7,
                                                     "Not asked at the fieldwork stage / not interviewed" = -3,
                                                     "Schedule not applicable / script error / information lost" = -2,
                                                     "Item not applicable" = -1))

merged_data$sori32 <- labelled::to_factor(merged_data$sori32, 
                                          labels = c("Heterosexual / Straight" = 1, 
                                                     "Gay / Lesbian" = 2, 
                                                     "Bisexual" = 3, 
                                                     "Other" = 4,
                                                     "Refusal" = -9,
                                                     "Don't know / insufficient information" = -8,
                                                     "Prefer not to say" = -7,
                                                     "Not asked at the fieldwork stage / not interviewed" = -3,
                                                     "Schedule not applicable / script error / information lost" = -2,
                                                     "Item not applicable" = -1))

# Select only the ID variable and derived variables for output
output_data <- merged_data %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write the output CSV file
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"
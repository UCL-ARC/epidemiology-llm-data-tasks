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
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to map missing values to standard codes
harmonise_missing <- function(var, wave) {
  if (is.null(var)) {
    return(rep(-3, length(merged_data$NSID)))
  }
  
  # Initialize output
  out <- var
  
  # Map missing values based on metadata
  if (wave == 1) {
    out[var %in% c(-99, -97, -96, -92, -91, -1)] <- -3
    out[var == -92] <- -9
    out[var == -91] <- -1
    out[var == -1] <- -8
  } else if (wave == 2) {
    out[var %in% c(-998, -997, -995, -99, -97, -96, -92, -91, -1)] <- -2
    out[var == -92] <- -9
    out[var == -91] <- -1
    out[var == -1] <- -8
  } else if (wave == 3) {
    out[var %in% c(-99, -97, -96, -92, -91, -1)] <- -3
    out[var == -92] <- -9
    out[var == -91] <- -1
    out[var == -1] <- -8
  } else if (wave == 4) {
    out[var %in% c(-99, -97, -96, -92, -91, -1)] <- -3
    out[var == -92] <- -9
    out[var == -91] <- -1
    out[var == -1] <- -8
  } else if (wave == 6) {
    out[var %in% c(-997, -97, -92, -91, -1)] <- -2
    out[var == -92] <- -9
    out[var == -91] <- -1
    out[var == -1] <- -8
  } else if (wave == 7) {
    out[var %in% c(-996, -97, -92, -91, -1)] <- -2
    out[var == -92] <- -9
    out[var == -91] <- -1
    out[var == -1] <- -8
  } else if (wave == 8) {
    out[var %in% c(-9, -8, -3, -1)] <- -3
    out[var == -9] <- -9
    out[var == -8] <- -8
    out[var == -3] <- -3
    out[var == -1] <- -1
  } else if (wave == 9) {
    out[var %in% c(-9, -8, -3, -1)] <- -3
    out[var == -9] <- -9
    out[var == -8] <- -8
    out[var == -3] <- -3
    out[var == -1] <- -1
  }
  
  return(out)
}

# Apply missing value harmonisation
merged_data$W1alceverYP <- harmonise_missing(merged_data$W1alceverYP, 1)
merged_data$W1alcmonYP <- harmonise_missing(merged_data$W1alcmonYP, 1)
merged_data$W2alceverYP <- harmonise_missing(merged_data$W2alceverYP, 2)
merged_data$W3alceverYP <- harmonise_missing(merged_data$W3alceverYP, 3)
merged_data$W4AlcEverYP <- harmonise_missing(merged_data$W4AlcEverYP, 4)
merged_data$W6AlcEverYP <- harmonise_missing(merged_data$W6AlcEverYP, 6)
merged_data$W7AlcEverYP <- harmonise_missing(merged_data$W7AlcEverYP, 7)
merged_data$W8AUDIT1 <- harmonise_missing(merged_data$W8AUDIT1, 8)
merged_data$W9AUDIT1 <- harmonise_missing(merged_data$W9AUDIT1, 9)

# Derive drinking indicators for each sweep
merged_data$drink14 <- ifelse(
  (merged_data$W1alceverYP == 1 & merged_data$W1alcmonYP == 1), 1, 0
)
merged_data$drink15 <- ifelse(merged_data$W2alceverYP == 1, 1, 0)
merged_data$drink16 <- ifelse(merged_data$W3alceverYP == 1, 1, 0)
merged_data$drink17 <- ifelse(merged_data$W4AlcEverYP == 1, 1, 0)
merged_data$drink19 <- ifelse(merged_data$W6AlcEverYP == 1, 1, 0)
merged_data$drink20 <- ifelse(merged_data$W7AlcEverYP == 1, 1, 0)
merged_data$drink25 <- ifelse(merged_data$W8AUDIT1 > 1, 1, 0)
merged_data$drink32 <- ifelse(merged_data$W9AUDIT1 > 1, 1, 0)

# Replace missing values in drinking indicators
merged_data$drink14[is.na(merged_data$drink14)] <- -3
merged_data$drink15[is.na(merged_data$drink15)] <- -3
merged_data$drink16[is.na(merged_data$drink16)] <- -3
merged_data$drink17[is.na(merged_data$drink17)] <- -3
merged_data$drink19[is.na(merged_data$drink19)] <- -3
merged_data$drink20[is.na(merged_data$drink20)] <- -3
merged_data$drink25[is.na(merged_data$drink25)] <- -3
merged_data$drink32[is.na(merged_data$drink32)] <- -3

# Derive alcfst variable
merged_data$alcfst <- NA

for (i in 1:nrow(merged_data)) {
  drink_vals <- c(
    if (!is.na(merged_data$drink14[i])) merged_data$drink14[i] else NA,
    if (!is.na(merged_data$drink15[i])) merged_data$drink15[i] else NA,
    if (!is.na(merged_data$drink16[i])) merged_data$drink16[i] else NA,
    if (!is.na(merged_data$drink17[i])) merged_data$drink17[i] else NA,
    if (!is.na(merged_data$drink19[i])) merged_data$drink19[i] else NA,
    if (!is.na(merged_data$drink20[i])) merged_data$drink20[i] else NA,
    if (!is.na(merged_data$drink25[i])) merged_data$drink25[i] else NA,
    if (!is.na(merged_data$drink32[i])) merged_data$drink32[i] else NA
  )
  
  drink_ages <- c(14, 15, 16, 17, 19, 20, 25, 32)
  
  valid_drink_vals <- drink_vals[!is.na(drink_vals)]
  valid_drink_ages <- drink_ages[!is.na(drink_vals)]
  
  if (length(valid_drink_vals) == 0) {
    merged_data$alcfst[i] <- -3
  } else if (all(valid_drink_vals == 0)) {
    if (any(drink_vals %in% c(-9, -8, -7, -3, -2, -1))) {
      merged_data$alcfst[i] <- -8
    } else {
      merged_data$alcfst[i] <- 99
    }
  } else {
    earliest_drink_age <- min(valid_drink_ages[valid_drink_vals == 1])
    merged_data$alcfst[i] <- earliest_drink_age
  }
}

# Convert alcfst to factor with appropriate levels and labels
merged_data$alcfst <- factor(
  merged_data$alcfst,
  levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
  labels = c(
    "Age 14", "Age 15", "Age 16", "Age 17", "Age 19", "Age 20", "Age 25", "Age 32",
    "Never had alcohol", "Don't know/insufficient information"
  )
)

# Select only NSID and alcfst for output
output_data <- merged_data %>%
  select(NSID, alcfst)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")

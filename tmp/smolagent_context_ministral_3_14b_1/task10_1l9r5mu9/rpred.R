
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(readr)

# Load all files
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_five <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave_nine <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets by NSID, ensuring all required columns are included
merged_data <- full_join(wave_one, wave_four, by = "NSID") %>%
  full_join(wave_five, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Ensure columns exist before processing
if (!"W4empsYP" %in% colnames(merged_data)) {
  merged_data$W4empsYP <- NA
}
if (!"W5mainactYP" %in% colnames(merged_data)) {
  merged_data$W5mainactYP <- NA
}
if (!"W6TCurrentAct" %in% colnames(merged_data)) {
  merged_data$W6TCurrentAct <- NA
}
if (!"W7TCurrentAct" %in% colnames(merged_data)) {
  merged_data$W7TCurrentAct <- NA
}
if (!"W8DACTIVITYC" %in% colnames(merged_data)) {
  merged_data$W8DACTIVITYC <- NA
}
if (!"W9DACTIVITYC" %in% colnames(merged_data)) {
  merged_data$W9DACTIVITYC <- NA
}

# Create collapsed variables using case_when
merged_data <- merged_data %>%
  mutate(
    ecoact17 = case_when(
      W4empsYP %in% c(1, 2) ~ 1,
      W4empsYP == 4 ~ 2,
      W4empsYP == 5 ~ 3,
      W4empsYP == 3 ~ 4,
      W4empsYP == 6 ~ 5,
      W4empsYP %in% c(7, 8, 9) ~ 6,
      W4empsYP %in% c(-999, -94, -92, -91, -9, -8, -2, -1, -7, -99) ~
        case_when(
          W4empsYP %in% c(-92, -9) ~ -9,
          W4empsYP %in% c(-94, -8) ~ -8,
          W4empsYP %in% c(-91, -1) ~ -1,
          W4empsYP %in% c(-999, -2, -99) ~ -2,
          TRUE ~ -3
        ),
      is.na(W4empsYP) ~ -3,
      TRUE ~ -2
    ),
    ecoact18 = case_when(
      W5mainactYP %in% c(1, 3, 5, 6) ~ 1,
      W5mainactYP == 4 ~ 3,
      W5mainactYP == 7 ~ 4,
      W5mainactYP == 8 ~ 5,
      W5mainactYP %in% c(2, 9, 10, 11) ~ 6,
      W5mainactYP %in% c(-999, -94, -92, -91, -9, -8, -2, -1, -7, -99) ~
        case_when(
          W5mainactYP %in% c(-92, -9) ~ -9,
          W5mainactYP %in% c(-94, -8) ~ -8,
          W5mainactYP %in% c(-91, -1) ~ -1,
          W5mainactYP %in% c(-999, -2, -99) ~ -2,
          TRUE ~ -3
        ),
      is.na(W5mainactYP) ~ -3,
      TRUE ~ -2
    ),
    ecoact19 = case_when(
      W6TCurrentAct %in% c(3, 10) ~ 1,
      W6TCurrentAct == 5 ~ 2,
      W6TCurrentAct == 4 ~ 2,
      W6TCurrentAct == 2 ~ 3,
      W6TCurrentAct == 8 ~ 4,
      W6TCurrentAct == 7 ~ 5,
      W6TCurrentAct %in% c(1, 6, 9, 11) ~ 6,
      W6TCurrentAct %in% c(-999, -94, -92, -91, -9, -8, -2, -1, -7, -99) ~
        case_when(
          W6TCurrentAct %in% c(-92, -9) ~ -9,
          W6TCurrentAct %in% c(-94, -8) ~ -8,
          W6TCurrentAct %in% c(-91, -1) ~ -1,
          W6TCurrentAct %in% c(-999, -2, -99) ~ -2,
          TRUE ~ -3
        ),
      is.na(W6TCurrentAct) ~ -3,
      TRUE ~ -2
    ),
    ecoact20 = case_when(
      W7TCurrentAct %in% c(3, 9) ~ 1,
      W7TCurrentAct == 5 ~ 2,
      W7TCurrentAct == 4 ~ 2,
      W7TCurrentAct %in% c(1, 2) ~ 3,
      W7TCurrentAct == 8 ~ 4,
      W7TCurrentAct == 7 ~ 5,
      W7TCurrentAct %in% c(6, 10, 11, 12, 13, 14, 15) ~ 6,
      W7TCurrentAct %in% c(-999, -94, -92, -91, -9, -8, -2, -1, -7, -99) ~
        case_when(
          W7TCurrentAct %in% c(-92, -9) ~ -9,
          W7TCurrentAct %in% c(-94, -8) ~ -8,
          W7TCurrentAct %in% c(-91, -1) ~ -1,
          W7TCurrentAct %in% c(-999, -2, -99) ~ -2,
          TRUE ~ -3
        ),
      is.na(W7TCurrentAct) ~ -3,
      TRUE ~ -2
    ),
    ecoact25 = case_when(
      W8DACTIVITYC %in% c(1, 2, 3) ~ 1,
      W8DACTIVITYC == 7 ~ 2,
      W8DACTIVITYC == 6 ~ 2,
      W8DACTIVITYC == 5 ~ 3,
      W8DACTIVITYC == 4 ~ 4,
      W8DACTIVITYC == 9 ~ 5,
      W8DACTIVITYC %in% c(8, 10) ~ 6,
      W8DACTIVITYC %in% c(-9, -8, -1) ~
        case_when(
          W8DACTIVITYC %in% c(-92, -9) ~ -9,
          W8DACTIVITYC %in% c(-94, -8) ~ -8,
          W8DACTIVITYC %in% c(-91, -1) ~ -1,
          TRUE ~ -3
        ),
      is.na(W8DACTIVITYC) ~ -3,
      TRUE ~ -2
    ),
    ecoact32 = case_when(
      W9DACTIVITYC %in% c(1, 2, 3) ~ 1,
      W9DACTIVITYC == 7 ~ 2,
      W9DACTIVITYC == 6 ~ 2,
      W9DACTIVITYC == 5 ~ 3,
      W9DACTIVITYC == 4 ~ 4,
      W9DACTIVITYC == 9 ~ 5,
      W9DACTIVITYC %in% c(8, 10) ~ 6,
      W9DACTIVITYC %in% c(-9, -8, -1) ~
        case_when(
          W9DACTIVITYC %in% c(-92, -9) ~ -9,
          W9DACTIVITYC %in% c(-94, -8) ~ -8,
          W9DACTIVITYC %in% c(-91, -1) ~ -1,
          TRUE ~ -3
        ),
      is.na(W9DACTIVITYC) ~ -3,
      TRUE ~ -2
    )
  )

# Create detailed variables for waves 8 and 9
merged_data <- merged_data %>%
  mutate(
    ecoactadu25 = case_when(
      W8DACTIVITYC == 1 ~ 1,
      W8DACTIVITYC == 2 ~ 2,
      W8DACTIVITYC == 3 ~ 3,
      W8DACTIVITYC == 4 ~ 4,
      W8DACTIVITYC == 5 ~ 5,
      W8DACTIVITYC == 6 ~ 6,
      W8DACTIVITYC == 7 ~ 7,
      W8DACTIVITYC == 8 ~ 8,
      W8DACTIVITYC == 9 ~ 9,
      W8DACTIVITYC == 10 ~ 10,
      W8DACTIVITYC %in% c(-9, -8, -1) ~
        case_when(
          W8DACTIVITYC %in% c(-92, -9) ~ -9,
          W8DACTIVITYC %in% c(-94, -8) ~ -8,
          W8DACTIVITYC %in% c(-91, -1) ~ -1,
          TRUE ~ -3
        ),
      is.na(W8DACTIVITYC) ~ -3,
      TRUE ~ -2
    ),
    ecoactadu32 = case_when(
      W9DACTIVITYC == 1 ~ 1,
      W9DACTIVITYC == 2 ~ 2,
      W9DACTIVITYC == 3 ~ 3,
      W9DACTIVITYC == 4 ~ 4,
      W9DACTIVITYC == 5 ~ 5,
      W9DACTIVITYC == 6 ~ 6,
      W9DACTIVITYC == 7 ~ 7,
      W9DACTIVITYC == 8 ~ 8,
      W9DACTIVITYC == 9 ~ 9,
      W9DACTIVITYC == 10 ~ 10,
      W9DACTIVITYC %in% c(-9, -8, -1) ~
        case_when(
          W9DACTIVITYC %in% c(-92, -9) ~ -9,
          W9DACTIVITYC %in% c(-94, -8) ~ -8,
          W9DACTIVITYC %in% c(-91, -1) ~ -1,
          TRUE ~ -3
        ),
      is.na(W9DACTIVITYC) ~ -3,
      TRUE ~ -2
    )
  )

# Define labels
collapsed_labels <- list(
  ecoact17 = c("1" = "In paid work", "2" = "Apprenticeship / government training scheme / training", "3" = "Education", "4" = "Unemployed", "5" = "Looking after home / family", "6" = "Other"),
  ecoact18 = c("1" = "In paid work", "2" = "Apprenticeship / government training scheme / training", "3" = "Education", "4" = "Unemployed", "5" = "Looking after home / family", "6" = "Other"),
  ecoact19 = c("1" = "In paid work", "2" = "Apprenticeship / government training scheme / training", "3" = "Education", "4" = "Unemployed", "5" = "Looking after home / family", "6" = "Other"),
  ecoact20 = c("1" = "In paid work", "2" = "Apprenticeship / government training scheme / training", "3" = "Education", "4" = "Unemployed", "5" = "Looking after home / family", "6" = "Other"),
  ecoact25 = c("1" = "In paid work", "2" = "Apprenticeship / government training scheme / training", "3" = "Education", "4" = "Unemployed", "5" = "Looking after home / family", "6" = "Other"),
  ecoact32 = c("1" = "In paid work", "2" = "Apprenticeship / government training scheme / training", "3" = "Education", "4" = "Unemployed", "5" = "Looking after home / family", "6" = "Other")
)

detailed_labels <- list(
  ecoactadu25 = c("1" = "Employee - in paid work", "2" = "Self employed", "3" = "In unpaid/voluntary work", "4" = "Unemployed", "5" = "Education: School/college/university", "6" = "Apprenticeship", "7" = "On gov't scheme for employment training", "8" = "Sick or disabled", "9" = "Looking after home or family", "10" = "Something else"),
  ecoactadu32 = c("1" = "Employee - in paid work", "2" = "Self employed", "3" = "In unpaid/voluntary work", "4" = "Unemployed", "5" = "Education: School/college/university", "6" = "Apprenticeship", "7" = "On gov't scheme for employment training", "8" = "Sick or disabled", "9" = "Looking after home or family", "10" = "Something else")
)

# Convert to factors with labels
for (var in c("ecoact17", "ecoact18", "ecoact19", "ecoact20", "ecoact25", "ecoact32")) {
  merged_data[[var]] <- factor(merged_data[[var]], levels = 1:6, labels = collapsed_labels[[var]])
}

for (var in c("ecoactadu25", "ecoactadu32")) {
  merged_data[[var]] <- factor(merged_data[[var]], levels = 1:10, labels = detailed_labels[[var]])
}

# Select only the required variables
final_data <- merged_data %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Ensure the output directory exists
if (!dir.exists("data/output")) {
  dir.create("data/output")
}

# Write the final output
write_csv(final_data, "data/output/cleaned_data.csv")

# Check if file was written
file_exists <- file.exists("data/output/cleaned_data.csv")
cat("File written:", file_exists)

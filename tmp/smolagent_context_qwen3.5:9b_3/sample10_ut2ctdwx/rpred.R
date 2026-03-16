library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Load all datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_five <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Wave 4 (Age 17) recoding - convert to standard missing codes
code_4 <- function(x) {
  x[is.na(x)] <- -3  # NA to -3
  x[x == -999 | x == -998 | x == -997 | x == -995] <- -2  # -999, etc to -2
  x[x == -94] <- -8  # -94 to -8
  x[x == -92] <- -9  # -92 to -9
  x[x == -91] <- -3  # -91 to -3
  x
}

# Wave 5 (Age 18) recoding
code_5 <- function(x) {
  x[is.na(x)] <- -3
  x[x == -999 | x == -998 | x == -997 | x == -995] <- -2
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -3
  x[x == -99] <- -3
  x
}

# Wave 6 (Age 19) recoding
code_6 <- function(x) {
  x[is.na(x)] <- -3
  x[x == -999 | x == -998 | x == -997 | x == -995] <- -2
  x[x == -91] <- -3
  x[x == -94] <- -8
  x[x == -92] <- -9
  x
}

# Wave 7 (Age 20) recoding
code_7 <- function(x) {
  x[is.na(x)] <- -3
  x[x == -999 | x == -998 | x == -997 | x == -995] <- -2
  x[x == -91] <- -3
  x
}

# Wave 8 (Age 25) recoding
code_8 <- function(x) {
  x[is.na(x)] <- -3
  x[x == -999 | x == -998 | x == -997 | x == -995] <- -2
  x
}

# Wave 9 (Age 32) recoding
code_9 <- function(x) {
  x[is.na(x)] <- -3
  x[x == -999 | x == -998 | x == -997 | x == -995] <- -2
  x
}

# Apply recoding
wave_four <- wave_four %>% mutate(W4empsYP = code_4(W4empsYP))
wave_five <- wave_five %>% mutate(W5mainactYP = code_5(W5mainactYP))
wave_six <- wave_six %>% mutate(W6TCurrentAct = code_6(W6TCurrentAct))
wave_seven <- wave_seven %>% mutate(W7TCurrentAct = code_7(W7TCurrentAct))
ns8 <- ns8 %>% mutate(W8DACTIVITYC = code_8(W8DACTIVITYC))
ns9 <- ns9 %>% mutate(W9DACTIVITYC = code_9(W9DACTIVITYC))

# Merge waves by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_five, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Create harmonized age-specific variables (17-20)
# ecoact17: Age 17 employment status
merged_data <- merged_data %>%
  mutate(
    ecoact17 = case_when(
      W4empsYP == -9 ~ -9,
      W4empsYP == -8 ~ -8,
      W4empsYP == -3 ~ -3,
      W4empsYP == -2 ~ -2,
      W4empsYP %in% c(1, 2) ~ 1,
      W4empsYP == 3 ~ 3,
      W4empsYP == 4 ~ 4,
      W4empsYP %in% c(5, 6, 10, 11) ~ 5,
      W4empsYP == 7 ~ 7,
      W4empsYP == 8 ~ 8,
      W4empsYP %in% c(9, 12, 13, 14, 15) ~ 9,
      TRUE ~ W4empsYP
    )
  )

# ecoact18: Age 18 main activity
merged_data <- merged_data %>%
  mutate(
    ecoact18 = case_when(
      W5mainactYP == -9 ~ -9,
      W5mainactYP == -8 ~ -8,
      W5mainactYP == -3 ~ -3,
      W5mainactYP == -2 ~ -2,
      W5mainactYP == 1 ~ 1,
      W5mainactYP == 2 ~ 2,
      W5mainactYP == 3 ~ 3,
      W5mainactYP %in% c(4, 5) ~ 4,
      W5mainactYP %in% c(6, 7) ~ 6,
      W5mainactYP %in% c(8, 9, 10, 11) ~ 7,
      TRUE ~ W5mainactYP
    )
  )

# ecoact19: Age 19 main activity
merged_data <- merged_data %>%
  mutate(
    ecoact19 = case_when(
      W6TCurrentAct == -9 ~ -9,
      W6TCurrentAct == -8 ~ -8,
      W6TCurrentAct == -3 ~ -3,
      W6TCurrentAct == -2 ~ -2,
      W6TCurrentAct == 1 ~ 1,
      W6TCurrentAct %in% c(2, 10) ~ 2,
      W6TCurrentAct == 3 ~ 3,
      W6TCurrentAct == 4 ~ 4,
      W6TCurrentAct == 5 ~ 1,
      W6TCurrentAct == 7 ~ 7,
      W6TCurrentAct == 8 ~ 8,
      W6TCurrentAct == 11 ~ 5,
      W6TCurrentAct %in% c(6, 9) ~ 6,
      TRUE ~ W6TCurrentAct
    )
  )

# ecoact20: Age 20 current activity
merged_data <- merged_data %>%
  mutate(
    ecoact20 = case_when(
      W7TCurrentAct == -9 ~ -9,
      W7TCurrentAct == -8 ~ -8,
      W7TCurrentAct == -3 ~ -3,
      W7TCurrentAct == -2 ~ -2,
      W7TCurrentAct == 1 ~ 1,
      W7TCurrentAct %in% c(2, 3) ~ 2,
      W7TCurrentAct %in% c(4, 11) ~ 4,
      W7TCurrentAct == 5 ~ 1,
      W7TCurrentAct == 6 ~ 6,
      W7TCurrentAct %in% c(7, 9) ~ 7,
      W7TCurrentAct == 8 ~ 8,
      W7TCurrentAct == 10 ~ 5,
      W7TCurrentAct == 12 ~ 9,
      W7TCurrentAct == 13 ~ 9,
      W7TCurrentAct == 14 ~ 8,
      W7TCurrentAct == 15 ~ 9,
      TRUE ~ W7TCurrentAct
    )
  )

# Create detailed adult variables (ages 25 and 32)
# ecoactadu25: Age 25 detailed activity
merged_data <- merged_data %>%
  mutate(
    ecoactadu25 = case_when(
      W8DACTIVITYC == -9 ~ -9,
      W8DACTIVITYC == -8 ~ -8,
      W8DACTIVITYC == -1 ~ -1,
      W8DACTIVITYC == -2 ~ -2,
      W8DACTIVITYC == 1 ~ 1,
      W8DACTIVITYC == 2 ~ 2,
      W8DACTIVITYC == 3 ~ 3,
      W8DACTIVITYC == 4 ~ 4,
      W8DACTIVITYC == 5 ~ 5,
      W8DACTIVITYC == 6 ~ 6,
      W8DACTIVITYC == 7 ~ 6,
      W8DACTIVITYC == 8 ~ 8,
      W8DACTIVITYC == 9 ~ 9,
      W8DACTIVITYC == 10 ~ 10,
      TRUE ~ W8DACTIVITYC
    )
  )

# ecoactadu32: Age 32 detailed activity
merged_data <- merged_data %>%
  mutate(
    ecoactadu32 = case_when(
      W9DACTIVITYC == -9 ~ -9,
      W9DACTIVITYC == -8 ~ -8,
      W9DACTIVITYC == -1 ~ -1,
      W9DACTIVITYC == -2 ~ -2,
      W9DACTIVITYC == 1 ~ 1,
      W9DACTIVITYC == 2 ~ 2,
      W9DACTIVITYC == 3 ~ 3,
      W9DACTIVITYC == 4 ~ 4,
      W9DACTIVITYC == 5 ~ 5,
      W9DACTIVITYC == 6 ~ 6,
      W9DACTIVITYC == 7 ~ 6,
      W9DACTIVITYC == 8 ~ 8,
      W9DACTIVITYC == 9 ~ 9,
      W9DACTIVITYC == 10 ~ 10,
      TRUE ~ W9DACTIVITYC
    )
  )

# Create collapsed versions from detailed variables
# ecoact25: 6-category collapsed version
merged_data <- merged_data %>%
  mutate(
    ecoact25 = case_when(
      W8DACTIVITYC == -9 ~ -9,
      W8DACTIVITYC == -8 ~ -8,
      W8DACTIVITYC == -1 ~ -1,
      W8DACTIVITYC == -2 ~ -2,
      W8DACTIVITYC %in% c(1, 3, 7) ~ 1,
      W8DACTIVITYC %in% c(2, 4) ~ 2,
      W8DACTIVITYC %in% c(5, 6) ~ 3,
      W8DACTIVITYC %in% c(8, 9) ~ 4,
      W8DACTIVITYC %in% c(10) ~ 5,
      TRUE ~ W8DACTIVITYC
    )
  )

# ecoact32: 6-category collapsed version
merged_data <- merged_data %>%
  mutate(
    ecoact32 = case_when(
      W9DACTIVITYC == -9 ~ -9,
      W9DACTIVITYC == -8 ~ -8,
      W9DACTIVITYC == -1 ~ -1,
      W9DACTIVITYC == -2 ~ -2,
      W9DACTIVITYC %in% c(1, 3, 7) ~ 1,
      W9DACTIVITYC %in% c(2, 4) ~ 2,
      W9DACTIVITYC %in% c(5, 6) ~ 3,
      W9DACTIVITYC %in% c(8, 9) ~ 4,
      W9DACTIVITYC %in% c(10) ~ 5,
      TRUE ~ W9DACTIVITYC
    )
  )

# Convert harmonized variables to factors with correct label counts
merged_data <- merged_data %>% mutate(
  ecoact17 = factor(ecoact17, 
    levels = c(-9, -8, -3, -2, 1, 3, 4, 5, 7, 8, 9),
    labels = c("Refused", "Insufficient", "Not asked", "Script error", "Paid work", "Unemployed", "Training", "Education", "Sick/disabled", "Looking after family", "Other")),
  ecoact18 = factor(ecoact18, 
    levels = c(-9, -8, -3, -2, 1, 2, 3, 4, 6, 7, 5),
    labels = c("Refused", "Insufficient", "Not asked", "Script error", "Apprenticeship", "College/employer", "Paid work", "Education", "Looking after family", "Waiting", "Employment scheme")),
  ecoact19 = factor(ecoact19, 
    levels = c(-9, -8, -3, -2, 1, 2, 3, 4, 5, 6, 7, 8),
    labels = c("Refused", "Insufficient", "Not asked", "Script error", "University", "Education", "Paid work", "Training", "Employment scheme", "Education/college", "Looking after family", "Unemployed")),
  ecoact20 = factor(ecoact20, 
    levels = c(-9, -8, -3, -2, 1, 2, 3, 4, 5, 6, 7, 8, 9),
    labels = c("Refused", "Insufficient", "Not asked", "Script error", "University", "Education", "Training", "Apprenticeship", "Looking after family", "Unemployed", "Voluntary work", "Other", "Ill/disabled")),
  ecoact25 = factor(ecoact25, 
    levels = c(-9, -8, -1, -2, 1, 2, 3, 4, 5, 9, 10),
    labels = c("Refused", "Insufficient", "Not applicable", "Script error", "Paid work", "Self-employed", "Voluntary work", "Unemployed", "Education", "Looking after family", "Other")),
  ecoact32 = factor(ecoact32, 
    levels = c(-9, -8, -1, -2, 1, 2, 3, 4, 5, 9, 10),
    labels = c("Refused", "Insufficient", "Not applicable", "Script error", "Paid work", "Self-employed", "Voluntary work", "Unemployed", "Education", "Looking after family", "Other")),
  ecoactadu25 = factor(ecoactadu25, 
    levels = c(-9, -8, -1, -2, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    labels = c("Refused", "Insufficient", "Not applicable", "Script error", "Employee", "Self-employed", "Voluntary work", "Unemployed", "Education", "Apprenticeship", "Govt scheme", "Sick/disabled", "Looking after family", "Other")),
  ecoactadu32 = factor(ecoactadu32, 
    levels = c(-9, -8, -1, -2, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    labels = c("Refused", "Insufficient", "Not applicable", "Script error", "Employee", "Self-employed", "Voluntary work", "Unemployed", "Education", "Apprenticeship", "Govt scheme", "Sick/disabled", "Looking after family", "Other"))
)

# Select variables for output
final_data <- merged_data %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")

# Print summary
cat("Output file created successfully\n")
cat("Number of observations:", nrow(final_data), "\n")
cat("Number of variables:", ncol(final_data), "\n")
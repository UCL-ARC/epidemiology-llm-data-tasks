# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load all datasets explicitly by name from metadata
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets by NSID using full_join
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to standardize missing value codes
# -9 = Refusal
# -8 = Don't know/insufficient information
# -1 = Item not applicable
# -3 = Not asked at the fieldwork stage/participated/interviewed
# Any Null value coded as -3

# Create standardized sex variables for each wave with age-specific suffixes
merged_data <- merged_data %>%
  mutate(
    # Age 14 (Wave 1): W1sexYP (-99=YP not interviewed, -92=Refused, -91=Not applicable)
    sex14 = case_when(
      W1sexYP %in% c(1, 2) ~ as.numeric(W1sexYP),
      W1sexYP == -92 ~ -9,  # Refused
      W1sexYP == -91 ~ -1,  # Not applicable
      W1sexYP == -99 ~ -3,  # YP not interviewed -> Not asked
      is.na(W1sexYP) ~ -3,  # Null -> Not asked
      TRUE ~ as.numeric(W1sexYP)
    ),
    
    # Age 15 (Wave 2): W2SexYP (-998=Interviewer missed, -997=Script error, -995=Missing history, -99=YP not interviewed, -92=Refused, -91=Not applicable, -1=Don't Know)
    sex15 = case_when(
      W2SexYP %in% c(1, 2) ~ as.numeric(W2SexYP),
      W2SexYP == -92 ~ -9,  # Refused
      W2SexYP == -91 ~ -1,  # Not applicable
      W2SexYP %in% c(-998, -997, -995, -99) ~ -3,  # Various not interviewed codes -> Not asked
      W2SexYP == -1 ~ -8,  # Don't know
      is.na(W2SexYP) ~ -3,  # Null -> Not asked
      TRUE ~ as.numeric(W2SexYP)
    ),
    
    # Age 16 (Wave 3): W3sexYP (-99=YP not interviewed, -92=Refused, -91=Not applicable)
    sex16 = case_when(
      W3sexYP %in% c(1, 2) ~ as.numeric(W3sexYP),
      W3sexYP == -92 ~ -9,  # Refused
      W3sexYP == -91 ~ -1,  # Not applicable
      W3sexYP == -99 ~ -3,  # YP not interviewed -> Not asked
      is.na(W3sexYP) ~ -3,  # Null -> Not asked
      TRUE ~ as.numeric(W3sexYP)
    ),
    
    # Age 17 (Wave 4): W4SexYP (-99=YP not interviewed, -92=Refused, -91=Not applicable, -1=Don't know)
    sex17 = case_when(
      W4SexYP %in% c(1, 2) ~ as.numeric(W4SexYP),
      W4SexYP == -92 ~ -9,  # Refused
      W4SexYP == -91 ~ -1,  # Not applicable
      W4SexYP == -99 ~ -3,  # YP not interviewed -> Not asked
      W4SexYP == -1 ~ -8,  # Don't know
      is.na(W4SexYP) ~ -3,  # Null -> Not asked
      TRUE ~ as.numeric(W4SexYP)
    ),
    
    # Age 18 (Wave 5): W5SexYP (-1=Don't know)
    sex18 = case_when(
      W5SexYP %in% c(1, 2) ~ as.numeric(W5SexYP),
      W5SexYP == -1 ~ -8,  # Don't know
      is.na(W5SexYP) ~ -3,  # Null -> Not asked
      TRUE ~ as.numeric(W5SexYP)
    ),
    
    # Age 19 (Wave 6): W6Sex (-92=Refused, -91=Not applicable)
    sex19 = case_when(
      W6Sex %in% c(1, 2) ~ as.numeric(W6Sex),
      W6Sex == -92 ~ -9,  # Refused
      W6Sex == -91 ~ -1,  # Not applicable
      is.na(W6Sex) ~ -3,  # Null -> Not asked
      TRUE ~ as.numeric(W6Sex)
    ),
    
    # Age 20 (Wave 7): W7Sex (-91=Not applicable)
    sex20 = case_when(
      W7Sex %in% c(1, 2) ~ as.numeric(W7Sex),
      W7Sex == -91 ~ -1,  # Not applicable
      is.na(W7Sex) ~ -3,  # Null -> Not asked
      TRUE ~ as.numeric(W7Sex)
    ),
    
    # Age ~25-30 (Wave 8): W8CMSEX (-9=Refused, -8=Don't know, -1=Not applicable)
    sex25 = case_when(
      W8CMSEX %in% c(1, 2) ~ as.numeric(W8CMSEX),
      W8CMSEX == -9 ~ -9,  # Refused
      W8CMSEX == -8 ~ -8,  # Don't know
      W8CMSEX == -1 ~ -1,  # Not applicable
      is.na(W8CMSEX) ~ -3,  # Null -> Not asked
      TRUE ~ as.numeric(W8CMSEX)
    ),
    
    # Age 32 (Wave 9): W9DSEX (no missing codes in metadata)
    sex32 = case_when(
      W9DSEX %in% c(1, 2) ~ as.numeric(W9DSEX),
      is.na(W9DSEX) ~ -3,  # Null -> Not asked
      TRUE ~ as.numeric(W9DSEX)
    )
  )

# Create consolidated sex variable (time-invariant characteristic)
# Prioritize most recent valid response (wave 9), fall back to earlier waves
# Only use positive values (1, 2) in prioritization logic
merged_data <- merged_data %>%
  mutate(
    sex = case_when(
      sex32 %in% c(1, 2) ~ sex32,
      sex25 %in% c(1, 2) ~ sex25,
      sex20 %in% c(1, 2) ~ sex20,
      sex19 %in% c(1, 2) ~ sex19,
      sex18 %in% c(1, 2) ~ sex18,
      sex17 %in% c(1, 2) ~ sex17,
      sex16 %in% c(1, 2) ~ sex16,
      sex15 %in% c(1, 2) ~ sex15,
      sex14 %in% c(1, 2) ~ sex14,
      TRUE ~ -3  # No valid response in any wave
    )
  )

# Convert sex variables to factors with explicit labels
merged_data <- merged_data %>%
  mutate(
    sex = factor(sex, levels = c(-9, -8, -3, -1, 1, 2),
                 labels = c("Refused", "Don't know", "Not asked", "Not applicable", "Male", "Female")),
    sex14 = factor(sex14, levels = c(-9, -8, -3, -1, 1, 2),
                   labels = c("Refused", "Don't know", "Not asked", "Not applicable", "Male", "Female")),
    sex15 = factor(sex15, levels = c(-9, -8, -3, -1, 1, 2),
                   labels = c("Refused", "Don't know", "Not asked", "Not applicable", "Male", "Female")),
    sex16 = factor(sex16, levels = c(-9, -8, -3, -1, 1, 2),
                   labels = c("Refused", "Don't know", "Not asked", "Not applicable", "Male", "Female")),
    sex17 = factor(sex17, levels = c(-9, -8, -3, -1, 1, 2),
                   labels = c("Refused", "Don't know", "Not asked", "Not applicable", "Male", "Female")),
    sex18 = factor(sex18, levels = c(-9, -8, -3, -1, 1, 2),
                   labels = c("Refused", "Don't know", "Not asked", "Not applicable", "Male", "Female")),
    sex19 = factor(sex19, levels = c(-9, -8, -3, -1, 1, 2),
                   labels = c("Refused", "Don't know", "Not asked", "Not applicable", "Male", "Female")),
    sex20 = factor(sex20, levels = c(-9, -8, -3, -1, 1, 2),
                   labels = c("Refused", "Don't know", "Not asked", "Not applicable", "Male", "Female")),
    sex25 = factor(sex25, levels = c(-9, -8, -3, -1, 1, 2),
                   labels = c("Refused", "Don't know", "Not asked", "Not applicable", "Male", "Female")),
    sex32 = factor(sex32, levels = c(-9, -8, -3, -1, 1, 2),
                   labels = c("Refused", "Don't know", "Not asked", "Not applicable", "Male", "Female"))
  )

# Select final variables for output (ID and derived variables only)
final_data <- merged_data %>%
  select(NSID, sex, sex14, sex15, sex16, sex17, sex18, sex19, sex20, sex25, sex32)

# Write output CSV
write_csv(final_data, "data/output/cleaned_data.csv")

# Print summary
cat("Data cleaning complete.\n")
cat("Output file: data/output/cleaned_data.csv\n")
cat("Number of observations:", nrow(final_data), "\n")
cat("Number of variables:", ncol(final_data), "\n")
cat("\nVariable summary:\n")
print(str(final_data))
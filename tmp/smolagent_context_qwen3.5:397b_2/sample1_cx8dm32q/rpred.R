# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load all wave files using read_delim with tab delimiter
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
ns8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
ns9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID using full_join
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Create harmonized sex variable (time-invariant characteristic)
# Prioritize most recent valid response (ns9_2022), fall back to earlier waves
cleaned_data <- merged_data %>%
  mutate(
    # Convert each wave's sex variable to standard codes
    # Wave 1 (Age 14): -99=not interviewed, -92=refused, -91=not applicable
    sex_w1 = case_when(
      is.na(W1sexYP) ~ -3,
      W1sexYP == -99 ~ -3,
      W1sexYP == -92 ~ -9,
      W1sexYP == -91 ~ -1,
      W1sexYP > 0 ~ as.numeric(W1sexYP),
      TRUE ~ -3
    ),
    
    # Wave 2 (Age 15): -99=not interviewed, -92=refused, -91=not applicable, -1=don't know
    # Also -998, -997, -995 are interviewer/script errors
    sex_w2 = case_when(
      is.na(W2SexYP) ~ -3,
      W2SexYP == -99 ~ -3,
      W2SexYP %in% c(-998, -997, -995) ~ -3,
      W2SexYP == -92 ~ -9,
      W2SexYP == -91 ~ -1,
      W2SexYP == -1 ~ -8,
      W2SexYP > 0 ~ as.numeric(W2SexYP),
      TRUE ~ -3
    ),
    
    # Wave 3 (Age 16): -99=not interviewed, -92=refused, -91=not applicable
    sex_w3 = case_when(
      is.na(W3sexYP) ~ -3,
      W3sexYP == -99 ~ -3,
      W3sexYP == -92 ~ -9,
      W3sexYP == -91 ~ -1,
      W3sexYP > 0 ~ as.numeric(W3sexYP),
      TRUE ~ -3
    ),
    
    # Wave 4 (Age 17): -99=not interviewed, -92=refused, -91=not applicable, -1=don't know
    sex_w4 = case_when(
      is.na(W4SexYP) ~ -3,
      W4SexYP == -99 ~ -3,
      W4SexYP == -92 ~ -9,
      W4SexYP == -91 ~ -1,
      W4SexYP == -1 ~ -8,
      W4SexYP > 0 ~ as.numeric(W4SexYP),
      TRUE ~ -3
    ),
    
    # Wave 5 (Age 18): -1=don't know (no other missing codes mentioned)
    sex_w5 = case_when(
      is.na(W5SexYP) ~ -3,
      W5SexYP == -1 ~ -8,
      W5SexYP > 0 ~ as.numeric(W5SexYP),
      TRUE ~ -3
    ),
    
    # Wave 6 (Age 19): -92=refused, -91=not applicable
    sex_w6 = case_when(
      is.na(W6Sex) ~ -3,
      W6Sex == -92 ~ -9,
      W6Sex == -91 ~ -1,
      W6Sex > 0 ~ as.numeric(W6Sex),
      TRUE ~ -3
    ),
    
    # Wave 7 (Age 20): -91=not applicable
    sex_w7 = case_when(
      is.na(W7Sex) ~ -3,
      W7Sex == -91 ~ -1,
      W7Sex > 0 ~ as.numeric(W7Sex),
      TRUE ~ -3
    ),
    
    # Wave 8 (Age ~25): -9=refused, -8=don't know, -1=not applicable
    sex_w8 = case_when(
      is.na(W8CMSEX) ~ -3,
      W8CMSEX == -9 ~ -9,
      W8CMSEX == -8 ~ -8,
      W8CMSEX == -1 ~ -1,
      W8CMSEX > 0 ~ as.numeric(W8CMSEX),
      TRUE ~ -3
    ),
    
    # Wave 9 (Age 32): No missing codes in metadata, only 1=Male, 2=Female
    sex_w9 = case_when(
      is.na(W9DSEX) ~ -3,
      W9DSEX > 0 ~ as.numeric(W9DSEX),
      TRUE ~ -3
    ),
    
    # Create harmonized sex variable - prioritize most recent valid (positive) response
    sex = case_when(
      !is.na(sex_w9) & sex_w9 > 0 ~ sex_w9,
      !is.na(sex_w8) & sex_w8 > 0 ~ sex_w8,
      !is.na(sex_w7) & sex_w7 > 0 ~ sex_w7,
      !is.na(sex_w6) & sex_w6 > 0 ~ sex_w6,
      !is.na(sex_w5) & sex_w5 > 0 ~ sex_w5,
      !is.na(sex_w4) & sex_w4 > 0 ~ sex_w4,
      !is.na(sex_w3) & sex_w3 > 0 ~ sex_w3,
      !is.na(sex_w2) & sex_w2 > 0 ~ sex_w2,
      !is.na(sex_w1) & sex_w1 > 0 ~ sex_w1,
      TRUE ~ -3
    ),
    
    # Create age-specific sex variables with proper naming
    sex14 = sex_w1,
    sex15 = sex_w2,
    sex16 = sex_w3,
    sex17 = sex_w4,
    sex18 = sex_w5,
    sex19 = sex_w6,
    sex20 = sex_w7,
    sex32 = sex_w9
  )

# Select only ID and derived variables for output
final_data <- cleaned_data %>%
  select(NSID, sex, sex14, sex15, sex16, sex17, sex18, sex19, sex20, sex32)

# Apply factor labels for sex variable
final_data <- final_data %>%
  mutate(
    sex = factor(sex, levels = c(-9, -8, -1, -3, 1, 2),
                 labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Male", "Female")),
    sex14 = factor(sex14, levels = c(-9, -8, -1, -3, 1, 2),
                   labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Male", "Female")),
    sex15 = factor(sex15, levels = c(-9, -8, -1, -3, 1, 2),
                   labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Male", "Female")),
    sex16 = factor(sex16, levels = c(-9, -8, -1, -3, 1, 2),
                   labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Male", "Female")),
    sex17 = factor(sex17, levels = c(-9, -8, -1, -3, 1, 2),
                   labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Male", "Female")),
    sex18 = factor(sex18, levels = c(-9, -8, -1, -3, 1, 2),
                   labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Male", "Female")),
    sex19 = factor(sex19, levels = c(-9, -8, -1, -3, 1, 2),
                   labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Male", "Female")),
    sex20 = factor(sex20, levels = c(-9, -8, -1, -3, 1, 2),
                   labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Male", "Female")),
    sex32 = factor(sex32, levels = c(-9, -8, -1, -3, 1, 2),
                   labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Male", "Female"))
  )

# Write output to CSV
write_csv(final_data, "data/output/cleaned_data.csv")

# Print summary
cat("Data cleaning complete.\n")
cat("Number of observations:", nrow(final_data), "\n")
cat("Number of variables:", ncol(final_data), "\n")
cat("Output saved to: data/output/cleaned_data.csv\n")
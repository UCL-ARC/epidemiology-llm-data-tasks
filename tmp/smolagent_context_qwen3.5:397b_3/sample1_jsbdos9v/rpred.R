library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Function to standardize missing value codes
standardize_missing <- function(x, wave_specific_codes) {
  # Convert wave-specific codes to standard codes
  # -9 = Refusal, -8 = Don't know, -1 = Not applicable, -3 = Not asked
  
  # First, replace any NA with -3 (not asked)
  x[is.na(x)] <- -3
  
  # Apply wave-specific mappings
  for (code in names(wave_specific_codes)) {
    code_num <- as.numeric(code)
    standard_code <- wave_specific_codes[[code]]
    x[x == code_num] <- standard_code
  }
  
  return(x)
}

# Load all datasets
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
w9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID using full_join
data <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w3, by = "NSID") %>%
  full_join(w4, by = "NSID") %>%
  full_join(w5, by = "NSID") %>%
  full_join(w6, by = "NSID") %>%
  full_join(w7, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9, by = "NSID")

# Standardize sex variables across waves
# Wave 1 (Age 14): W1sexYP - -99=not interviewed, -92=refused, -91=not applicable, 1=Male, 2=Female
# Map: -99 -> -3 (not asked), -92 -> -9 (refusal), -91 -> -1 (not applicable)
data <- data %>%
  mutate(
    sex14 = case_when(
      W1sexYP == -99 ~ -3,
      W1sexYP == -92 ~ -9,
      W1sexYP == -91 ~ -1,
      W1sexYP %in% c(1, 2) ~ W1sexYP,
      is.na(W1sexYP) ~ -3,
      TRUE ~ W1sexYP
    )
  )

# Wave 2 (Age 15): W2SexYP - -998=interviewer missed, -997=script error, -995=missing history, -99=not interviewed, -92=refused, -91=not applicable, -1=don't know, 1=Male, 2=Female
# Map: -998,-997,-995,-99 -> -3 (not asked), -92 -> -9 (refusal), -91 -> -1 (not applicable), -1 -> -8 (don't know)
data <- data %>%
  mutate(
    sex15 = case_when(
      W2SexYP %in% c(-998, -997, -995, -99) ~ -3,
      W2SexYP == -92 ~ -9,
      W2SexYP == -91 ~ -1,
      W2SexYP == -1 ~ -8,
      W2SexYP %in% c(1, 2) ~ W2SexYP,
      is.na(W2SexYP) ~ -3,
      TRUE ~ W2SexYP
    )
  )

# Wave 3 (Age 16): W3sexYP - -99=not interviewed, -92=refused, -91=not applicable, 1=Male, 2=Female
data <- data %>%
  mutate(
    sex16 = case_when(
      W3sexYP == -99 ~ -3,
      W3sexYP == -92 ~ -9,
      W3sexYP == -91 ~ -1,
      W3sexYP %in% c(1, 2) ~ W3sexYP,
      is.na(W3sexYP) ~ -3,
      TRUE ~ W3sexYP
    )
  )

# Wave 4 (Age 17): W4SexYP - -99=not interviewed, -92=refused, -91=not applicable, -1=don't know, 1=Male, 2=Female
data <- data %>%
  mutate(
    sex17 = case_when(
      W4SexYP == -99 ~ -3,
      W4SexYP == -92 ~ -9,
      W4SexYP == -91 ~ -1,
      W4SexYP == -1 ~ -8,
      W4SexYP %in% c(1, 2) ~ W4SexYP,
      is.na(W4SexYP) ~ -3,
      TRUE ~ W4SexYP
    )
  )

# Wave 5 (Age 18): W5SexYP - -1=don't know, 1=Male, 2=Female
data <- data %>%
  mutate(
    sex18 = case_when(
      W5SexYP == -1 ~ -8,
      W5SexYP %in% c(1, 2) ~ W5SexYP,
      is.na(W5SexYP) ~ -3,
      TRUE ~ W5SexYP
    )
  )

# Wave 6 (Age 19): W6Sex - -92=refused, -91=not applicable, 1=Male, 2=Female
data <- data %>%
  mutate(
    sex19 = case_when(
      W6Sex == -92 ~ -9,
      W6Sex == -91 ~ -1,
      W6Sex %in% c(1, 2) ~ W6Sex,
      is.na(W6Sex) ~ -3,
      TRUE ~ W6Sex
    )
  )

# Wave 7 (Age 20): W7Sex - -91=not applicable, 1=Male, 2=Female
data <- data %>%
  mutate(
    sex20 = case_when(
      W7Sex == -91 ~ -1,
      W7Sex %in% c(1, 2) ~ W7Sex,
      is.na(W7Sex) ~ -3,
      TRUE ~ W7Sex
    )
  )

# Wave 8 (Age ~25): W8CMSEX - -9=refused, -8=don't know, -1=not applicable, 1=Male, 2=Female
data <- data %>%
  mutate(
    sex25 = case_when(
      W8CMSEX == -9 ~ -9,
      W8CMSEX == -8 ~ -8,
      W8CMSEX == -1 ~ -1,
      W8CMSEX %in% c(1, 2) ~ W8CMSEX,
      is.na(W8CMSEX) ~ -3,
      TRUE ~ W8CMSEX
    )
  )

# Wave 9 (Age 32): W9DSEX - 1=Male, 2=Female (no missing codes in metadata)
data <- data %>%
  mutate(
    sex32 = case_when(
      W9DSEX %in% c(1, 2) ~ W9DSEX,
      is.na(W9DSEX) ~ -3,
      TRUE ~ W9DSEX
    )
  )

# Create consolidated sex variable (time-invariant)
# Prioritize most recent valid response (sex32 -> sex25 -> sex20 -> ... -> sex14)
# Only use positive values (1, 2) for valid responses
data <- data %>%
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
      TRUE ~ -3  # All missing
    )
  )

# Create boost variable from wave 4 (age 17)
data <- data %>%
  mutate(
    boost17 = case_when(
      W4Boost == 1 ~ 1,
      W4Boost == 2 ~ 2,
      is.na(W4Boost) ~ -3,
      TRUE ~ W4Boost
    )
  )

# Select only the ID and derived variables for output
cleaned_data <- data %>%
  select(
    NSID,
    sex,
    sex14, sex15, sex16, sex17, sex18, sex19, sex20, sex25, sex32,
    boost17
  )

# Apply factor labels to sex variables
labelled::set_value_labels(cleaned_data$sex, c(`Male` = 1, `Female` = 2, `Not applicable` = -1, `Don't know` = -8, `Refused` = -9, `Not asked` = -3))

# Write output
write_csv(cleaned_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(cleaned_data), "\n")
cat("Number of columns:", ncol(cleaned_data), "\n")

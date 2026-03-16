library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load all four wave files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Rename language variables to standard format (lowercase with wave suffix)
wave1 <- wave1 %>% rename(lang_S1 = W1englangYP)
wave2 <- wave2 %>% rename(lang_S2 = W2EnglangYP)
wave3 <- wave3 %>% rename(lang_S3 = W3englangHH)
wave4 <- wave4 %>% rename(lang_S4 = W4EngLangHH)

# Keep only NSID and language variable from each wave
wave1 <- wave1 %>% select(NSID, lang_S1)
wave2 <- wave2 %>% select(NSID, lang_S2)
wave3 <- wave3 %>% select(NSID, lang_S3)
wave4 <- wave4 %>% select(NSID, lang_S4)

# Full join all waves by NSID
data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Function to harmonize missing value codes for each wave
harmonize_missing_wave1 <- function(x) {
  case_when(
    x == -99 ~ -3,  # YP not interviewed -> Not asked
    x == -92 ~ -9,  # Refused -> Refusal
    x == -91 ~ -1,  # Not applicable -> Item not applicable
    x == -1 ~ -8,   # Don't know -> Don't know/insufficient information
    x %in% c(-999, -998, -997, -995, -94) ~ -2,  # Schedule not applicable
    TRUE ~ x
  )
}

harmonize_missing_wave2 <- function(x) {
  case_when(
    x %in% c(-998, -997, -995, -99) ~ -3,  # Interviewer missed/Script error/Missing history/YP not interviewed -> Not asked
    x == -92 ~ -9,  # Refused -> Refusal
    x == -91 ~ -1,  # Not applicable -> Item not applicable
    x == -1 ~ -8,   # Don't know -> Don't know/insufficient information
    x %in% c(-999, -94) ~ -2,  # Schedule not applicable
    TRUE ~ x
  )
}

harmonize_missing_wave3 <- function(x) {
  case_when(
    x == -999 ~ -2,  # HH grid missing -> Schedule not applicable
    x %in% c(-997, -99) ~ -3,  # Script Error/blank -> Not asked
    x == -92 ~ -9,  # Refused -> Refusal
    x == -91 ~ -1,  # Not applicable -> Item not applicable
    x == -1 ~ -8,   # Don't know -> Don't know/insufficient information
    x %in% c(-998, -995, -94) ~ -2,  # Schedule not applicable
    TRUE ~ x
  )
}

harmonize_missing_wave4 <- function(x) {
  case_when(
    x == -999 ~ -2,  # Missing household grid -> Schedule not applicable
    x == -997 ~ -3,  # Script error -> Not asked
    x == -92 ~ -9,  # Refused -> Refusal
    x == -91 ~ -1,  # Not applicable -> Item not applicable
    x == -1 ~ -8,   # Don't know -> Don't know/insufficient information
    x %in% c(-998, -995, -99, -94) ~ -2,  # Schedule not applicable
    TRUE ~ x
  )
}

# Apply missing value harmonization to each wave
data <- data %>%
  mutate(
    lang_S1 = harmonize_missing_wave1(lang_S1),
    lang_S2 = harmonize_missing_wave2(lang_S2),
    lang_S3 = harmonize_missing_wave3(lang_S3),
    lang_S4 = harmonize_missing_wave4(lang_S4)
  )

# Convert any NA values to -3 (Not asked/interview not conducted)
data <- data %>%
  mutate(
    lang_S1 = ifelse(is.na(lang_S1), -3, lang_S1),
    lang_S2 = ifelse(is.na(lang_S2), -3, lang_S2),
    lang_S3 = ifelse(is.na(lang_S3), -3, lang_S3),
    lang_S4 = ifelse(is.na(lang_S4), -3, lang_S4)
  )

# Create consolidated language variable using two-step prioritization:
# 1. First, prioritize earliest valid POSITIVE value (values > 0)
# 2. If no positive values exist, use earliest available missing code as fallback
data <- data %>%
  mutate(
    lang = case_when(
      !is.na(lang_S1) & lang_S1 > 0 ~ lang_S1,
      !is.na(lang_S2) & lang_S2 > 0 ~ lang_S2,
      !is.na(lang_S3) & lang_S3 > 0 ~ lang_S3,
      !is.na(lang_S4) & lang_S4 > 0 ~ lang_S4,
      !is.na(lang_S1) ~ lang_S1,
      !is.na(lang_S2) ~ lang_S2,
      !is.na(lang_S3) ~ lang_S3,
      !is.na(lang_S4) ~ lang_S4,
      TRUE ~ -3
    )
  )

# Create factor with labels for the consolidated variable
data <- data %>%
  mutate(
    lang = factor(lang, 
                  levels = c(-9, -8, -3, -2, -1, 1, 2, 3, 4),
                  labels = c("Refusal", "Don't know/insufficient information", 
                             "Not asked/interview not conducted", "Schedule not applicable",
                             "Item not applicable", "Yes - English only", 
                             "Yes - English first/main and speaks other languages",
                             "No - another language is first/main language", 
                             "Bilingual"))
  )

# Select only ID and consolidated variables for output
output_data <- data %>% select(NSID, lang)

# Write to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Print summary
cat("Data cleaning complete.\n")
cat("Number of observations:", nrow(output_data), "\n")
cat("Number of variables:", ncol(output_data), "\n")
cat("Variables:", paste(names(output_data), collapse = ", "), "\n")
cat("Output saved to: data/output/cleaned_data.csv\n")
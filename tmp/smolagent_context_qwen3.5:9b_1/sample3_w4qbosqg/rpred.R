library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols())
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", col_types = cols())
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = cols())
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = cols())

# Merge all datasets by NSID
combined_data <- full_join(wave1, wave2, by = "NSID")
combined_data <- full_join(combined_data, wave3, by = "NSID")
combined_data <- full_join(combined_data, wave4, by = "NSID")

# Function to recode missing values according to standard codes
recode_missing <- function(data, var_name) {
  data <- data %>%
    mutate(
      !!var_name := case_when(
        is.na(!!var_name) ~ as.integer(-3),
        !is.na(!!var_name) & !!var_name >= -999 & !!var_name <= -995 ~ as.integer(-2),
        !is.na(!!var_name) & !!var_name == -99 ~ as.integer(-3),
        !is.na(!!var_name) & !!var_name == -92 ~ as.integer(-9),
        !is.na(!!var_name) & !!var_name == -91 ~ as.integer(-1),
        !is.na(!!var_name) & !!var_name == -1 ~ as.integer(-8),
        TRUE ~ as.integer(!!var_name)
      )
    )
  return(data)
}

# Apply recoding to each language variable
combined_data <- recode_missing(combined_data, "W1englangYP")
combined_data <- recode_missing(combined_data, "W2EnglangYP")
combined_data <- recode_missing(combined_data, "W3englangHH")
combined_data <- recode_missing(combined_data, "W4EngLangHH")

# Consolidate language variables - prioritize earliest valid POSITIVE value
combined_data <- combined_data %>%
  mutate(
    lang = case_when(
      !is.na(W1englangYP) & W1englangYP > 0 ~ W1englangYP,
      !is.na(W2EnglangYP) & W2EnglangYP > 0 ~ W2EnglangYP,
      !is.na(W3englangHH) & W3englangHH > 0 ~ W3englangHH,
      !is.na(W4EngLangHH) & W4EngLangHH > 0 ~ W4EngLangHH,
      !is.na(W1englangYP) ~ W1englangYP,
      !is.na(W2EnglangYP) ~ W2EnglangYP,
      !is.na(W3englangHH) ~ W3englangHH,
      !is.na(W4EngLangHH) ~ W4EngLangHH,
      TRUE ~ as.integer(-3)
    )
  )

# Create factor variable with explicit labels for consolidated lang
combined_data <- combined_data %>%
  mutate(
    lang = factor(lang,
                  levels = c(-9, -8, -1, -3, 1, 2, 3, 4),
                  labels = c("Refused", "Don't know/insufficient information", "Not applicable", "Not asked/interview not conducted",
                           "Yes - English only", "Yes - English first/main and speaks other languages",
                           "No - another language is household's first/main language", "Household is bilingual"))
  )

# Keep only NSID and consolidated language variable for output
output_data <- combined_data %>%
  select(NSID, lang)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")

# Print summary
cat("Data loaded and cleaned successfully!\n")
cat("Total rows:", nrow(output_data), "\n")
cat("Total columns:", ncol(output_data), "\n")
cat("Variables:", paste(names(output_data), collapse = ", "), "\n")
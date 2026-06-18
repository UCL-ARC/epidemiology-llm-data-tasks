library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Function to harmonize missing values
harmonize_missing <- function(var) {
  case_when(
    var %in% c(-92, -92.0) ~ -9,  # Refusal
    var %in% c(-1, -1.0) ~ -8,    # Don't know / insufficient information
    var %in% c(-91, -91.0) ~ -7,  # Prefer not to say
    var %in% c(-99, -99.0) ~ -3,  # Not asked at the fieldwork stage / not interviewed
    var %in% c(-999, -999.0, -998, -998.0, -997, -997.0, -995, -995.0) ~ -2,  # Schedule not applicable / script error / information lost
    TRUE ~ var
  )
}

# Harmonize missing values for language variables
merged_data <- merged_data %>%
  mutate(
    W1englangYP = harmonize_missing(W1englangYP),
    W2EnglangYP = harmonize_missing(W2EnglangYP),
    W3englangHH = harmonize_missing(W3englangHH),
    W4EngLangHH = harmonize_missing(W4EngLangHH)
  )

# Consolidate language information into a single variable 'lang'
# Priority: Use earliest valid response
merged_data <- merged_data %>%
  mutate(
    lang = case_when(
      !is.na(W1englangYP) & W1englangYP > 0 ~ W1englangYP,
      !is.na(W2EnglangYP) & W2EnglangYP > 0 ~ W2EnglangYP,
      !is.na(W3englangHH) & W3englangHH > 0 ~ W3englangHH,
      !is.na(W4EngLangHH) & W4EngLangHH > 0 ~ W4EngLangHH,
      # If no valid response, use the most informative missing code
      W1englangYP < 0 ~ W1englangYP,
      W2EnglangYP < 0 ~ W2EnglangYP,
      W3englangHH < 0 ~ W3englangHH,
      W4EngLangHH < 0 ~ W4EngLangHH,
      TRUE ~ -3  # Default missing code
    )
  )

# Convert 'lang' to a labelled factor
lang_labels <- c(
  "1" = "Yes - English only",
  "2" = "Yes - English first/main and speaks other languages",
  "3" = "No, another language is first or main",
  "4" = "Bilingual",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

merged_data$lang <- factor(merged_data$lang, levels = c(1, 2, 3, 4, -9, -8, -7, -3, -2, -1), labels = lang_labels)

# Select only NSID and the final derived variable 'lang'
output_data <- merged_data %>%
  select(NSID, lang)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"
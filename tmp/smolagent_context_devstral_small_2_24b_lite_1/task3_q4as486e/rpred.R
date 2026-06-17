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

# Function to map missing values to standard codes
map_missing <- function(x) {
  case_when(
    x %in% c(-92, -92.0) ~ -9,  # Refusal
    x %in% c(-1, -1.0) ~ -8,    # Don't know / insufficient information
    x %in% c(-91, -91.0) ~ -7,  # Prefer not to say
    x %in% c(-99, -99.0, -999, -999.0, -998, -998.0, -997, -997.0, -995, -995.0) ~ -3,  # Not asked / not interviewed / script error
    x %in% c(-2, -2.0) ~ -2,    # Schedule not applicable
    TRUE ~ x
  )
}

# Harmonize missing values for language variables
merged_data <- merged_data %>%
  mutate(
    W1englangYP = map_missing(W1englangYP),
    W2EnglangYP = map_missing(W2EnglangYP),
    W3englangHH = map_missing(W3englangHH),
    W4EngLangHH = map_missing(W4EngLangHH)
  )

# Derive consolidated language variable (lang)
# Prioritize earliest valid response
merged_data <- merged_data %>%
  mutate(
    lang = case_when(
      !is.na(W1englangYP) & W1englangYP > 0 ~ W1englangYP,
      !is.na(W2EnglangYP) & W2EnglangYP > 0 ~ W2EnglangYP,
      !is.na(W3englangHH) & W3englangHH > 0 ~ W3englangHH,
      !is.na(W4EngLangHH) & W4EngLangHH > 0 ~ W4EngLangHH,
      TRUE ~ -3  # Default to -3 if no valid response
    )
  )

# Convert lang to labelled factor
lang_labels <- c(
  "1" = "Yes - English only",
  "2" = "Yes - English first/main and speaks other languages",
  "3" = "No - another language is first/main language",
  "4" = "Bilingual",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked / not interviewed / script error",
  "-2" = "Schedule not applicable"
)

merged_data$lang <- factor(merged_data$lang, levels = c(1, 2, 3, 4, -9, -8, -7, -3, -2), labels = lang_labels)

# Select only NSID and lang for output
output_data <- merged_data %>%
  select(NSID, lang)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")

# Return summary
cat("Output written to data/output/cleaned_data.csv\n")
cat("Summary of lang variable:\n")
print(table(output_data$lang))

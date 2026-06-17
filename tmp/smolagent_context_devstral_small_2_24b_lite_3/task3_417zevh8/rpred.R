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

# Merge datasets using NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(x) {
  case_when(
    x %in% c(-92, -92.0) ~ -9,  # Refusal
    x %in% c(-1, -1.0) ~ -8,    # Don't know / insufficient information
    x %in% c(-91, -91.0) ~ -1,  # Not applicable
    x %in% c(-99, -99.0, -999, -999.0, -998, -998.0, -997, -997.0, -995, -995.0) ~ -2,  # Schedule not applicable / script error / information lost
    TRUE ~ x
  )
}

# Harmonize language variables
# Wave 1: W1englangYP
merged_data <- merged_data %>%
  mutate(W1englangYP_clean = map_missing(W1englangYP))

# Wave 2: W2EnglangYP
merged_data <- merged_data %>%
  mutate(W2EnglangYP_clean = map_missing(W2EnglangYP))

# Wave 3: W3englangHH
merged_data <- merged_data %>%
  mutate(W3englangHH_clean = map_missing(W3englangHH))

# Wave 4: W4EngLangHH
merged_data <- merged_data %>%
  mutate(W4EngLangHH_clean = map_missing(W4EngLangHH))

# Consolidate language information
# Priority: Use earliest valid response
merged_data <- merged_data %>%
  mutate(lang = case_when(
    !is.na(W1englangYP_clean) & W1englangYP_clean > 0 ~ W1englangYP_clean,
    !is.na(W2EnglangYP_clean) & W2EnglangYP_clean > 0 ~ W2EnglangYP_clean,
    !is.na(W3englangHH_clean) & W3englangHH_clean > 0 ~ W3englangHH_clean,
    !is.na(W4EngLangHH_clean) & W4EngLangHH_clean > 0 ~ W4EngLangHH_clean,
    TRUE ~ -3  # Not interviewed / no valid response
  ))

# Define labels for the consolidated variable
lang_labels <- c(
  "1.0" = "Yes - English only",
  "2.0" = "Yes - English first/main and speaks other languages",
  "3.0" = "No - another language is first/main language",
  "4.0" = "Bilingual",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-1" = "Not applicable",
  "-2" = "Schedule not applicable / script error / information lost",
  "-3" = "Not interviewed / no valid response"
)

# Convert to labelled factor
merged_data$lang <- factor(merged_data$lang, levels = as.numeric(names(lang_labels)), labels = lang_labels)

# Select only NSID and the consolidated variable
output_data <- merged_data %>%
  select(NSID, lang)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from data/input/ with tab delimiter
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge all files by NSID
merged <- full_join(w1, w2, by = "NSID")
merged <- full_join(merged, w3, by = "NSID")
merged <- full_join(merged, w4, by = "NSID")

# Helper function to map missing codes to standard codes
# Based on label meaning from metadata
map_missing_codes <- function(x) {
  x[x == -99] <- -3
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  x[x == -999] <- -2
  x[x == -998] <- -2
  x[x == -997] <- -2
  x[x == -995] <- -2
  return(x)
}

# Apply mapping to all language variables
merged$W1englangYP_clean <- map_missing_codes(merged$W1englangYP)
merged$W2EnglangYP_clean <- map_missing_codes(merged$W2EnglangYP)
merged$W3englangHH_clean <- map_missing_codes(merged$W3englangHH)
merged$W4EngLangHH_clean <- map_missing_codes(merged$W4EngLangHH)

# Derive consolidated lang variable using earliest valid positive response first
# Valid substantive values are 1, 2, 3, 4
merged$lang <- case_when(
  merged$W1englangYP_clean %in% c(1, 2, 3, 4) ~ merged$W1englangYP_clean,
  merged$W2EnglangYP_clean %in% c(1, 2, 3, 4) ~ merged$W2EnglangYP_clean,
  merged$W3englangHH_clean %in% c(1, 2, 3, 4) ~ merged$W3englangHH_clean,
  merged$W4EngLangHH_clean %in% c(1, 2, 3, 4) ~ merged$W4EngLangHH_clean,
  TRUE ~ -3
)

# Create labelled factor for lang variable with proper ordering
lang_labels <- c(
  "1" = "Yes - English only",
  "2" = "Yes - English first/ main and speaks other languages",
  "3" = "No, another language is respondent's first or main language",
  "4" = "Respondent is bilingual",
  "-1" = "Item not applicable",
  "-2" = "Schedule not applicable / script error / information lost",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-8" = "Don't know / insufficient information"
)

merged$lang <- factor(merged$lang, 
                      levels = c(1, 2, 3, 4, -1, -2, -3, -8),
                      labels = lang_labels)

# Keep only final derived variables
output <- merged %>%
  select(NSID, lang)

# Write output to data/output/cleaned_data.csv
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(output, "data/output/cleaned_data.csv")

# Print summary
cat("Output dimensions:", nrow(output), "rows,", ncol(output), "columns\n")
cat("Summary of lang variable:\n")
print(table(output$lang, useNA = "ifany"))

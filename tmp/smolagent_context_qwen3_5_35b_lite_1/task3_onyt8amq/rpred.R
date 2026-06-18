library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all wave files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Merge all waves by NSID
merged <- full_join(wave1, wave2, by = 'NSID')
merged <- full_join(merged, wave3, by = 'NSID')
merged <- full_join(merged, wave4, by = 'NSID')

# Function to convert raw values to harmonized codes based on label meaning
harmonize_lang <- function(raw_val) {
  # Handle NA first
  if (is.na(raw_val)) {
    return(NA_real_)
  }
  # Valid substantive responses
  if (raw_val %in% c(1, 2, 3, 4)) {
    return(raw_val)
  }
  # Missing values - map by label meaning
  # Not interviewed / not asked
  if (raw_val %in% c(-99, -999, -998, -997, -995)) {
    return(-3)
  }
  if (raw_val == -92) {
    return(-9)  # Refused
  }
  if (raw_val == -91) {
    return(-1)  # Not applicable
  }
  if (raw_val == -1) {
    return(-8)  # Don't know
  }
  # Other missing (script error, etc.)
  if (raw_val %in% c(-995, -997, -998)) {
    return(-2)  # Schedule not applicable
  }
  return(-3)
}

# Harmonize each wave's language variable
w1_lang <- sapply(merged$W1englangYP, harmonize_lang)
w2_lang <- sapply(merged$W2EnglangYP, harmonize_lang)
w3_lang <- sapply(merged$W3englangHH, harmonize_lang)
w4_lang <- sapply(merged$W4EngLangHH, harmonize_lang)

# Consolidate: use earliest valid substantive response (wave 1 > wave 2 > wave 3 > wave 4)
# Valid values are 1, 2, 3, 4
merged$lang <- case_when(
  !is.na(w1_lang) & w1_lang %in% c(1, 2, 3, 4) ~ w1_lang,
  !is.na(w2_lang) & w2_lang %in% c(1, 2, 3, 4) ~ w2_lang,
  !is.na(w3_lang) & w3_lang %in% c(1, 2, 3, 4) ~ w3_lang,
  !is.na(w4_lang) & w4_lang %in% c(1, 2, 3, 4) ~ w4_lang,
  TRUE ~ NA_real_
)

# Convert final NA to -3 (not asked)
merged$lang[is.na(merged$lang)] <- -3

# Create labelled factor with proper labels
merged$lang <- factor(merged$lang, 
                      levels = c(1, 2, 3, 4, -9, -8, -3, -2, -1),
                      labels = c('English only', 'English + other languages', 'Non-English first/main', 'Bilingual',
                                '-9', '-8', '-3', '-2', '-1'))

# Select only final variables
output <- merged %>% 
  select(NSID, lang)

# Write output
write_csv(output, 'data/output/cleaned_data.csv')

cat('Output written successfully\n')

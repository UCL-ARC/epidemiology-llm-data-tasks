library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all 4 files
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
w2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
w3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Merge all files by NSID using full_join
merged <- w1 %>%
  full_join(w2, by = 'NSID') %>%
  full_join(w3, by = 'NSID') %>%
  full_join(w4, by = 'NSID')

# Function to map missing values to standard codes based on meaning
harmonise_missing <- function(x, label_map) {
  # Convert to numeric
  x_num <- suppressWarnings(as.numeric(x))
  
  # Map based on label meanings
  # -999.0 -> -2 (Schedule not applicable / information lost)
  x_num[x_num == -999] <- -2
  # -998.0 -> -2 (Interviewer missed question / information lost)
  x_num[x_num == -998] <- -2
  # -997.0 -> -2 (Script error / information lost)
  x_num[x_num == -997] <- -2
  # -995.0 -> -2 (Missing data - unexplained / information lost)
  x_num[x_num == -995] <- -2
  # -99.0 -> -8 (YP not interviewed / insufficient information)
  x_num[x_num == -99] <- -8
  # -92.0 -> -9 (Refused)
  x_num[x_num == -92] <- -9
  # -1.0 -> -8 (Don't know / insufficient information)
  x_num[x_num == -1] <- -8
  # -91.0 -> -1 (Not applicable)
  x_num[x_num == -91] <- -1
  
  # Convert R NA to -3 (Not asked)
  x_num[is.na(x_num)] <- -3
  
  return(x_num)
}

# Apply harmonised missing values to all wave variables
merged <- merged %>%
  mutate(
    W1englangYP = harmonise_missing(W1englangYP),
    W2EnglangYP = harmonise_missing(W2EnglangYP),
    W3englangHH = harmonise_missing(W3englangHH),
    W4EngLangHH = harmonise_missing(W4EngLangHH)
  )

# Create valid indicator variables (substantive values only)
merged <- merged %>%
  mutate(
    W1englangYP_valid = ifelse(W1englangYP %in% c(1, 2, 3, 4), W1englangYP, NA),
    W2EnglangYP_valid = ifelse(W2EnglangYP %in% c(1, 2, 3, 4), W2EnglangYP, NA),
    W3englangHH_valid = ifelse(W3englangHH %in% c(1, 2, 3, 4), W3englangHH, NA),
    W4EngLangHH_valid = ifelse(W4EngLangHH %in% c(1, 2, 3, 4), W4EngLangHH, NA)
  )

# Create lang variable using earliest valid rule
# Order: W1 (age 14), W2 (age 15), W3 (age 16), W4 (age 17)
merged <- merged %>%
  mutate(
    lang = case_when(
      !is.na(W1englangYP_valid) ~ W1englangYP_valid,
      !is.na(W2EnglangYP_valid) ~ W2EnglangYP_valid,
      !is.na(W3englangHH_valid) ~ W3englangHH_valid,
      !is.na(W4EngLangHH_valid) ~ W4EngLangHH_valid,
      TRUE ~ NA_real_
    )
  )

# Create labelled factor with proper labels
merged <- merged %>%
  mutate(
    lang = factor(
      ifelse(is.na(lang), NA, lang),
      levels = c(1, 2, 3, 4),
      labels = c(
        'Yes - English only',
        'Yes - English first/main and speaks other languages',
        'No - another language is respondent first/main language',
        'Household is bilingual'
      )
    )
  )

# Keep only ID and final derived variable
output <- merged %>%
  select(NSID, lang)

# Write to CSV
write_csv(output, 'data/output/cleaned_data.csv')

cat('Script completed successfully.\n')
cat('Output shape:', dim(output), '\n')
cat('Value counts (including NA):', table(output$lang, useNA = 'ifany'), '\n')

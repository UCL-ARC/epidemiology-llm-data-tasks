library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all three family background files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Map missing values based on metadata label meanings
map_missing_1and2 <- function(x) {
  result <- x
  result[result == -999] <- -2
  result[result == -99] <- -3
  result[result == -98] <- -3
  result[result == -94] <- -8
  result[result == -92] <- -9
  result[result == -91] <- -1
  result[result == -1] <- -8
  return(result)
}

map_missing_4 <- function(x) {
  result <- x
  result[result == -999] <- -2
  result[result == -99] <- -3
  result[result == -98] <- -3
  result[result == -94] <- -8
  return(result)
}

# Apply missing value mapping
wave1 <- wave1 %>%
  mutate(
    W1hiqualmum_clean = map_missing_1and2(W1hiqualmum),
    W1hiqualdad_clean = map_missing_1and2(W1hiqualdad)
  )

wave2 <- wave2 %>%
  mutate(
    W2hiqualmum_clean = map_missing_1and2(W2hiqualmum),
    W2hiqualdad_clean = map_missing_1and2(W2hiqualdad)
  )

wave4 <- wave4 %>%
  mutate(
    w4hiqualmum_clean = map_missing_4(w4hiqualmum),
    w4hiqualdad_clean = map_missing_4(w4hiqualdad)
  )

# Merge all waves by NSID
combined <- full_join(wave1, wave2, by = 'NSID') %>%
  full_join(wave4, by = 'NSID')

# Function to recode to 5-level NVQ (with levels 0-4)
recode_nvq <- function(x) {
  # 1-4: NVQ4, 5-12: NVQ3, 13-16: NVQ2, 17-19: NVQ1, 20: No qual = 0
  coding <- c(4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 0)
  factor(x, levels = 1:20, labels = coding)
}

# Create detailed 20-category variables
combined <- combined %>%
  mutate(
    educdtlma = if_else(!is.na(W1hiqualmum_clean) & abs(W1hiqualmum_clean) <= 20, W1hiqualmum_clean, NA),
    educdtlma = if_else(is.na(educdtlma) & !is.na(W2hiqualmum_clean) & abs(W2hiqualmum_clean) <= 20, W2hiqualmum_clean, educdtlma),
    educdtlma = if_else(is.na(educdtlma) & !is.na(w4hiqualmum_clean) & abs(w4hiqualmum_clean) <= 20, w4hiqualmum_clean, educdtlma),
    
    educdtlpa = if_else(!is.na(W1hiqualdad_clean) & abs(W1hiqualdad_clean) <= 20, W1hiqualdad_clean, NA),
    educdtlpa = if_else(is.na(educdtlpa) & !is.na(W2hiqualdad_clean) & abs(W2hiqualdad_clean) <= 20, W2hiqualdad_clean, educdtlpa),
    educdtlpa = if_else(is.na(educdtlpa) & !is.na(w4hiqualdad_clean) & abs(w4hiqualdad_clean) <= 20, w4hiqualdad_clean, educdtlpa)
  )

# Create 5-level NVQ variables
combined <- combined %>%
  mutate(
    educma = if_else(!is.na(W1hiqualmum_clean) & abs(W1hiqualmum_clean) <= 20, recode_nvq(W1hiqualmum_clean), NA),
    educma = if_else(is.na(educma) & !is.na(W2hiqualmum_clean) & abs(W2hiqualmum_clean) <= 20, recode_nvq(W2hiqualmum_clean), educma),
    educma = if_else(is.na(educma) & !is.na(w4hiqualmum_clean) & abs(w4hiqualmum_clean) <= 20, recode_nvq(w4hiqualmum_clean), educma),
    
    educpa = if_else(!is.na(W1hiqualdad_clean) & abs(W1hiqualdad_clean) <= 20, recode_nvq(W1hiqualdad_clean), NA),
    educpa = if_else(is.na(educpa) & !is.na(W2hiqualdad_clean) & abs(W2hiqualdad_clean) <= 20, recode_nvq(W2hiqualdad_clean), educpa),
    educpa = if_else(is.na(educpa) & !is.na(w4hiqualdad_clean) & abs(w4hiqualdad_clean) <= 20, recode_nvq(w4hiqualdad_clean), educpa)
  )

# Remove intermediate variables
combined <- combined %>%
  select(-contains('W1hiqual'), -contains('W2hiqual'), -contains('w4hiqual'), -contains('_clean'))

# Write output
write_csv(combined, 'data/output/cleaned_data.csv')

cat('Script completed successfully.\n')
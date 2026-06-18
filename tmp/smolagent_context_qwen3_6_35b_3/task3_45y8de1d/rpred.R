library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(haven)
library(labelled)

# Load all files from data/input/
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)

# Merge all datasets by NSID using full_join
df <- wave1 %>%
  full_join(wave2, by = 'NSID') %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(wave4, by = 'NSID')

# Function to map source missing values to standard codes
code_missing <- function(x) {
  x[x == -999] <- -2
  x[x == -998] <- -2
  x[x == -997] <- -2
  x[x == -995] <- -2
  x[x == -94] <- -2
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -99] <- -3
  x[x == -1] <- -8
  x[is.na(x)] <- -3
  return(x)
}

# Apply missing value coding to source variables
df$W1englangYP_clean <- code_missing(df$W1englangYP)
df$W2EnglangYP_clean <- code_missing(df$W2EnglangYP)
df$W3englangHH_clean <- code_missing(df$W3englangHH)
df$W4EngLangHH_clean <- code_missing(df$W4EngLangHH)

# Derive consolidated lang variable using earliest valid response first
df$lang <- coalesce(
  ifelse(df$W1englangYP_clean >= 1 & df$W1englangYP_clean <= 4, df$W1englangYP_clean, NA_real_),
  ifelse(df$W2EnglangYP_clean >= 1 & df$W2EnglangYP_clean <= 4, df$W2EnglangYP_clean, NA_real_),
  ifelse(df$W3englangHH_clean >= 1 & df$W3englangHH_clean <= 4, df$W3englangHH_clean, NA_real_),
  ifelse(df$W4EngLangHH_clean >= 1 & df$W4EngLangHH_clean <= 4, df$W4EngLangHH_clean, NA_real_)
)

# Replace any remaining NAs with -3
df$lang[is.na(df$lang)] <- -3

# Create labelled vector - labels are the names, codes are the values
labels_vec <- c(
  'Yes - English only' = 1,
  'Yes - English first/main and speaks other languages' = 2,
  "No, another language is respondent's first/main language" = 3,
  'Respondent is bilingual' = 4,
  'Item not applicable' = -1,
  'Schedule not applicable / script error / information lost' = -2,
  'Not asked at the fieldwork stage / not interviewed' = -3,
  'Prefer not to say' = -7,
  'Don\'t know / insufficient information' = -8,
  'Refusal' = -9
)

df$lang <- haven::labelled(df$lang, labels = labels_vec)

# Select only NSID and the final derived variable
output_df <- df %>% select(NSID, lang)

# Write output
dir.create('data/output', showWarnings = FALSE, recursive = TRUE)
write_csv(output_df, 'data/output/cleaned_data.csv')

# Print summary for verification
cat('Total rows:', nrow(output_df), '\n')
cat('lang distribution:\n')
print(table(output_df$lang, useNA = 'ifany'))
print(head(output_df))

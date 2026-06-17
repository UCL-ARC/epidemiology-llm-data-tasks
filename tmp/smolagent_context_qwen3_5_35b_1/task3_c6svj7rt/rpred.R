library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from data/input/
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
w2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
w3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Merge all files by NSID using full_join
df <- full_join(w1, w2, by = 'NSID')
df <- full_join(df, w3, by = 'NSID')
df <- full_join(df, w4, by = 'NSID')

# Function to clean missing values according to metadata and requirements
clean_missing <- function(x) {
  x <- case_when(
    x == -1 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -99 ~ -3,
    x %in% c(-999, -998, -997, -995, -94) ~ -2,
    x %in% c(1, 2, 3, 4) ~ x,
    TRUE ~ -3
  )
  return(x)
}

# Clean each source variable
df$W1englangYP <- clean_missing(df$W1englangYP)
df$W2EnglangYP <- clean_missing(df$W2EnglangYP)
df$W3englangHH <- clean_missing(df$W3englangHH)
df$W4EngLangHH <- clean_missing(df$W4EngLangHH)

# Create consolidated lang variable using earliest-valid-first logic
df$lang <- case_when(
  df$W1englangYP %in% c(1, 2, 3, 4) ~ df$W1englangYP,
  df$W2EnglangYP %in% c(1, 2, 3, 4) ~ df$W2EnglangYP,
  df$W3englangHH %in% c(1, 2, 3, 4) ~ df$W3englangHH,
  df$W4EngLangHH %in% c(1, 2, 3, 4) ~ df$W4EngLangHH,
  TRUE ~ NA_real_
)

# Convert NA to -3 for final missing values
df$lang[is.na(df$lang)] <- -3

# Keep only NSID and lang in output
df_out <- df %>% select(NSID, lang)

# Write to CSV
write_csv(df_out, 'data/output/cleaned_data.csv')

cat('Output written to data/output/cleaned_data.csv\n')
cat('Total records:', nrow(df_out), '\n')
cat('Lang variable summary:\n')
print(table(df_out$lang, useNA = 'ifany'))
library(dplyr)
library(readr)
library(haven)
library(labelled)

# Define file paths
files <- c(
  wave1 = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave2 = 'data/input/wave_two_lsype_young_person_2020.tab',
  wave3 = 'data/input/wave_three_lsype_family_background_2020.tab',
  wave4 = 'data/input/wave_four_lsype_family_background_2020.tab'
)

# Load each file
w1 <- read_delim(files['wave1'], delim = '\t', show_col_types = FALSE)
w2 <- read_delim(files['wave2'], delim = '\t', show_col_types = FALSE)
w3 <- read_delim(files['wave3'], delim = '\t', show_col_types = FALSE)
w4 <- read_delim(files['wave4'], delim = '\t', show_col_types = FALSE)

# Merge all datasets by NSID using full_join
df <- w1 %>%
  full_join(w2, by = 'NSID') %>%
  full_join(w3, by = 'NSID') %>%
  full_join(w4, by = 'NSID')

# Function to recode missing values to standard scheme
recode_missing <- function(x) {
  # Map source missing codes to standard codes based on label meaning
  # -999, -998, -997, -995 -> -2 (schedule not applicable / script error / information lost)
  # -94 -> -2 (per additional requirements)
  # -99 -> -3 (not asked / not interviewed)
  # -92 -> -9 (refusal)
  # -91 -> -1 (not applicable)
  # -1 Don't know -> -8
  
  x[x == -999 | x == -998 | x == -997 | x == -995] <- -2
  x[x == -94] <- -2
  x[x == -99] <- -3
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  
  return(x)
}

# Recode missing values in each source variable within the merged dataframe
df$W1englangYP_recoded <- recode_missing(df$W1englangYP)
df$W2EnglangYP_recoded <- recode_missing(df$W2EnglangYP)
df$W3englangHH_recoded <- recode_missing(df$W3englangHH)
df$W4EngLangHH_recoded <- recode_missing(df$W4EngLangHH)

# Derive consolidated lang variable using earliest valid positive response first
# Valid codes are 1-4
# Priority: W1englangYP -> W2EnglangYP -> W3englangHH -> W4EngLangHH

df$lang <- case_when(
  !is.na(df$W1englangYP_recoded) & df$W1englangYP_recoded %in% 1:4 ~ df$W1englangYP_recoded,
  !is.na(df$W2EnglangYP_recoded) & df$W2EnglangYP_recoded %in% 1:4 ~ df$W2EnglangYP_recoded,
  !is.na(df$W3englangHH_recoded) & df$W3englangHH_recoded %in% 1:4 ~ df$W3englangHH_recoded,
  !is.na(df$W4EngLangHH_recoded) & df$W4EngLangHH_recoded %in% 1:4 ~ df$W4EngLangHH_recoded,
  TRUE ~ NA_real_
)

# Convert any remaining NAs to -3 (not asked)
df$lang[is.na(df$lang)] <- -3

# Create labelled factor for lang
lang_factor <- labelled(
  df$lang,
  labels = c(
    'English only' = 1,
    'English first/main and speaks other languages' = 2,
    'Another language is first/main' = 3,
    'Bilingual' = 4,
    "Don't know" = -8,
    'Not applicable' = -1,
    'Not interviewed/asked' = -3,
    'Refused' = -9,
    'Schedule not applicable' = -2
  )
)

# Create final output dataframe
cleaned <- df %>%
  select(NSID) %>%
  mutate(lang = lang_factor)

# Write output
write_csv(cleaned, 'data/output/cleaned_data.csv')

cat('Output written to data/output/cleaned_data.csv\n')
cat('Number of rows:', nrow(cleaned), '\n')
cat('Number of columns:', ncol(cleaned), '\n')
print(table(cleaned$lang, useNA = 'ifany'))

library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab'
)

load_data <- function(file) {
  readr::read_delim(paste0('data/input/', file), delim = '\t', col_types = readr::cols(.default = 'double'))
}

# Read files
df1 <- readr::read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
df2 <- readr::read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
df3 <- readr::read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
df4 <- readr::read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Ensure NSID is string
df1$NSID <- as.character(df1$NSID)
df2$NSID <- as.character(df2$NSID)
df3$NSID <- as.character(df3$NSID)
df4$NSID <- as.character(df4$NSID)

# Merge datasets
full_df <- df1 %>%
  full_join(df2, by = 'NSID') %>%
  full_join(df3, by = 'NSID') %>%
  full_join(df4, by = 'NSID')

# 2. Define mapping for missing values based on metadata labels
# -9 = Refusal
# -8 = Don't know / insufficient information
# -7 = Prefer not to say
# -3 = Not asked / not interviewed
# -2 = Schedule not applicable / script error / info lost
# -1 = Item not applicable

clean_ecoact <- function(var_vec, wave_meta) {
  # Copy vector to avoid modifying original
  res <- var_vec
  
  # Map by labels provided in metadata
  # -999.0: Missing household information - lost -> -2
  res[var_vec == -999] <- -2
  # -99.0: Mother/Father not interviewed -> -3
  res[var_vec == -99] <- -3
  # -98.0: Mother/Father not present -> -1
  res[var_vec == -98] <- -1
  # -94.0: Insufficient information -> -8
  res[var_vec == -94] <- -8
  # -996.0 (found in wave 4 father): No parent in household -> -1
  res[var_vec == -996] <- -1
  # -92.0 (found in wave 4 father): Refusal -> -9
  res[var_vec == -92] <- -9
  
  # Convert NAs to -3 as per general guidance (unless specified otherwise)
  res[is.na(res)] <- -3
  
  return(res)
}

# Process each wave
# Wave 1 (14)
full_df$ecoactma14 <- clean_ecoact(full_df$W1empsmum, NULL)
full_df$ecoactpa14 <- clean_ecoact(full_df$W1empsdad, NULL)

# Wave 2 (15)
full_df$ecoactma15 <- clean_ecoact(full_df$W2empsmum, NULL)
full_df$ecoactpa15 <- clean_ecoact(full_df$W2empsdad, NULL)

# Wave 3 (16)
full_df$ecoactma16 <- clean_ecoact(full_df$W3empsmum, NULL)
full_df$ecoactpa16 <- clean_ecoact(full_df$W3empsdad, NULL)

# Wave 4 (17)
full_df$ecoactma17 <- clean_ecoact(full_df$w4empsmum, NULL)
full_df$ecoactpa17 <- clean_ecoact(full_df$w4empsdad, NULL)

# Define labels for the factor
# The categories 1-9 are consistent across all waves
ecoact_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable",
  "1" = "Doing paid work for 30 or more hours a week",
  "2" = "Doing paid work for fewer than 30 hours a week",
  "3" = "Unemployed/ Looking for a job",
  "4" = "On a training course or scheme",
  "5" = "In full-time education/ at school",
  "6" = "Looking after the family/ household",
  "7" = "Retired from work altogether",
  "8" = "Sick/ disabled",
  "9" = "Other"
)

# Convert to factors
target_vars <- c('ecoactma14', 'ecoactpa14', 'ecoactma15', 'ecoactpa15', 'ecoactma16', 'ecoactpa16', 'ecoactma17', 'ecoactpa17')

full_df <- full_df %>%
  mutate(across(all_of(target_vars), ~ factor(.x, levels = as.numeric(names(ecoact_labels)), labels = ecoact_labels)))

# Final selection
final_df <- full_df %>%
  select(NSID, all_of(target_vars))

# Write output
readr::write_csv(final_df, 'data/output/cleaned_data.csv')
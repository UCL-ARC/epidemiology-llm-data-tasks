library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Helper function to load and ensure NSID is character
load_data_fixed <- function(file) {
  df <- read_delim(paste0('data/input/', file), delim = '\t', guess_max = 10000, show_col_types = FALSE)
  if('NSID' %in% names(df)) {
    df <- df %>% mutate(NSID = as.character(NSID))
  }
  return(df)
}

# Standard Missing-Value Codes helper
standardize_missing <- function(x) {
  x <- as.numeric(x)
  # -94 is Insufficient information -> -8
  x[x == -94] <- -8
  # Wide range of wave-specific missing codes (-999, -99 etc) -> -3
  x[x < -9] <- -3
  # Handle Nulls
  x[is.na(x)] <- -3
  return(x)
}

# Load datasets
w1 <- load_data_fixed('wave_one_lsype_young_person_2020.tab')
w4 <- load_data_fixed('wave_four_lsype_young_person_2020.tab')
w2 <- load_data_fixed('wave_two_lsype_family_background_2020.tab')
w3 <- load_data_fixed('wave_three_lsype_family_background_2020.tab')
w8 <- load_data_fixed('ns8_2015_derived.tab')
w9d <- load_data_fixed('ns9_2022_derived_variables.tab')
w9m <- load_data_fixed('ns9_2022_main_interview.tab')

# Process Wave 2 (Age 15)
# Use the exact variable names from metadata: urbind, gor
w2_clean <- w2 %>%
  mutate(regub15 = standardize_missing(urbind),
         regov15 = standardize_missing(gor)) %>%
  select(NSID, regub15, regov15)

# Process Wave 3 (Age 16)
w3_clean <- w3 %>%
  mutate(regub16 = standardize_missing(urbind),
         regov16 = standardize_missing(gor)) %>%
  select(NSID, regub16, regov16)

# Process Wave 8
w8_clean <- w8 %>%
  mutate(regov_w8 = standardize_missing(W8DGOR)) %>%
  select(NSID, regov_w8)

# Process Wave 9 Derived
w9d_clean <- w9d %>%
  mutate(regov_w9 = standardize_missing(W9DRGN)) %>%
  select(NSID, regov_w9)

# Process Wave 9 Main
w9m_clean <- w9m %>%
  mutate(regint32 = standardize_missing(W9NATIONRES)) %>%
  select(NSID, regint32)

# Merge all processed datasets
# Start with NSID from wave 1 to ensure base cohort
final_df <- w1 %>% select(NSID)
final_df <- full_join(final_df, w2_clean, by = 'NSID')
final_df <- full_join(final_df, w3_clean, by = 'NSID')
final_df <- full_join(final_df, w8_clean, by = 'NSID')
final_df <- full_join(final_df, w9d_clean, by = 'NSID')
final_df <- full_join(final_df, w9m_clean, by = 'NSID')

# Create derived binary UK/Abroad (regint_uk)
# W9NATIONRES: 1=England, 2=Scotland, 3=Wales, 4=NI, 5=Outside/Unknown
final_df <- final_df %>%
  mutate(regint_uk_val = case_when(
    regint32 %in% 1:4 ~ 1, # UK
    regint32 == 5 ~ 0,    # Abroad
    TRUE ~ regint32       # Keep missing codes
  )) %>%
  mutate(regint_uk = factor(regint_uk_val, 
                            levels = c(-9, -8, -3, -1, -2, -7, 0, 1), 
                            labels = c("Refusal", "Don't know", "Not asked", "N/A", "Script error", "Prefer not to say", "Abroad", "UK")))

# Final Selection: ID and derived variables only
output_df <- final_df %>%
  select(NSID, regub15, regov15, regub16, regov16, regov_w8, regov_w9, regint32, regint_uk)

# Write to CSV
write_csv(output_df, 'data/output/cleaned_data.csv')
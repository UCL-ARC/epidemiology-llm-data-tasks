library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 3. Standard Missing-Value Codes function
harmonize_missing <- function(x) {
  if (is.null(x)) return(NULL)
  x[is.na(x)] <- -3
  # Convert wave-specific codes based on metadata
  # -94.0 is 'Insufficient information' -> -8
  x[x == -94] <- -8
  # General missing range -999 to -1 mapping to standard codes
  # We'll map -999, -99 to -3 (Not asked/lost)
  x[x == -999] <- -3
  x[x == -99] <- -3
  return(x)
}

# Process files individually to avoid name clashes and handle specific columns
# Wave 2 (Age 15)
w2_raw <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w2 <- w2_raw %>%
  transmute(NSID, regub15 = harmonize_missing(urbind), regov15 = harmonize_missing(gor))

# Wave 3 (Age 16)
w3_raw <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w3 <- w3_raw %>%
  transmute(NSID, regub16 = harmonize_missing(urbind), regov16 = harmonize_missing(gor))

# Wave 8
w8_raw <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', show_col_types = FALSE)
w8 <- w8_raw %>%
  transmute(NSID, regov_w8 = harmonize_missing(W8DGOR))

# Wave 9 (Derived)
w9d_raw <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', show_col_types = FALSE)
w9d <- w9d_raw %>%
  transmute(NSID, regov_w9 = harmonize_missing(W9DRGN))

# Wave 9 (Main)
w9m_raw <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)
w9m <- w9m_raw %>%
  transmute(NSID, regint32 = harmonize_missing(W9NATIONRES))

# Merge all processed dataframes
final_df <- w2 %>%
  full_join(w3, by = 'NSID') %>%
  full_join(w8, by = 'NSID') %>%
  full_join(w9d, by = 'NSID') %>%
  full_join(w9m, by = 'NSID')

# Factor labels for regov variables
regov_labels <- c("-9" = "Refusal", "-8" = "Don't know", "-1" = "Not applicable", "-3" = "Not asked", "-2" = "Script error", "-7" = "Prefer not to say", 
                  "1" = "North East", "2" = "North West", "3" = "Yorkshire and The Humber", "4" = "East Midlands", "5" = "West Midlands", "6" = "East of England", "7" = "London", "8" = "South East", "9" = "South West", "10" = "Wales", "11" = "Scotland", "12" = "Northern Ireland", "13" = "Unknown")

# Convert regov variables to factors
final_df <- final_df %>%
  mutate(across(starts_with("regov"), ~factor(.x, levels = as.numeric(names(regov_labels)), labels = regov_labels)))

# Save output
write_csv(final_df, 'data/output/cleaned_data.csv')
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
wave8_derived <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
wave9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = readr::cols(.default = 'c'))

# Merge datasets
merged_data <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave8_derived, by = 'NSID') %>%
  full_join(wave9_derived, by = 'NSID')

# Helper function to process BMI variables
# BMI is continuous. Missing values are negative. Map to standard codes.
# -9 Refusal, -8 Don't know/Insufficient, -7 Prefer not to say, -3 Not asked, -2 Schedule not app/info lost, -1 Not applicable
process_bmi <- function(var_name, data) {
  vec <- as.numeric(data[[var_name]])
  
  # Map based on metadata provided in the prompt
  # For W8DBMI: -9 Refused, -8 Insufficient info, -1 Not applicable
  # For W9DBMI: -9 Refused, -8 Insufficient info, -1 Not applicable
  
  # Standard missing-value codes:
  # -9 = Refusal
  # -8 = Don't know / insufficient information
  # -1 = Item not applicable
  # -3 = Not asked/NA
  
  res <- vec
  res[vec == -9.0] <- -9
  res[vec == -8.0] <- -8
  res[vec == -1.0] <- -1
  res[is.na(res)] <- -3
  
  return(res)
}

# Derive bmi25 (from wave 8) and bmi32 (from wave 9)
bmi25 <- process_bmi('W8DBMI', merged_data)
bmi32 <- process_bmi('W9DBMI', merged_data)

# Create final dataframe
final_df <- data.frame(
  NSID = merged_data$NSID,
  bmi25 = bmi25,
  bmi32 = bmi32
)

# Write output
write_csv(final_df, 'data/output/cleaned_data.csv')
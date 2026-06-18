library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Load all files mentioned in metadata to preserve the full cohort frame
file_paths <- c(
  'data/input/wave_one_lsype_young_person_2020.tab',
  'data/input/wave_four_lsype_young_person_2020.tab',
  'data/input/ns8_2015_derived.tab',
  'data/input/ns9_2022_derived_variables.tab'
)

# Reading the files
# Since the files are tab-delimited .tab files
df1 <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(), show_col_types = FALSE)
df4 <- readr::read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(), show_col_types = FALSE)
df8 <- readr::read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = readr::cols(), show_col_types = FALSE)
df9 <- readr::read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = readr::cols(), show_col_types = FALSE)

# Merge datasets using full_join by NSID
full_frame <- df1 %>%
  full_join(df4, by = 'NSID') %>%
  full_join(df8, by = 'NSID') %>%
  full_join(df9, by = 'NSID')

# 2. Variable Derivation for BMI
# Target: bmi25 and bmi32
# Source for bmi25 (Age 25 / Wave 8): W8DBMI
# Source for bmi32 (Age 32 / Wave 9): W9DBMI

# Standard Missing Value Mapping:
# -9 = Refusal
# -8 = Don't know / insufficient information
# -7 = Prefer not to say
# -3 = Not asked / not interviewed (NA)
# -2 = Schedule not applicable / script error / information lost
# -1 = Item not applicable

process_bmi <- function(var_vec) {
  # Create a copy to avoid altering raw data
  res <- var_vec
  
  # Map based on metadata labels
  # W8DBMI: -9.0: Refused, -8.0: Insufficient information, -1.0: Not applicable
  # W9DBMI: -9.0: Refused, -8.0: Insufficient information, -1.0: Not applicable
  
  # The metadata for both explicitly defines:
  # -9.0 -> Refused (-9)
  # -8.0 -> Insufficient information (-8)
  # -1.0 -> Not applicable (-1)
  
  # Note: In R, NAs should become -3
  res[is.na(res)] <- -3
  
  # The values are already aligned with standard codes in this specific case
  # but let's be explicit
  res <- ifelse(res == -9.0, -9, 
                ifelse(res == -8.0, -8, 
                       ifelse(res == -1.0, -1, res)))
  
  return(as.numeric(res))
}

full_frame <- full_frame %>%
  mutate(
    bmi25 = process_bmi(W8DBMI),
    bmi32 = process_bmi(W9DBMI)
  )

# 3. Final Selection
# Only keep NSID and final derived variables
final_data <- full_frame %>%
  select(NSID, bmi25, bmi32)

# 4. Output
readr::write_csv(final_data, 'data/output/cleaned_data.csv')
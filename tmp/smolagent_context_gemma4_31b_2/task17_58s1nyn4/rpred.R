library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# 1. Load Files
data_w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'c')) %>% select(NSID)
data_w4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'c')) %>% select(NSID)
data_w2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', col_types = cols(.default = 'c')) %>% select(NSID, IMDRSCORE)
data_w3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', col_types = cols(.default = 'c')) %>% select(NSID, IMDRSCORE)
data_w9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = cols(.default = 'c')) %>% select(NSID, W9DIMDD)

# 2. Merge datasets using explicit suffixes to avoid any ambiguity
# Start with a base frame of all NSIDs from all files to ensure full cohort frame
all_nsids <- bind_rows(
  data_w1 %>% select(NSID),
  data_w4 %>% select(NSID),
  data_w2 %>% select(NSID),
  data_w3 %>% select(NSID),
  data_w9 %>% select(NSID)
) %>% distinct(NSID)

final_df <- all_nsids %>%
  left_join(data_w2, by = 'NSID') %>%
  left_join(data_w3, by = 'NSID', suffix = c('_w2', '_w3')) %>%
  left_join(data_w9, by = 'NSID')

# Note: After these joins, the IMDRSCORE from data_w2 is named 'IMDRSCORE' 
# and the one from data_w3 is named 'IMDRSCORE_w3'.

# 3. Cleaning Functions
process_imd <- function(x) {
  if (is.null(x)) return(numeric(0))
  res <- as.numeric(x)
  # Metadata: -94.0 is 'Insufficient Information' -> -8
  # We use the original character vector x to check for specific values
  res[x == "-94.0" | x == "-94"] <- -8
  # Metadata: user_missing_values -999.0 thru -1.0 -> -2
  res[res < 0 & res != -8] <- -2
  # Convert R NA to -3
  res[is.na(res)] <- -3
  return(res)
}

process_imd32 <- function(x) {
  if (is.null(x)) return(numeric(0))
  res <- as.numeric(x)
  # Metadata: -8.0 is 'Insufficient information' -> -8
  res[x == "-8.0" | x == "-8"] <- -8
  # Convert R NA to -3
  res[is.na(res)] <- -3
  return(res)
}

# 4. Apply transformation
# We check if columns exist before calling the functions to avoid the 'object not found' error
final_df <- final_df %>%
  mutate(
    imd15 = if('IMDRSCORE' %in% names(.)) process_imd(IMDRSCORE) else -3,
    imd16 = if('IMDRSCORE_w3' %in% names(.)) process_imd(IMDRSCORE_w3) else -3,
    imd32 = if('W9DIMDD' %in% names(.)) process_imd32(W9DIMDD) else -3
  ) %>%
  select(NSID, imd15, imd16, imd32)

# 5. Output
write_csv(final_df, 'data/output/cleaned_data.csv')

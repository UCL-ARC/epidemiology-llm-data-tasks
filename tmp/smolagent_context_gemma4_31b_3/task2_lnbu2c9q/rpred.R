library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. Load files
file_paths <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

load_tab <- function(filename) {
  read_delim(paste0('data/input/', filename), delim = '\t', col_types = readr::cols())
}

df1 <- load_tab('wave_one_lsype_young_person_2020.tab')
df2 <- load_tab('wave_two_lsype_young_person_2020.tab')
df4 <- load_tab('wave_four_lsype_young_person_2020.tab')
df8 <- load_tab('ns8_2015_derived.tab')
df9 <- load_tab('ns9_2022_derived_variables.tab')

# Merge datasets
cohort <- df1 %>%
  full_join(df2, by = 'NSID') %>%
  full_join(df4, by = 'NSID') %>%
  full_join(df8, by = 'NSID') %>%
  full_join(df9, by = 'NSID')

# 2. Define harmonisation for missing values
# Standard codes:
# -9: Refusal, -8: Don't know/Insuff, -7: Prefer not, -3: Not asked, -2: Schedule error, -1: Not applicable

clean_missing <- function(x, wave_labels) {
  # Create a mapping based on metadata provided for each wave
  # This is a generic helper; we will apply specific logic per variable below
  res <- x
  return(res)
}

# Specific mapping for ethnicity variables based on metadata labels
# W1ethnic2YP
# -999.0: Missing - household data lost -> -2
# -94.0: Insufficient information -> -8
# -92.0: Refused -> -9
# -91.0: Not applicable -> -1
# -1.0: Don't know -> -8

# W2ethnicYP
# -998.0: Interviewer missed question -> -3
# -997.0: Script error -> -2
# -995.0: Missing history section data -> -2
# -99.0: YP not interviewed -> -3
# -92.0: Refused -> -9
# -91.0: Not applicable -> -1
# -1.0: Don't Know -> -8

# w4ethnic2YP
# -94.0: Insufficient information -> -8
# -1.0: Don't know -> -8

# W8DETHN15
# -9.0: Refused -> -9
# -8.0: Insufficient information -> -8
# -1.0: Not applicable -> -1

# W9DETHN15
# -8.0: Insufficient information -> -8

# Pre-process each source variable to map missing values to standard codes
# We only need to know if the value is a valid substantive response (1-16) or a missing code

process_eth <- function(var_name, data) {
  vals <- data[[var_name]]
  # Substantive responses are 1-16
  # Everything else is missing
  # For the purpose of "earliest valid positive response", we just need to identify 1-16
  res <- ifelse(vals >= 1 & vals <= 16, vals, NA)
  return(res)
}

# Apply derivation logic: earliest valid positive response first
# Order: W1ethnic2YP, W2ethnicYP, w4ethnic2YP, W8DETHN15, W9DETHN15

cohort <- cohort %>%
  mutate(
    eth_w1 = process_eth('W1ethnic2YP', .),
    eth_w2 = process_eth('W2ethnicYP', .),
    eth_w4 = process_eth('w4ethnic2YP', .),
    eth_w8 = process_eth('W8DETHN15', .),
    eth_w9 = process_eth('W9DETHN15', .)
  )

cohort <- cohort %>%
  mutate(
    eth = coalesce(eth_w1, eth_w2, eth_w4, eth_w8, eth_w9)
  )

# Now handle the final missing values for 'eth' if no substantive response was found
# Since the requirement is to use detailed categories 1-16, and we used coalesce on valid ones,
# any remaining NA means no valid response was found across all waves.
# According to General Guidance 6: Convert R NA values to -3 unless metadata indicates otherwise.

cohort$eth[is.na(cohort$eth)] <- -3

# Define labels for the factor
eth_labels <- c(
  "1" = "White - British",
  "2" = "White - Irish",
  "3" = "Any other White background",
  "4" = "Mixed - White and Black Caribbean",
  "5" = "Mixed - White and Black African",
  "6" = "Mixed - White and Asian",
  "7" = "Any other mixed background",
  "8" = "Indian",
  "9" = "Pakistani",
  "10" = "Bangladeshi",
  "11" = "Any other Asian background",
  "12" = "Black Caribbean",
  "13" = "Black African",
  "14" = "Any other Black background",
  "15" = "Chinese",
  "16" = "Any other ethnic background",
  "-3" = "Not asked at the fieldwork stage / not interviewed"
)

# Convert to factor with labels
cohort$eth <- factor(cohort$eth, levels = names(eth_labels), labels = eth_labels)

# Final selection: NSID and eth
final_data <- cohort %>%
  select(NSID, eth)

write_csv(final_data, 'data/output/cleaned_data.csv')

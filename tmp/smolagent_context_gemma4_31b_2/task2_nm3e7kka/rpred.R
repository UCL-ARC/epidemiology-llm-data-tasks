library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
file1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols())
file2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols())
file4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols())
file8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = readr::cols())
file9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols())

# Merge datasets
full_df <- file1 %>%
  full_join(file2, by = "NSID") %>%
  full_join(file4, by = "NSID") %>%
  full_join(file8, by = "NSID") %>%
  full_join(file9, by = "NSID")

# Define a function to map missing values based on metadata labels
# Standard: -9 Refusal, -8 Don't know/insufficient, -7 Prefer not to say, -3 Not asked, -2 Schedule/Script, -1 Not applicable

map_missing <- function(val, wave_name) {
  if (is.na(val)) return(-3)
  
  # Logic based on metadata for each specific variable
  if (wave_name == "W1") {
    if (val == -999) return(-2)
    if (val == -94) return(-8)
    if (val == -92) return(-9)
    if (val == -91) return(-1)
    if (val == -1) return(-8)
  }
  if (wave_name == "W2") {
    if (val == -998) return(-2)
    if (val == -997) return(-2)
    if (val == -995) return(-2)
    if (val == -99) return(-3)
    if (val == -92) return(-9)
    if (val == -91) return(-1)
    if (val == -1) return(-8)
  }
  if (wave_name == "W4") {
    if (val == -94) return(-8)
    if (val == -1) return(-8)
  }
  if (wave_name == "W8") {
    if (val == -9) return(-9)
    if (val == -8) return(-8)
    if (val == -1) return(-1)
  }
  if (wave_name == "W9") {
    if (val == -8) return(-8)
  }
  
  return(val)
}

# Pre-process variables to identify valid substantive responses (1-16)
# and map missing values

process_var <- function(df, var_name, wave_name) {
  df[[var_name]] <- sapply(df[[var_name]], function(x) map_missing(x, wave_name))
  return(df[[var_name]])
}

# Create temporary cleaned columns for the consolidation process
full_df <- full_df %>%
  mutate(
    w1_clean = process_var(full_df, "W1ethnic2YP", "W1"),
    w2_clean = process_var(full_df, "W2ethnicYP", "W2"),
    w4_clean = process_var(full_df, "w4ethnic2YP", "W4"),
    w8_clean = process_var(full_df, "W8DETHN15", "W8"),
    w9_clean = process_var(full_df, "W9DETHN15", "W9")
  )

# Consolidation logic: earliest valid positive response first (1-16)
# Order: W1 -> W2 -> W4 -> W8 -> W9

full_df <- full_df %>%
  mutate(eth = case_when(
    w1_clean >= 1 & w1_clean <= 16 ~ w1_clean,
    w2_clean >= 1 & w2_clean <= 16 ~ w2_clean,
    w4_clean >= 1 & w4_clean <= 16 ~ w4_clean,
    w8_clean >= 1 & w8_clean <= 16 ~ w8_clean,
    w9_clean >= 1 & w9_clean <= 16 ~ w9_clean,
    TRUE ~ coalesce(w1_clean, w2_clean, w4_clean, w8_clean, w9_clean, -3)
  ))

# If the fallback (TRUE) results in a value outside the standard missing range, set to -3
# However, based on the logic, it will be one of the mapped missing codes.

# Define labels for eth
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
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

full_df$eth <- factor(full_df$eth, levels = names(eth_labels), labels = eth_labels)

# Final output selection
output_df <- full_df %>%
  select(NSID, eth)

write_csv(output_df, "data/output/cleaned_data.csv")
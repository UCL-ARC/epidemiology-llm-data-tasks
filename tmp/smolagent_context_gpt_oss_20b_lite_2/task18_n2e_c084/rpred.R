# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# Define file paths
files <- list(
  wave_one = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_two = 'data/input/wave_two_lsype_young_person_2020.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  wave_eight_main = 'data/input/ns8_2015_self_completion.tab',
  wave_eight_derived = 'data/input/ns8_2015_derived.tab',
  wave_nine_main  = 'data/input/ns9_2022_main_interview.tab',
  wave_nine_derived = 'data/input/ns9_2022_derived_variables.tab'
)

# Read all files (tab‑delimited, treat "" as NA)
raw <- lapply(files, function(p) {
  read_delim(p, delim = '\t', col_types = cols(), na = c('', 'NA'))
})

# Assign to variables for clarity
wave_one          <- raw$wave_one
wave_two          <- raw$wave_two
wave_four         <- raw$wave_four
wave_eight_main   <- raw$wave_eight_main
wave_eight_derived<- raw$wave_eight_derived
wave_nine_main    <- raw$wave_nine_main
wave_nine_derived<- raw$wave_nine_derived

# Merge all datasets by NSID using full_join to keep all cohort members
full <- wave_one %>%
  full_join(wave_two,   by = 'NSID') %>%
  full_join(wave_four,  by = 'NSID') %>%
  full_join(wave_eight_main,   by = 'NSID') %>%
  full_join(wave_eight_derived, by = 'NSID') %>%
  full_join(wave_nine_main,    by = 'NSID') %>%
  full_join(wave_nine_derived, by = 'NSID')

# Create GHQ‑12 caseness variables (pre‑derived where available, otherwise use the wave score)
full <- full %>%
  mutate(
    ghq15 = W2ghq12scr,            # Wave 2 (age 15)
    ghq17 = W4ghq12scr,            # Wave 4 (age 17)
    ghq25 = W8DGHQSC,             # Derived GHQ score from wave 8 (age 25)
    ghq32 = W9DGHQSC              # Derived GHQ score from wave 9 (age 32)
  )

# Function to compute summed GHQ‑12 score only if all items are non‑negative
sum_ghq <- function(df, prefix) {
  items <- df %>% select(starts_with(prefix)) %>% unlist()
  if (all(!is.na(items) & items >= 0)) {
    sum(items, na.rm = FALSE)
  } else {
    NA_real_
  }
}

# Compute item‑summed GHQ‑12 scores for waves where raw items exist
full <- full %>% rowwise() %>% mutate(
  ghqtl15 = W2ghq12scr,              # Wave 2 (age 15) – already summed
  ghqtl17 = W4ghq12scr,              # Wave 4 (age 17) – already summed
  ghqtl25 = sum_ghq(cur_data(), 'W8GHQ12_'),
  ghqtl32 = sum_ghq(cur_data(), 'W9GHQ12_')
) %>% ungroup()

# Replace R NA in the new summed variables with the standard missing code -3
full <- full %>%
  mutate(across(starts_with('ghqtl'), ~replace_na(., -3)))

# Select only the ID and the required derived variables
cleaned <- full %>%
  select(NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32)

# Write the cleaned data to CSV
write_csv(cleaned, 'data/output/cleaned_data.csv')
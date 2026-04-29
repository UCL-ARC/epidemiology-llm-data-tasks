library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create('data/output', showWarnings = FALSE, recursive = TRUE)

# Load each file
wave_two <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave_four <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
ns8_self <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t', show_col_types = FALSE)
ns8_derived <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', show_col_types = FALSE)
ns9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)
ns9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', show_col_types = FALSE)

# Define GHQ item names for each sweep
ghq_items_w2 <- c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP', 'W2strainYP', 'W2difficYP',
                  'W2activYP', 'W2probsYP', 'W2depressYP', 'W2noconfYP', 'W2wthlessYP', 'W2happyYP')

ghq_items_w4 <- c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP', 'W4StrainYP', 'W4DifficYP',
                  'W4ActivYP', 'W4ProbsYP', 'W4DepressYP', 'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP')

ghq_items_w8 <- c('W8GHQ12_1', 'W8GHQ12_2', 'W8GHQ12_3', 'W8GHQ12_4', 'W8GHQ12_5', 'W8GHQ12_6',
                  'W8GHQ12_7', 'W8GHQ12_8', 'W8GHQ12_9', 'W8GHQ12_10', 'W8GHQ12_11', 'W8GHQ12_12')

ghq_items_w9 <- c('W9GHQ12_1', 'W9GHQ12_2', 'W9GHQ12_3', 'W9GHQ12_4', 'W9GHQ12_5', 'W9GHQ12_6',
                  'W9GHQ12_7', 'W9GHQ12_8', 'W9GHQ12_9', 'W9GHQ12_10', 'W9GHQ12_11', 'W9GHQ12_12')

# Extract ID and GHQ variables from each file
# Wave 2 (Age 15)
w2_data <- wave_two %>%
  select(NSID, all_of(ghq_items_w2), W2ghq12scr)

# Wave 4 (Age 17)
w4_data <- wave_four %>%
  select(NSID, all_of(ghq_items_w4), W4ghq12scr)

# Wave 8 (Age 25) - from self-completion (items) and derived (pre-derived score)
w8_data <- ns8_self %>%
  select(NSID, all_of(ghq_items_w8))
w8_derived <- ns8_derived %>%
  select(NSID, W8DGHQSC)

# Wave 9 (Age 32) - from main interview (items) and derived (pre-derived score)
w9_data <- ns9_main %>%
  select(NSID, all_of(ghq_items_w9))
w9_derived <- ns9_derived %>%
  select(NSID, W9DGHQSC)

# Merge all datasets using full_join by NSID
df_merged <- w2_data %>%
  full_join(w4_data, by = 'NSID') %>%
  full_join(w8_data, by = 'NSID') %>%
  full_join(w8_derived, by = 'NSID') %>%
  full_join(w9_data, by = 'NSID') %>%
  full_join(w9_derived, by = 'NSID')

# Function to harmonize pre-derived GHQ scores for earlier sweeps (wave 2 and 4)
harmonize_early_ghq <- function(x) {
  x <- case_when(
    x %in% c(-96, -99) ~ -3,
    x %in% c(-97, -92) ~ -9,
    is.na(x) ~ -3,
    TRUE ~ x
  )
  return(x)
}

# Function to harmonize pre-derived GHQ scores for later sweeps (wave 8 and 9)
harmonize_late_ghq <- function(x) {
  x <- case_when(
    is.na(x) ~ -3,
    TRUE ~ x
  )
  return(x)
}

# Harmonize pre-derived GHQ scores
df_merged <- df_merged %>%
  mutate(
    ghq15 = harmonize_early_ghq(W2ghq12scr),
    ghq17 = harmonize_early_ghq(W4ghq12scr),
    ghq25 = harmonize_late_ghq(W8DGHQSC),
    ghq32 = harmonize_late_ghq(W9DGHQSC)
  )

# Function to compute manually derived GHQ-12 sum scores
compute_ghq_sum <- function(items) {
  # Check if ALL 12 items are NA
  all_na <- rowSums(is.na(items)) == 12
  
  # Check if ANY item contains a negative value
  has_negative <- rowSums(items < 0, na.rm = TRUE) > 0
  
  # Compute sum for valid cases
  sum_scores <- rowSums(items, na.rm = TRUE)
  
  # Assign values based on conditions
  result <- ifelse(all_na, -3,
                   ifelse(has_negative, -8, sum_scores))
  return(result)
}

# Compute manually derived GHQ-12 sum scores
df_merged <- df_merged %>%
  mutate(
    ghqtl15 = compute_ghq_sum(across(all_of(ghq_items_w2))),
    ghqtl17 = compute_ghq_sum(across(all_of(ghq_items_w4))),
    ghqtl25 = compute_ghq_sum(across(all_of(ghq_items_w8))),
    ghqtl32 = compute_ghq_sum(across(all_of(ghq_items_w9)))
  )

# Convert all derived GHQ variables to factors
df_merged <- df_merged %>%
  mutate(
    ghq15 = as.factor(ghq15),
    ghq17 = as.factor(ghq17),
    ghq25 = as.factor(ghq25),
    ghq32 = as.factor(ghq32),
    ghqtl15 = as.factor(ghqtl15),
    ghqtl17 = as.factor(ghqtl17),
    ghqtl25 = as.factor(ghqtl25),
    ghqtl32 = as.factor(ghqtl32)
  )

# Select only required columns: ID and 8 GHQ variables
df_output <- df_merged %>%
  select(NSID, ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32)

# Write output to CSV
write_csv(df_output, 'data/output/cleaned_data.csv')

# Print summary
cat('Output file created successfully!\n')
cat('Number of rows:', nrow(df_output), '\n')
cat('Number of columns:', ncol(df_output), '\n')
cat('Columns:', paste(names(df_output), collapse = ', '), '\n')

# Show first few rows
print(head(df_output))

library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

input_dir <- 'data/input'
output_dir <- 'data/output'

# Load all datasets
wave1 <- read_delim(file.path(input_dir, 'wave_one_lsype_family_background_2020.tab'), delim = '\t')
wave2 <- read_delim(file.path(input_dir, 'wave_two_lsype_family_background_2020.tab'), delim = '\t')
wave3 <- read_delim(file.path(input_dir, 'wave_three_lsype_family_background_2020.tab'), delim = '\t')
wave4 <- read_delim(file.path(input_dir, 'wave_four_lsype_family_background_2020.tab'), delim = '\t')
wave5 <- read_delim(file.path(input_dir, 'wave_five_lsype_family_background_2020.tab'), delim = '\t')
wave6 <- read_delim(file.path(input_dir, 'wave_six_lsype_young_person_2020.tab'), delim = '\t')
wave7 <- read_delim(file.path(input_dir, 'wave_seven_lsype_young_person_2020.tab'), delim = '\t')
wave8 <- read_delim(file.path(input_dir, 'ns8_2015_main_interview.tab'), delim = '\t')
wave9 <- read_delim(file.path(input_dir, 'ns9_2022_derived_variables.tab'), delim = '\t')

# Create full cohort frame
cohort <- full_join(wave1, wave2, by = 'NSID') %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave5, by = 'NSID') %>%
  full_join(wave6, by = 'NSID') %>%
  full_join(wave7, by = 'NSID') %>%
  full_join(wave8, by = 'NSID') %>%
  full_join(wave9, by = 'NSID')

# Function to recode missing values for sweeps 1-7: -1 -> -8
recode_sweeps_1_7 <- function(x) {
  if (is.numeric(x)) {
    x[x == -1] <- -8
  }
  return(x)
}

# ---- Sweep 1 (age 14)
cohort <- cohort %>% mutate(
  hownteen14 = recode_sweeps_1_7(W1hous12HH),
  hown14 = case_when(
    W1hous12HH == -999 ~ -3,
    W1hous12HH == -997 ~ -2,
    W1hous12HH == -92 ~ -9,
    W1hous12HH == -91 ~ -3,
    W1hous12HH == -1 ~ -8,
    W1hous12HH == 1 ~ 1,
    W1hous12HH == 2 ~ 2,
    W1hous12HH == 3 ~ 3,
    W1hous12HH == 4 ~ 4,
    W1hous12HH == 5 ~ 5,
    W1hous12HH == 6 ~ 7,
    W1hous12HH == 7 ~ 5,
    W1hous12HH == 8 ~ 8,
    TRUE ~ NA_integer_
  )
)

# ---- Sweep 2 (age 15)
cohort <- cohort %>% mutate(
  hownteen15 = recode_sweeps_1_7(W2Hous12HH),
  hown15 = case_when(
    W2Hous12HH == -998 ~ -3,
    W2Hous12HH == -997 ~ -2,
    W2Hous12HH == -995 ~ -2,
    W2Hous12HH == -99 ~ -3,
    W2Hous12HH == -92 ~ -9,
    W2Hous12HH == -91 ~ -3,
    W2Hous12HH == -1 ~ -8,
    W2Hous12HH == 1 ~ 1,
    W2Hous12HH == 2 ~ 2,
    W2Hous12HH == 3 ~ 3,
    W2Hous12HH == 4 ~ 4,
    W2Hous12HH == 5 ~ 5,
    W2Hous12HH == 6 ~ 7,
    W2Hous12HH == 7 ~ 5,
    W2Hous12HH == 8 ~ 8,
    TRUE ~ NA_integer_
  )
)

# ---- Sweep 3 (age 16)
cohort <- cohort %>% mutate(
  hownteen16 = recode_sweeps_1_7(W3hous12HH),
  hown16 = case_when(
    W3hous12HH == -999 ~ -3,
    W3hous12HH == -99 ~ -3,
    W3hous12HH == -92 ~ -9,
    W3hous12HH == -91 ~ -3,
    W3hous12HH == -1 ~ -8,
    W3hous12HH == 1 ~ 1,
    W3hous12HH == 2 ~ 2,
    W3hous12HH == 3 ~ 3,
    W3hous12HH == 4 ~ 4,
    W3hous12HH == 5 ~ 5,
    W3hous12HH == 6 ~ 7,
    W3hous12HH == 7 ~ 5,
    W3hous12HH == 8 ~ 8,
    TRUE ~ NA_integer_
  )
)

# ---- Sweep 4 (age 17)
cohort <- cohort %>% mutate(
  hownteen17 = recode_sweeps_1_7(W4Hous12HH),
  hown17 = case_when(
    W4Hous12HH == -999 ~ -3,
    W4Hous12HH == -997 ~ -2,
    W4Hous12HH == -92 ~ -9,
    W4Hous12HH == -91 ~ -3,
    W4Hous12HH == -1 ~ -8,
    W4Hous12HH == 1 ~ 1,
    W4Hous12HH == 2 ~ 2,
    W4Hous12HH == 3 ~ 3,
    W4Hous12HH == 4 ~ 4,
    W4Hous12HH == 5 ~ 5,
    W4Hous12HH == 6 ~ 7,
    W4Hous12HH == 7 ~ 5,
    W4Hous12HH == 8 ~ 8,
    TRUE ~ NA_integer_
  )
)

# ---- Sweep 5 (age 18): Three subtype variables
cohort <- cohort %>% mutate(
  W5Hous12BHH_clean = recode_sweeps_1_7(W5Hous12BHH),
  W5Hous12CHH_clean = recode_sweeps_1_7(W5Hous12CHH)
)

cohort <- cohort %>% mutate(
  hownteen18 = case_when(
    is.na(W5Hous12HH) | W5Hous12HH == 6 | W5Hous12BHH_clean %in% c(-999, -92, -91) | W5Hous12CHH_clean %in% c(-999, -92, -91) ~ NA_integer_,
    W5Hous12BHH_clean %in% c(1, 2, 3, 4) ~ W5Hous12BHH_clean,
    TRUE ~ NA_integer_
  ),
  hown18 = case_when(
    W5Hous12BHH_clean == 1 ~ 1,
    W5Hous12BHH_clean == 2 ~ 2,
    W5Hous12BHH_clean == 3 ~ 3,
    W5Hous12BHH_clean == 4 ~ 8,
    W5Hous12CHH_clean == 1 ~ 4,
    W5Hous12CHH_clean == 2 ~ 5,
    W5Hous12CHH_clean == 3 ~ 6,
    W5Hous12CHH_clean == 4 ~ 7,
    W5Hous12CHH_clean == 5 ~ 8,
    TRUE ~ -1
  )
)

# ---- Sweep 6 (age 19): Three subtype variables
cohort <- cohort %>% mutate(
  W6Hous12bYP_clean = recode_sweeps_1_7(W6Hous12bYP),
  W6Hous12cYP_clean = recode_sweeps_1_7(W6Hous12cYP)
)

cohort <- cohort %>% mutate(
  hownteen19 = case_when(
    is.na(W6Hous12bYP) | W6Hous12bYP %in% c(-92, -91) | is.na(W6Hous12cYP) | W6Hous12cYP %in% c(-92, -91) ~ NA_integer_,
    W6Hous12bYP_clean %in% c(1, 2, 3, 4) ~ W6Hous12bYP_clean,
    TRUE ~ NA_integer_
  ),
  hown19 = case_when(
    W6Hous12bYP_clean == 1 ~ 1,
    W6Hous12bYP_clean == 2 ~ 2,
    W6Hous12bYP_clean == 3 ~ 3,
    W6Hous12bYP_clean == 4 ~ 8,
    W6Hous12cYP_clean == 1 ~ 4,
    W6Hous12cYP_clean == 2 ~ 5,
    W6Hous12cYP_clean == 3 ~ 6,
    W6Hous12cYP_clean == 4 ~ 7,
    W6Hous12cYP_clean == 5 ~ 8,
    TRUE ~ -1
  )
)

# ---- Sweep 7 (age 20): Three subtype variables
cohort <- cohort %>% mutate(
  W7Hous12bYP_clean = recode_sweeps_1_7(W7Hous12bYP),
  W7Hous12cYP_clean = recode_sweeps_1_7(W7Hous12cYP)
)

cohort <- cohort %>% mutate(
  hownteen20 = case_when(
    is.na(W7Hous12bYP) | W7Hous12bYP %in% c(-92, -91) | is.na(W7Hous12cYP) | W7Hous12cYP %in% c(-92, -91) ~ NA_integer_,
    W7Hous12bYP_clean %in% c(1, 2, 3, 4) ~ W7Hous12bYP_clean,
    TRUE ~ NA_integer_
  ),
  hown20 = case_when(
    W7Hous12bYP_clean == 1 ~ 1,
    W7Hous12bYP_clean == 2 ~ 2,
    W7Hous12bYP_clean == 3 ~ 3,
    W7Hous12bYP_clean == 4 ~ 8,
    W7Hous12cYP_clean == 1 ~ 4,
    W7Hous12cYP_clean == 2 ~ 5,
    W7Hous12cYP_clean == 3 ~ 6,
    W7Hous12cYP_clean == 4 ~ 7,
    W7Hous12cYP_clean == 5 ~ 8,
    TRUE ~ -1
  )
)

# ---- Sweep 8 (age 25)
cohort <- cohort %>% mutate(
  hown25 = case_when(
    W8TENURE == 1 ~ 1,
    W8TENURE == 2 ~ 2,
    W8TENURE == 3 ~ 3,
    W8TENURE == 4 ~ 4,
    W8TENURE == 5 ~ 7,
    W8TENURE == 6 ~ 6,
    W8TENURE == 7 ~ 8,
    W8TENURE %in% c(-9, -8, -1) ~ -1,
    TRUE ~ -1
  )
)

# ---- Sweep 9 (age 32)
cohort <- cohort %>% mutate(
  hown32 = case_when(
    W9DTENURE == 1 ~ 1,
    W9DTENURE == 2 ~ 2,
    W9DTENURE == 3 ~ 3,
    W9DTENURE == 4 ~ 4,
    W9DTENURE == 5 ~ 7,
    W9DTENURE == 6 ~ 6,
    W9DTENURE == 7 ~ 8,
    W9DTENURE %in% c(-8, -3) ~ -1,
    TRUE ~ -1
  )
)

# Ensure proper types
cohort <- cohort %>% mutate(
  across(c(hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32), as.integer)
)

# Keep only NSID and final derived variables
final_vars <- c('NSID', 'hownteen14', 'hownteen15', 'hownteen16', 'hownteen17', 
               'hownteen18', 'hownteen19', 'hownteen20',
               'hown14', 'hown15', 'hown16', 'hown17', 'hown18', 
               'hown19', 'hown20', 'hown25', 'hown32')

cohort_final <- cohort %>% select(all_of(final_vars))

# Write output
write_csv(cohort_final, file.path(output_dir, 'cleaned_data.csv'))

print('Processing complete!')
print(paste('Output rows:', nrow(cohort_final)))
print(paste('Output columns:', ncol(cohort_final)))
}
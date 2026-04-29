library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
library(tidyr)

# 1. File Loading
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_self_completion.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_main_interview.tab',
  'ns9_2022_derived_variables.tab'
)

full_df <- NULL

for (f in files) {
  df <- read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(.default = 'c'))
  # Convert numeric columns to numeric for calculations
  df <- df %>% mutate(across(everything(), ~type.convert(., as.numeric = TRUE)))
  
  if (is.null(full_df)) {
    full_df <- df
  } else {
    full_df <- full_join(full_df, df, by = 'NSID')
  }
}

# Function to calculate item-summed score
calc_ghq_sum <- function(row) {
  if (all(is.na(row))) return(-3)
  if (any(row < 0, na.rm = TRUE)) return(-8)
  return(sum(row, na.rm = TRUE))
}

# Wave 2 (Age 15)
items_w2 <- c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP', 'W2strainYP', 'W2difficYP', 'W2activYP', 'W2probsYP', 'W2depressYP', 'W2noconfYP', 'W2wthlessYP', 'W2happyYP')
full_df$ghqtl15 <- apply(full_df[, items_w2], 1, calc_ghq_sum)

full_df <- full_df %>% mutate(
  ghq15 = case_when(
    W2ghq12scr == -97 ~ -9,
    W2ghq12scr == -92 ~ -9,
    W2ghq12scr == -99 ~ -3,
    W2ghq12scr < 0 ~ -2, # Default for -998, -997, etc
    TRUE ~ W2ghq12scr
  )
)

# Wave 4 (Age 17)
items_w4 <- c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP', 'W4StrainYP', 'W4DifficYP', 'W4ActivYP', 'W4ProbsYP', 'W4DepressYP', 'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP')
full_df$ghqtl17 <- apply(full_df[, items_w4], 1, calc_ghq_sum)

full_df <- full_df %>% mutate(
  ghq17 = case_when(
    W4ghq12scr == -97 ~ -9,
    W4ghq12scr == -92 ~ -9,
    W4ghq12scr == -99 ~ -3,
    W4ghq12scr < 0 ~ -2,
    TRUE ~ W4ghq12scr
  )
)

# Wave 8 (Age 25)
items_w8 <- paste0('W8GHQ12_', 1:12)
full_df$ghqtl25 <- apply(full_df[, items_w8], 1, calc_ghq_sum)

full_df <- full_df %>% mutate(
  ghq25 = case_when(
    W8DGHQSC == -9 ~ -9,
    W8DGHQSC == -8 ~ -8,
    W8DGHQSC == -1 ~ -1,
    TRUE ~ W8DGHQSC
  )
)

# Wave 9 (Age 32)
items_w9 <- paste0('W9GHQ12_', 1:12)
full_df$ghqtl32 <- apply(full_df[, items_w9], 1, calc_ghq_sum)

full_df <- full_df %>% mutate(
  ghq32 = case_when(
    W9DGHQSC == -9 ~ -9,
    W9DGHQSC == -8 ~ -8,
    W9DGHQSC == -1 ~ -1,
    TRUE ~ W9DGHQSC
  )
)

# Handle NAs in final variables
final_vars <- c('ghqtl15', 'ghq15', 'ghqtl17', 'ghq17', 'ghqtl25', 'ghq25', 'ghqtl32', 'ghq32')
full_df[final_vars] <- lapply(full_df[final_vars], function(x) tidyr::replace_na(x, -3))

# Final output
output_df <- full_df %>% select(NSID, all_of(final_vars))
write_csv(output_df, 'data/output/cleaned_data.csv')
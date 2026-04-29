library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

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

data_list <- map(files, ~ read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = 'c')))
names(data_list) <- files

# Convert numeric columns that should be numeric
# Since we loaded as strings for safety, we convert the specific columns needed
# However, for GHQ items, they are numeric

# Merge datasets using full_join by NSID
full_df <- data_list %>% reduce(full_join, by = 'NSID')

# Helper to convert and clean missing values based on standard scheme
harmonise_missing <- function(x, wave = NULL) {
  x <- as.numeric(x)
  if (!is.null(wave) && (wave == 'W2' || wave == 'W4')) {
    x <- case_when(
      x == -97 ~ -9, # YP refused self completion -> Refusal
      x == -92 ~ -9, # Refused -> Refusal
      x == -99 ~ -3, # YP not interviewed -> Not asked/interviewed
      x == -96 ~ -2, # YP using interpreter -> information lost/error (approx)
      x == -91 ~ -1, # Not applicable
      x == -1  ~ -8, # Don't know
      x == -998 ~ -2,
      x == -997 ~ -2,
      x == -995 ~ -2,
      TRUE ~ x
    )
  } else {
    x <- case_when(
      x == -9 ~ -9, # Refused
      x == -8 ~ -8, # Don't know
      x == -1 ~ -1, # Not applicable
      x == -3 ~ -3, # Not asked
      x == -99 ~ -3, # Default for not interviewed
      TRUE ~ x
    )
  }
  x[is.na(x)] <- -3
  return(x)
}

# Item summed GHQ logic
calculate_ghq_sum <- function(df, vars) {
  # Create a temporary matrix of the items as numeric
  mat <- df %>% select(all_of(vars)) %>% mutate(across(everything(), as.numeric))
  
  # Row-wise logic
  results <- apply(mat, 1, function(row) {
    if (all(is.na(row))) return(-3)
    if (any(row < 0, na.rm = TRUE)) return(-8)
    # If any item is NA but not all, and no negative values, we have a problem
    # The requirement says "Else sum the 12 items". Usually this implies valid values.
    # If there are NAs, we can't sum properly. Let's treat NA as -8 if any are present but not all
    if (any(is.na(row))) return(-8)
    return(sum(row))
  })
  return(results)
}

# Define waves and variables
waves_info <- list(
  "15" = list(
    items = c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP', 'W2strainYP', 'W2difficYP', 'W2activYP', 'W2probsYP', 'W2depressYP', 'W2noconfYP', 'W2wthlessYP', 'W2happyYP'),
    pre = 'W2ghq12scr',
    wave_id = 'W2'
  ),
  "17" = list(
    items = c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP', 'W4StrainYP', 'W4DifficYP', 'W4ActivYP', 'W4ProbsYP', 'W4DepressYP', 'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP'),
    pre = 'W4ghq12scr',
    wave_id = 'W4'
  ),
  "25" = list(
    items = c('W8GHQ12_1', 'W8GHQ12_2', 'W8GHQ12_3', 'W8GHQ12_4', 'W8GHQ12_5', 'W8GHQ12_6', 'W8GHQ12_7', 'W8GHQ12_8', 'W8GHQ12_9', 'W8GHQ12_10', 'W8GHQ12_11', 'W8GHQ12_12'),
    pre = 'W8DGHQSC',
    wave_id = 'W8'
  ),
  "32" = list(
    items = c('W9GHQ12_1', 'W9GHQ12_2', 'W9GHQ12_3', 'W9GHQ12_4', 'W9GHQ12_5', 'W9GHQ12_6', 'W9GHQ12_7', 'W9GHQ12_8', 'W9GHQ12_9', 'W9GHQ12_10', 'W9GHQ12_11', 'W9GHQ12_12'),
    pre = 'W9DGHQSC',
    wave_id = 'W9'
  )
)

final_vars <- list(NSID = full_df$NSID)

for (age in names(waves_info)) {
  info <- waves_info[[age]]
  
  # Summed score
  final_vars[[paste0('ghqtl', age)]] <- calculate_ghq_sum(full_df, info$items)
  
  # Pre-derived score
  pre_var <- full_df[[info$pre]]
  final_vars[[paste0('ghq', age)]] <- harmonise_missing(pre_var, info$wave_id)
}

# Create final dataframe
output_df <- as.data.frame(final_vars)

# Save to CSV
write_csv(output_df, 'data/output/cleaned_data.csv')

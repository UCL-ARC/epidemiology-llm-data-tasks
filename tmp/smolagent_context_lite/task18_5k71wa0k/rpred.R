library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
library(tidyr)

# To avoid memory issues, we will load only the necessary columns from each file
# instead of loading entire datasets and then joining them.

# Wave 15 variables
vars15_raw <- c('NSID', 'W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP', 'W2strainYP', 'W2difficYP', 'W2activYP', 'W2probsYP', 'W2depressYP', 'W2noconfYP', 'W2wthlessYP', 'W2happyYP', 'W2ghq12scr')
# Wave 17 variables
vars17_raw <- c('NSID', 'W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP', 'W4StrainYP', 'W4DifficYP', 'W4ActivYP', 'W4ProbsYP', 'W4DepressYP', 'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP', 'W4ghq12scr')
# Wave 25 variables
vars25_raw <- c('NSID', paste0('W8GHQ12_', 1:12))
vars25_der <- c('NSID', 'W8DGHQSC')
# Wave 32 variables
vars32_raw <- c('NSID', paste0('W9GHQ12_', 1:12))
vars32_der <- c('NSID', 'W9DGHQSC')

# Helper to load specific columns
load_cols <- function(file, cols) {
  # Read only first row to get all names, then filter for target columns
  temp <- read_delim(paste0('data/input/', file), delim = '\t', n_max = 0, col_types = cols(.default = 'numeric'))
  all_cols <- colnames(temp)
  target_cols <- intersect(all_cols, cols)
  # read_delim doesn't have a 'usecols' like pandas, so we read all but select immediately
  # To save memory, we use a smaller read and then select
  df <- read_delim(paste0('data/input/', file), delim = '\t', col_types = cols(.default = 'numeric'))
  df %>% select(all_of(target_cols)) %>% mutate(NSID = as.character(NSID))
}

# Load only what's needed
df15 <- load_cols('wave_two_lsype_young_person_2020.tab', vars15_raw)
df17 <- load_cols('wave_four_lsype_young_person_2020.tab', vars17_raw)
df25_raw <- load_cols('ns8_2015_self_completion.tab', vars25_raw)
df25_der <- load_cols('ns8_2015_derived.tab', vars25_der)
df32_raw <- load_cols('ns9_2022_main_interview.tab', vars32_raw)
df32_der <- load_cols('ns9_2022_derived_variables.tab', vars32_der)

# Merge incrementally
full_df <- df15 %>%
  full_join(df17, by = 'NSID') %>%
  full_join(df25_raw, by = 'NSID') %>%
  full_join(df25_der, by = 'NSID') %>%
  full_join(df32_raw, by = 'NSID') %>%
  full_join(df32_der, by = 'NSID')

# Missing value mapping helper
map_missing_vals <- function(x) {
  res <- x
  res[x == -99] <- -3
  res[x == -92] <- -9
  res[x == -91] <- -1
  res[x == -1] <- -8
  res[x == -97] <- -2
  res[x == -98] <- -2
  res[x == -998] <- -2
  res[x == -997] <- -2
  res[x == -995] <- -2
  res[x == -96] <- -2
  res[x == -999] <- -2
  res[is.na(res)] <- -3
  return(res)
}

# GHQ scoring helper
calc_ghq_sum <- function(df, vars) {
  scores <- map_dfc(vars, function(v) {
    if(!v %in% colnames(df)) return(rep(NA, nrow(df)))
    val <- df[[v]]
    s <- rep(NA, length(val))
    s[val %in% c(1, 2)] <- 0
    s[val %in% c(3, 4)] <- 1
    return(s)
  })
  res <- rowSums(scores, na.rm = TRUE)
  res[rowSums(!is.na(scores)) == 0] <- NA
  return(res)
}

# Calculate variables
full_df$ghqtl15 <- calc_ghq_sum(full_df, c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP', 'W2strainYP', 'W2difficYP', 'W2activYP', 'W2probsYP', 'W2depressYP', 'W2noconfYP', 'W2wthlessYP', 'W2happyYP'))
full_df$ghq15 <- map_missing_vals(full_df$W2ghq12scr)

full_df$ghqtl17 <- calc_ghq_sum(full_df, c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP', 'W4StrainYP', 'W4DifficYP', 'W4ActivYP', 'W4ProbsYP', 'W4DepressYP', 'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP'))
full_df$ghq17 <- map_missing_vals(full_df$W4ghq12scr)

full_df$ghqtl25 <- calc_ghq_sum(full_df, paste0('W8GHQ12_', 1:12))
full_df$ghq25 <- map_missing_vals(full_df$W8DGHQSC)

full_df$ghqtl32 <- calc_ghq_sum(full_df, paste0('W9GHQ12_', 1:12))
full_df$ghq32 <- map_missing_vals(full_df$W9DGHQSC)

# Final output
final_vars <- c('NSID', 'ghqtl15', 'ghqtl17', 'ghqtl25', 'ghqtl32', 'ghq15', 'ghq17', 'ghq25', 'ghq32')
output_df <- full_df %>% select(all_of(final_vars)) %>% mutate(across(where(is.numeric), ~replace_na(.x, -3)))

write_csv(output_df, 'data/output/cleaned_data.csv')

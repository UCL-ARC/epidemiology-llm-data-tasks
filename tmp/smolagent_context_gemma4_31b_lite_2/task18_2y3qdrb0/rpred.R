library(haven)
library(dplyr)
library(tidyr)
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

data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t'))

# Merge all datasets explicitly
# Note: data_list is a list of data frames. reduce(full_join, ...) needs a list of data frames.
full_df <- reduce(data_list, full_join, by = 'NSID')

# Function to standardize missing values based on labels
standardize_missing <- function(x, labels) {
  if (is.null(labels)) return(x)
  
  res <- x
  for (val in names(labels)) {
    lbl <- labels[[val]]
    val_num <- as.numeric(val)
    
    if (grepl('Refused', lbl, ignore.case = TRUE)) {
      res[x == val_num] <- -9
    } else if (grepl('Don\'t Know|Insufficient information', lbl, ignore.case = TRUE)) {
      res[x == val_num] <- -8
    } else if (grepl('Prefer not to say', lbl, ignore.case = TRUE)) {
      res[x == val_num] <- -7
    } else if (grepl('Not asked', lbl, ignore.case = TRUE)) {
      res[x == val_num] <- -3
    } else if (grepl('Script error|information lost|missed question|unexplained', lbl, ignore.case = TRUE)) {
      res[x == val_num] <- -2
    } else if (grepl('Not applicable', lbl, ignore.case = TRUE)) {
      res[x == val_num] <- -1
    }
  }
  res[is.na(res)] <- -3
  return(res)
}

# Variable Sets
w2_vars <- c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP', 'W2strainYP', 'W2difficYP', 'W2activYP', 'W2probsYP', 'W2depressYP', 'W2noconfYP', 'W2wthlessYP', 'W2happyYP')
w4_vars <- c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP', 'W4StrainYP', 'W4DifficYP', 'W4ActivYP', 'W4ProbsYP', 'W4DepressYP', 'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP')
w8_vars <- paste0('W8GHQ12_', 1:12)
w9_vars <- paste0('W9GHQ12_', 1:12)

# Function for Likert Sum
calc_sum <- function(df, vars) {
  # If any columns are missing in df, handle gracefully
  available_vars <- intersect(vars, names(df))
  if (length(available_vars) < length(vars)) {
    return(rep(-3, nrow(df)))
  }
  
  sum_val <- apply(df[, vars, drop = FALSE], 1, function(row) {
    if (any(is.na(row)) || any(row < 1)) return(-3)
    return(sum(row))
  })
  return(sum_val)
}

# Calculate summed scores
full_df$ghqtl15 <- calc_sum(full_df, w2_vars)
full_df$ghqtl17 <- calc_sum(full_df, w4_vars)
full_df$ghqtl25 <- calc_sum(full_df, w8_vars)
full_df$ghqtl32 <- calc_sum(full_df, w9_vars)

# Process Caseness scores with specific metadata labels
full_df <- full_df %>%
  mutate(
    ghq15 = standardize_missing(W2ghq12scr, c('-99.0' = 'YP not interviewed', '-97.0' = 'YP refused self completion', '-96.0' = 'YP using interpreter', '-92.0' = 'Refused')),
    ghq17 = standardize_missing(W4ghq12scr, c('-99.0' = 'YP not interviewed', '-97.0' = 'YP refused self completion', '-96.0' = 'YP using interpreter', '-92.0' = 'Refused')),
    ghq25 = standardize_missing(W8DGHQSC, c('-9.0' = 'Refused', '-8.0' = 'Insufficient information', '-1.0' = 'Not applicable')),
    ghq32 = standardize_missing(W9DGHQSC, c('-9.0' = 'Refused', '-8.0' = 'Insufficient information', '-1.0' = 'Not applicable'))
  )

# Final selection
final_df <- full_df %>%
  select(NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32)

# Write output
write_csv(final_df, 'data/output/cleaned_data.csv')

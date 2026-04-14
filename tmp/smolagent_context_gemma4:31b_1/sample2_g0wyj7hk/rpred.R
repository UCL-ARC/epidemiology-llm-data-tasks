library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# To avoid the massive join overflow, we must ensure NSID is treated correctly.
# The error suggests NSID might be read as NA or constant for many rows if the type is wrong.
# We will explicitly read NSID as a character.

meta_vars <- list(
  'wave_one_lsype_young_person_2020.tab' = 'W1ethnic2YP',
  'wave_two_lsype_young_person_2020.tab' = 'W2ethnicYP',
  'wave_four_lsype_young_person_2020.tab' = 'w4ethnic2YP',
  'ns8_2015_derived.tab' = 'W8DETHN15',
  'ns9_2022_derived_variables.tab' = 'W9DETHN15'
)

load_subset <- function(file, var) {
  # Read as character first to ensure NSID is not corrupted by numeric parsing
  df <- readr::read_delim(paste0('data/input/', file), delim = '\t', col_types = readr::cols(NSID = col_character(), .default = 'numeric'))
  # Remove rows where NSID is NA to prevent Cartesian product during join
  df <- df %>% filter(!is.na(NSID)) %>% select(NSID, all_of(var))
  return(df)
}

# Load and merge subsets
merged_data <- NULL

for (file in names(meta_vars)) {
  var_name <- meta_vars[[file]]
  temp_df <- load_subset(file, var_name)
  
  if (is.null(merged_data)) {
    merged_data <- temp_df
  } else {
    # Use full_join and ensure we are not creating a many-to-many explosion
    # In cohort studies, NSID should be unique per file
    temp_df <- temp_df %>% distinct(NSID, .keep_all = TRUE)
    merged_data <- full_join(merged_data, temp_df, by = 'NSID')
  }
}

# 3 & 5. Missing Value Harmonization Function
harmonize_missing <- function(x) {
  x <- as.numeric(x)
  x[x == -999] <- -3
  x[x == -998] <- -3
  x[x == -997] <- -3
  x[x == -995] <- -3
  x[x == -99] <- -3
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  x[x == -9] <- -9
  x[x == -8] <- -8
  x[is.na(x)] <- -3
  return(x)
}

# 2 & 4. Create age-specific variables
merged_data <- merged_data %>%
  mutate(
    eth14 = harmonize_missing(W1ethnic2YP),
    eth15 = harmonize_missing(W2ethnicYP),
    eth17 = harmonize_missing(w4ethnic2YP),
    eth_w8 = harmonize_missing(W8DETHN15),
    eth_w9 = harmonize_missing(W9DETHN15)
  )

# 6. Time-Invariant: Ethnicity (Prioritize earliest valid response > 0)
# Use a vectorized approach instead of apply for speed and reliability
get_earliest_valid_vec <- function(df) {
  cols <- c('eth14', 'eth15', 'eth17', 'eth_w8', 'eth_w9')
  mat <- as.matrix(df[, cols])
  
  # Find first column where value > 0
  pos_mask <- mat > 0
  first_pos_idx <- apply(pos_mask, 1, function(x) which(x)[1])
  
  # Find first column where value is not NA (fallback)
  non_na_mask <- !is.na(mat)
  first_non_na_idx <- apply(non_na_mask, 1, function(x) which(x)[1])
  
  result <- rep(-3, nrow(df))
  
  # Fill fallback first
  for(i in 1:nrow(df)) {
    if(!is.na(first_non_na_idx[i])) {
      result[i] <- mat[i, first_non_na_idx[i]]
    }
  }
  
  # Overwrite with positive value if exists
  for(i in 1:nrow(df)) {
    if(!is.na(first_pos_idx[i])) {
      result[i] <- mat[i, first_pos_idx[i]]
    }
  }
  return(result)
}

merged_data$eth <- get_earliest_valid_vec(merged_data)

# 7. Factor Variables
eth_labels <- c(
  '-9' = 'Refusal',
  '-8' = 'Don\'t know/Insufficient',
  '-3' = 'Not asked/interviewed',
  '-2' = 'Schedule not applicable',
  '-1' = 'Item not applicable',
  '1' = 'White - British',
  '2' = 'White - Irish',
  '3' = 'Any other White background',
  '4' = 'Mixed - White and Black Caribbean',
  '5' = 'Mixed - White and Black African',
  '6' = 'Mixed - White and Asian',
  '7' = 'Any other mixed background',
  '8' = 'Indian',
  '9' = 'Pakistani',
  '10' = 'Bangladeshi',
  '11' = 'Any other Asian background',
  '12' = 'Black Caribbean',
  '13' = 'Black African',
  '14' = 'Any other Black background',
  '15' = 'Chinese',
  '16' = 'Any other ethnic background'
)

merged_data$eth <- factor(as.numeric(merged_data$eth), levels = as.numeric(names(eth_labels)), labels = eth_labels)

# 10. Select final variables
final_data <- merged_data %>%
  select(NSID, eth)

# 9. Output
write_csv(final_data, 'data/output/cleaned_data.csv')
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# The error "Column NSID doesn't exist" after `colnames(df)[1] <- 'NSID'` 
# suggests that the dataframe df might be empty or not have any columns.
# Let's use a more robust approach to loading and selecting.

load_and_clean <- function(file) {
  # Read file
  df <- readr::read_delim(paste0('data/input/', file), delim = '\t', col_types = readr::cols(.default = 'character'))
  
  if (ncol(df) == 0) return(NULL)
  
  # Force first column name to NSID
  names(df)[1] <- 'NSID'
  
  # Determine target variable
  target_var <- NULL
  if (grepl('wave_four', file)) target_var <- 'W4nsseccatYP'
  else if (grepl('wave_five', file)) target_var <- 'W5nsseccatYP'
  else if (grepl('wave_six', file)) target_var <- 'w6nsseccatYP'
  else if (grepl('wave_seven', file)) target_var <- 'W7NSSECCat'
  else if (grepl('ns8', file)) target_var <- 'W8DNSSEC17'
  else if (grepl('ns9', file)) target_var <- 'W9NSSEC'
  
  # Keep only NSID and target_var if it exists
  cols_to_keep <- c('NSID')
  if (!is.null(target_var) && target_var %in% names(df)) {
    cols_to_keep <- c(cols_to_keep, target_var)
  }
  
  df <- df[, cols_to_keep, drop = FALSE]
  
  if (!is.null(target_var) && target_var %in% names(df)) {
    df[[target_var]] <- as.numeric(df[[target_var]])
  }
  
  # Remove duplicates
  df <- df %>% distinct(NSID, .keep_all = TRUE)
  
  return(df)
}

# Load files
df1 <- load_and_clean('wave_one_lsype_young_person_2020.tab')
df4 <- load_and_clean('wave_four_lsype_young_person_2020.tab')
df5 <- load_and_clean('wave_five_lsype_young_person_2020.tab')
df6 <- load_and_clean('wave_six_lsype_young_person_2020.tab')
df7 <- load_and_clean('wave_seven_lsype_young_person_2020.tab')
df8 <- load_and_clean('ns8_2015_derived.tab')
df9 <- load_and_clean('ns9_2022_main_interview.tab')

# Merge (handle potential NULLs)
data_list <- list(df1, df4, df5, df6, df7, df8, df9)
data_list <- compact(data_list)

full_df <- reduce(data_list, full_join, by = 'NSID')

# Processing function
process_nssec <- function(var) {
  case_when(
    var >= 1.0 ~ floor(var),
    var == -91.0 ~ -1,
    var == -99.0 ~ -3,
    var == -9.0 ~ -9,
    var == -8.0 ~ -8,
    var == -1.0 ~ -1,
    TRUE ~ -3
  )
}

# Derive variables
full_df <- full_df %>%
  mutate(
    nssec17 = if('W4nsseccatYP' %in% names(.)) process_nssec(W4nsseccatYP) else -3,
    nssec18 = if('W5nsseccatYP' %in% names(.)) process_nssec(W5nsseccatYP) else -3,
    nssec19 = if('w6nsseccatYP' %in% names(.)) process_nssec(w6nsseccatYP) else -3,
    nssec20 = if('W7NSSECCat' %in% names(.)) process_nssec(W7NSSECCat) else -3,
    nssec25 = if('W8DNSSEC17' %in% names(.)) process_nssec(W8DNSSEC17) else -3,
    nssec32 = if('W9NSSEC' %in% names(.)) {
      case_when(
        W9NSSEC >= 1.0 ~ floor(W9NSSEC),
        W9NSSEC == -1.0 ~ -1,
        W9NSSEC >= -9.0 & W9NSSEC <= -2.0 ~ -2,
        TRUE ~ -3
      )
    } else -3
  )

# Final output
final_vars <- c('NSID', 'nssec17', 'nssec18', 'nssec19', 'nssec20', 'nssec25', 'nssec32')
output_df <- full_df %>% select(all_of(final_vars))

output_df <- output_df %>%
  mutate(across(-NSID, ~replace_na(., -3)))

output_df <- output_df %>% filter(!is.na(NSID))

readr::write_csv(output_df, 'data/output/cleaned_data.csv')
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
file1 <- 'data/input/wave_one_lsype_family_background_2020.tab'
file2 <- 'data/input/wave_two_lsype_family_background_2020.tab'
file3 <- 'data/input/wave_three_lsype_family_background_2020.tab'
file4 <- 'data/input/wave_four_lsype_family_background_2020.tab'

data1 <- readr::read_delim(file1, delim = '\t', col_types = readr::cols())
data2 <- readr::read_delim(file2, delim = '\t', col_types = readr::cols())
data3 <- readr::read_delim(file3, delim = '\t', col_types = readr::cols())
data4 <- readr::read_delim(file4, delim = '\t', col_types = readr::cols())

# Merge datasets
full_df <- data1 %>%
  full_join(data2, by = 'NSID') %>%
  full_join(data3, by = 'NSID') %>%
  full_join(data4, by = 'NSID')

# Function to harmonize missing values and categories based on metadata
harmonize_ecoact <- function(var_name, data) {
  vec <- data[[var_name]]
  
  # Initialize result vector with NA
  res <- rep(NA, length(vec))
  
  # Use which() or handle NAs in logical indexing to avoid "NAs are not allowed in subscripted assignments"
  # Substantive values: 1-9 remain 1-9
  idx_subst <- which(vec >= 1 & vec <= 9)
  res[idx_subst] <- vec[idx_subst]
  
  # Missing values mapping
  # -999.0: Missing household information - lost -> -2
  res[which(vec == -999)] <- -2
  
  # -99.0: Not interviewed -> -3
  res[which(vec == -99)] <- -3
  
  # -98.0: Not present -> -1
  res[which(vec == -98)] <- -1
  
  # -94.0: Insufficient information -> -8
  res[which(vec == -94)] <- -8
  
  # Special cases for Wave 4
  if (var_name == 'w4empsdad') {
    # -996.0: No parent in household -> -1
    res[which(vec == -996)] <- -1
    # -92.0: Refusal -> -9
    res[which(vec == -92)] <- -9
  }
  
  # Default remaining NA to -3
  res[is.na(res)] <- -3
  
  return(res)
}

# Process each wave/parent
full_df <- full_df %>%
  mutate(
    ecoactma14 = harmonize_ecoact('W1empsmum', .),
    ecoactpa14 = harmonize_ecoact('W1empsdad', .),
    ecoactma15 = harmonize_ecoact('W2empsmum', .),
    ecoactpa15 = harmonize_ecoact('W2empsdad', .),
    ecoactma16 = harmonize_ecoact('W3empsmum', .),
    ecoactpa16 = harmonize_ecoact('W3empsdad', .),
    ecoactma17 = harmonize_ecoact('w4empsmum', .),
    ecoactpa17 = harmonize_ecoact('w4empsdad', .)
  )

# Select only ID and the derived variables
final_df <- full_df %>%
  select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

readr::write_csv(final_df, 'data/output/cleaned_data.csv')

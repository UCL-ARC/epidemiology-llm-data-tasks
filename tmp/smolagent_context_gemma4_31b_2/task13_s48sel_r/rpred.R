library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
file1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
file2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
file3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
file4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
file5 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))

# Merge datasets
full_data <- file1 %>%
  full_join(file2, by = 'NSID') %>%
  full_join(file3, by = 'NSID') %>%
  full_join(file4, by = 'NSID') %>%
  full_join(file5, by = 'NSID')

# Define NS-SEC 17-category labels
nssec_labels <- c(
  '1' = 'Employers in large organisations',
  '2' = 'Higher managerial occupations',
  '3' = 'Higher professional',
  '4' = 'Lower professional',
  '5' = 'Lower managerial occupations',
  '6' = 'Higher supervisory occupations',
  '7' = 'Intermediate',
  '8' = 'Employers in small orgs',
  '9' = 'Own account workers',
  '10' = 'Lower supervisory occupations',
  '11' = 'Lower technical craft',
  '12' = 'Semi routine',
  '13' = 'Routine',
  '14' = 'Never worked/Unemployed',
  '15' = 'Full-time students',
  '16' = 'Not classified or inadequately stated',
  '17' = 'Not classifiable for other reasons'
)

# Function to process NS-SEC variables
process_nssec <- function(var_name, output_name) {
  # Convert to numeric first
  vec <- as.numeric(full_data[[var_name]])
  
  # 1. Handle specific missing value: -98 -> -3
  vec[vec == -98] <- -3
  
  # 2. Handle other missing values from metadata
  # -999, -99, -94 etc. should be mapped according to general guidance
  # Let's use the general guidance mapping for the remaining negatives
  vec[vec == -999] <- -2
  vec[vec == -99] <- -3
  vec[vec == -94] <- -8
  
  # 3. For valid positive values, take the integer part (collapse fractional codes)
  # Create a result vector initialized with the processed missing values
  res <- vec
  valid_idx <- which(vec >= 1)
  res[valid_idx] <- floor(vec[valid_idx])
  
  # 4. Map NAs to -3
  res[is.na(res)] <- -3
  
  # 5. Convert to factor with labels
  # We need to include the missing value codes in the factor levels
  # Based on guidance: -9 Refusal, -8 Don't know, -7 Prefer not to say, -3 Not asked, -2 Schedule error, -1 Not applicable
  levels_vals <- c(-9, -8, -7, -3, -2, -1, 1:17)
  levels_labs <- c('Refusal', 'Don\'t know', 'Prefer not to say', 'Not asked', 'Schedule error', 'Not applicable', nssec_labels)
  
  # Ensure all values are within the defined levels
  res <- pmax(pmin(res, 17), -9)
  
  factor_res <- factor(res, levels = levels_vals, labels = levels_labs)
  return(factor_res)
}

# Apply processing to the 10 required variables
final_df <- full_data %>%
  select(NSID) %>%
  mutate(
    nssecma14 = process_nssec('W1nsseccatmum', 'nssecma14'),
    nssecpa14 = process_nssec('W1nsseccatdad', 'nssecpa14'),
    nssecma15 = process_nssec('W2nsseccatmum', 'nssecma15'),
    nssecpa15 = process_nssec('W2nsseccatdad', 'nssecpa15'),
    nssecma16 = process_nssec('W3cnsseccatmum', 'nssecma16'),
    nssecpa16 = process_nssec('W3cnsseccatdad', 'nssecpa16'),
    nssecma17 = process_nssec('w4cnsseccatmum', 'nssecma17'),
    nssecpa17 = process_nssec('w4cnsseccatdad', 'nssecpa17'),
    nssecma18 = process_nssec('w5Cnsseccatmum', 'nssecma18'),
    nssecpa18 = process_nssec('w5Cnsseccatdad', 'nssecpa18')
  )

# Write output
write_csv(final_df, 'data/output/cleaned_data.csv')

library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- readr::read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'character'))
wave2 <- readr::read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'character'))
wave3 <- readr::read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'character'))
wave4 <- readr::read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'character'))

# Convert necessary columns to numeric
wave1 <- wave1 %>% mutate(across(c(W1empsmum, W1empsdad), as.numeric))
wave2 <- wave2 %>% mutate(across(c(W2empsmum, W2empsdad), as.numeric))
wave3 <- wave3 %>% mutate(across(c(W3empsmum, W3empsdad), as.numeric))
wave4 <- wave4 %>% mutate(across(c(w4empsmum, w4empsdad), as.numeric))

# Merge datasets
data <- wave1 %>% 
  full_join(wave2, by = 'NSID') %>% 
  full_join(wave3, by = 'NSID') %>% 
  full_join(wave4, by = 'NSID')

# Helper function to harmonise missing values based on labels provided in metadata
harmonise_ecoact <- function(x) {
  res <- rep(-3, length(x)) # Default NA to -3
  
  # We must handle NAs explicitly in logical comparisons to avoid "NAs are not allowed in subscripted assignments"
  valid_indices <- which(x >= 1 & x <= 9)
  res[valid_indices] <- x[valid_indices]
  
  # Specific missing codes
  res[which(x == -999)] <- -2
  res[which(x == -99)] <- -3
  res[which(x == -98)] <- -1
  res[which(x == -94)] <- -8
  res[which(x == -92)] <- -9
  res[which(x == -996)] <- -1
  
  return(res)
}

# Apply harmonisation
data <- data %>%
  mutate(
    ecoactma14 = harmonise_ecoact(W1empsmum),
    ecoactpa14 = harmonise_ecoact(W1empsdad),
    ecoactma15 = harmonise_ecoact(W2empsmum),
    ecoactpa15 = harmonise_ecoact(W2empsdad),
    ecoactma16 = harmonise_ecoact(W3empsmum),
    ecoactpa16 = harmonise_ecoact(W3empsdad),
    ecoactma17 = harmonise_ecoact(w4empsmum),
    ecoactpa17 = harmonise_ecoact(w4empsdad)
  )

# Define labels
eco_labels <- c(
  '1' = 'Doing paid work for 30 or more hours a week',
  '2' = 'Doing paid work for fewer than 30 hours a week',
  '3' = 'Unemployed/ Looking for a job',
  '4' = 'On a training course or scheme',
  '5' = 'In full-time education/ at school',
  '6' = 'Looking after the family/ household',
  '7' = 'Retired from work altogether',
  '8' = 'Sick/ disabled',
  '9' = 'Other',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know / insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

# Apply labels as factors
final_vars <- c('ecoactma14', 'ecoactpa14', 'ecoactma15', 'ecoactpa15', 'ecoactma16', 'ecoactpa16', 'ecoactma17', 'ecoactpa17')

for(var in final_vars) {
  data[[var]] <- factor(data[[var]], levels = as.numeric(names(eco_labels)), labels = eco_labels)
}

# Select final columns
final_data <- data %>%
  select(NSID, all_of(final_vars))

# Write to CSV
readr::write_csv(final_data, 'data/output/cleaned_data.csv')

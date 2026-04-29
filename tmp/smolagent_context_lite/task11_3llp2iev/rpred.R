library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load files
file_w1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
file_w2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
file_w3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
file_w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))

# Convert relevant columns to numeric
file_w1 <- file_w1 %>% mutate(across(c(W1empsmum, W1empsdad), as.numeric))
file_w2 <- file_w2 %>% mutate(across(c(W2empsmum, W2empsdad), as.numeric))
file_w3 <- file_w3 %>% mutate(across(c(W3empsmum, W3empsdad), as.numeric))
file_w4 <- file_w4 %>% mutate(across(c(w4empsmum, w4empsdad), as.numeric))

# Merge datasets
df <- file_w1 %>%
  full_join(file_w2, by = 'NSID') %>%
  full_join(file_w3, by = 'NSID') %>%
  full_join(file_w4, by = 'NSID')

# Harmonisation function for missing values based on metadata
harmonise_ecoact <- function(x, wave) {
  # Standard missing codes:
  # -9 Refusal, -8 Don't know, -7 Prefer not to say, -3 Not asked, -2 Schedule not applicable/lost, -1 Item not applicable
  
  res <- x
  # Generic mapping based on metadata labels provided in the prompt
  # -999.0: Missing household information - lost -> -2
  # -99.0: Not interviewed -> -3
  # -98.0: Not present -> -3 (or -1 depending on context, but usually treated as not asked/not present)
  # -94.0: Insufficient information -> -8
  # -92.0: Refusal (found in W4) -> -9
  # -996.0: No parent in household (found in W4) -> -1
  
  res[x == -999] <- -2
  res[x == -99] <- -3
  res[x == -98] <- -3
  res[x == -94] <- -8
  res[x == -92] <- -9
  res[x == -996] <- -1
  
  # Convert NA to -3
  res[is.na(res)] <- -3
  return(res)
}

# Define labels for the factor
ecoact_labels <- c(
  "1" = "Doing paid work for 30 or more hours a week",
  "2" = "Doing paid work for fewer than 30 hours a week",
  "3" = "Unemployed/ Looking for a job",
  "4" = "On a training course or scheme",
  "5" = "In full-time education/ at school",
  "6" = "Looking after the family/ household",
  "7" = "Retired from work altogether",
  "8" = "Sick/ disabled",
  "9" = "Other",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

# Apply harmonisation and create factors
process_var <- function(var_name) {
  vals <- harmonise_ecoact(df[[var_name]], "")
  # Create factor with specified levels
  f <- factor(vals, levels = as.numeric(names(ecoact_labels)), labels = ecoact_labels)
  return(f)
}

final_df <- df %>%
  mutate(
    ecoactma14 = process_var('W1empsmum'),
    ecoactpa14 = process_var('W1empsdad'),
    ecoactma15 = process_var('W2empsmum'),
    ecoactpa15 = process_var('W2empsdad'),
    ecoactma16 = process_var('W3empsmum'),
    ecoactpa16 = process_var('W3empsdad'),
    ecoactma17 = process_var('w4empsmum'),
    ecoactpa17 = process_var('w4empsdad')
  ) %>%
  select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

write_csv(final_df, 'data/output/cleaned_data.csv')

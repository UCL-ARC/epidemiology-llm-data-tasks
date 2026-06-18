library(readr)
library(dplyr)
library(purrr)

# Helper: map raw codes to the standard missing‑value scheme
map_codes <- function(x) {
  case_when(
    x %in% 1:9                          ~ x,                    # substantive codes
    x %in% c(-99, -98, -996)           ~ -3,                    # not asked / not interviewed
    x == -94                            ~ -8,                    # insufficient information
    x == -999                           ~ -2,                    # missing household info / lost
    x == -92                            ~ -9,                    # refusal
    TRUE                                ~ NA_real_                # leave other NAs as NA
  )
}

# Read each wave, keep only the variables we need
root <- 'data/input/'

# Wave 1 – age 14
w1 <- read_delim(file.path(root, 'wave_one_lsype_family_background_2020.tab'),
                 delim = '\t',
                 col_types = cols(NSID = col_character(), .default = col_double())) %>%
  mutate(
    ecoactma14 = map_codes(W1empsmum),
    ecoactpa14 = map_codes(W1empsdad)
  ) %>%
  select(NSID, ecoactma14, ecoactpa14)

# Wave 2 – age 15
w2 <- read_delim(file.path(root, 'wave_two_lsype_family_background_2020.tab'),
                 delim = '\t',
                 col_types = cols(NSID = col_character(), .default = col_double())) %>%
  mutate(
    ecoactma15 = map_codes(W2empsmum),
    ecoactpa15 = map_codes(W2empsdad)
  ) %>%
  select(NSID, ecoactma15, ecoactpa15)

# Wave 3 – age 16
w3 <- read_delim(file.path(root, 'wave_three_lsype_family_background_2020.tab'),
                 delim = '\t',
                 col_types = cols(NSID = col_character(), .default = col_double())) %>%
  mutate(
    ecoactma16 = map_codes(W3empsmum),
    ecoactpa16 = map_codes(W3empsdad)
  ) %>%
  select(NSID, ecoactma16, ecoactpa16)

# Wave 4 – age 17
w4 <- read_delim(file.path(root, 'wave_four_lsype_family_background_2020.tab'),
                 delim = '\t',
                 col_types = cols(NSID = col_character(), .default = col_double())) %>%
  mutate(
    ecoactma17 = map_codes(w4empsmum),
    ecoactpa17 = map_codes(w4empsdad)
  ) %>%
  select(NSID, ecoactma17, ecoactpa17)

# Merge all waves
cleaned <- full_join(w1, w2, by = 'NSID') %>%
  full_join(w3, by = 'NSID') %>%
  full_join(w4, by = 'NSID')

# Convert to labelled factors with consistent category ordering
levels_vec <- c(1:9, -3, -8, -2, -9)
labels_vec <- c(
  'Doing paid work for 30 or more hours a week',
  'Doing paid work for fewer than 30 hours a week',
  'Unemployed/ Looking for a job',
  'On a training course or scheme',
  'In full-time education/ at school',
  'Looking after the family/ household',
  'Retired from work altogether',
  'Sick/ disabled',
  'Other',
  'Not asked at the fieldwork stage / not interviewed',
  'Insufficient information',
  'Missing household information',
  'Refusal'
)

vars <- c(
  'ecoactma14','ecoactpa14',
  'ecoactma15','ecoactpa15',
  'ecoactma16','ecoactpa16',
  'ecoactma17','ecoactpa17'
)
cleaned <- cleaned %>%
  mutate(across(all_of(vars), ~ factor(.x,
                                      levels = levels_vec,
                                      labels = labels_vec,
                                      ordered = FALSE)))

# Write to CSV
write_csv(cleaned, 'data/output/cleaned_data.csv')
cat('Data written to data/output/cleaned_data.csv\n')
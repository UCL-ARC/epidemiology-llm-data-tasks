library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
ns8_derived <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')
ns9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Full join all datasets
full_data <- full_join(wave1, wave4, by = 'NSID')
full_data <- full_join(full_data, ns8, by = 'NSID')
full_data <- full_join(full_data, ns8_derived, by = 'NSID')
full_data <- full_join(full_data, ns9, by = 'NSID')
full_data <- full_join(full_data, ns9_derived, by = 'NSID')

# Create educ25
full_data <- full_data %>%
  mutate(
    W8DHANVQH_mapped = case_when(
      W8DHANVQH == 95 ~ 0L, W8DHANVQH == 96 ~ 4L,
      W8DHANVQH == 4 ~ 0L, W8DHANVQH == 5 ~ 0L,
      W8DHANVQH == 3 ~ 1L, W8DHANVQH == 2 ~ 1L, W8DHANVQH == 1 ~ 1L,
      TRUE ~ NA_integer_
    ),
    W8VCQU0A = case_when(W8VCQU0A == 1 ~ 2L, TRUE ~ NA_integer_),
    W8VCQU0B = case_when(W8VCQU0B == 1 ~ 2L, TRUE ~ NA_integer_),
    W8VCQU0C = case_when(W8VCQU0C == 1 ~ 2L, TRUE ~ NA_integer_),
    W8VCQU0D = case_when(W8VCQU0D == 1 ~ 2L, TRUE ~ NA_integer_),
    W8VCQU0E = case_when(W8VCQU0E == 1 ~ 2L, TRUE ~ NA_integer_),
    W8VCQU0F = case_when(W8VCQU0F == 1 ~ 1L, TRUE ~ NA_integer_),
    W8VCQU0G = case_when(W8VCQU0G == 1 ~ 1L, TRUE ~ NA_integer_),
    W8VCQU0H = case_when(W8VCQU0H == 1 ~ 1L, TRUE ~ NA_integer_),
    W8VCQU0I = case_when(W8VCQU0I == 1 ~ 2L, TRUE ~ NA_integer_),
    W8VCQU0J = case_when(W8VCQU0J == 1 ~ 1L, TRUE ~ NA_integer_),
    W8VCQU0K = case_when(W8VCQU0K == 1 ~ 1L, TRUE ~ NA_integer_),
    W8VCQU0L = case_when(W8VCQU0L == 1 ~ 1L, TRUE ~ NA_integer_),
    W8VCQU0M = case_when(W8VCQU0M == 1 ~ 1L, TRUE ~ NA_integer_),
    W8VCQU0N = case_when(W8VCQU0N == 1 ~ 1L, TRUE ~ NA_integer_),
    W8VCQU0O = case_when(W8VCQU0O == 1 ~ 1L, TRUE ~ NA_integer_),
    W8VCQU0P = case_when(W8VCQU0P == 1 ~ 4L, TRUE ~ NA_integer_),
    W8VCQU0Q = case_when(W8VCQU0Q == -8 ~ -8L, TRUE ~ NA_integer_),
    W8VCQU0R = case_when(W8VCQU0R == -9 ~ -9L, TRUE ~ NA_integer_)
  ) %>%
  mutate(
    educ25 = case_when(
      !is.na(W8DHANVQH_mapped) & !is.na(W8VCQU0A) ~ pmax(W8DHANVQH_mapped, W8VCQU0A, W8VCQU0B, W8VCQU0C, W8VCQU0D, W8VCQU0E, W8VCQU0F, W8VCQU0G, W8VCQU0H, W8VCQU0I, W8VCQU0J, W8VCQU0K, W8VCQU0L, W8VCQU0M, W8VCQU0N, W8VCQU0O, W8VCQU0P, W8VCQU0Q, W8VCQU0R, na.rm = TRUE),
      !is.na(W8DHANVQH_mapped) ~ W8DHANVQH_mapped,
      TRUE ~ NA_integer_
    )
  ) %>%
  mutate(educ25 = factor(educ25, levels = c(-9L, -8L, -1L, 0L, 1L, 2L, 3L, 4L),
    labels = c('Refused', 'Don\'t know', 'Not applicable',
               'NVQ 4-5 equivalent', 'NVQ 1-3 equivalent',
               'Entry level or no qualifications',
               'Other qualifications not mappable to NVQ',
               'None of these qualifications')))

# Create educ32
full_data <- full_data %>%
  mutate(
    W9DANVQH_mapped = case_when(
      W9DANVQH == 0 ~ 2L, W9DANVQH == 1 ~ 1L, W9DANVQH == 2 ~ 1L,
      W9DANVQH == 3 ~ 1L, W9DANVQH == 4 ~ 0L, W9DANVQH == 5 ~ 0L,
      W9DANVQH == 95 ~ 3L, W9DANVQH == 96 ~ 4L,
      TRUE ~ as.integer(NA)
    ),
    W9DVNVQH_mapped = case_when(
      W9DVNVQH == 0 ~ 2L, W9DVNVQH == 1 ~ 1L, W9DVNVQH == 2 ~ 1L,
      W9DVNVQH == 3 ~ 1L, W9DVNVQH == 4 ~ 0L, W9DVNVQH == 5 ~ 0L,
      W9DVNVQH == 95 ~ 3L, W9DVNVQH == 96 ~ 4L,
      TRUE ~ as.integer(NA)
    )
  ) %>%
  mutate(
    educ32 = case_when(
      !is.na(W9DANVQH_mapped) & !is.na(W9DVNVQH_mapped) ~ pmax(W9DANVQH_mapped, W9DVNVQH_mapped),
      is.na(W9DANVQH_mapped) ~ W9DVNVQH_mapped,
      is.na(W9DVNVQH_mapped) ~ W9DANVQH_mapped,
      TRUE ~ as.integer(NA)
    )
  ) %>%
  mutate(educ32 = factor(educ32, levels = c(-9L, -8L, -1L, 0L, 1L, 2L, 3L, 4L),
    labels = c('Refused', 'Insufficient information', 'Not applicable',
               'NVQ 4-5 equivalent', 'NVQ 1-3 equivalent',
               'Entry level or no qualifications',
               'Other qualifications not mappable to NVQ',
               'None of these qualifications')))

# Create educadtl32
full_data <- full_data %>%
  mutate(
    first_qual = case_when(
      W9ACQU0A == 1 ~ 5L, W9ACQU0B == 1 ~ 4L, W9ACQU0C == 1 ~ 3L,
      W9ACQU0D == 1 ~ 2L, W9ACQU0E == 1 ~ 2L, W9ACQU0F == 1 ~ 2L,
      W9ACQU0G == 1 ~ 1L, W9ACQU0H == 1 ~ 1L, W9ACQU0I == 1 ~ 1L,
      W9ACQU0J == 1 ~ 1L, W9ACQU0K == 1 ~ 1L, W9ACQU0L == 1 ~ 1L,
      W9ACQU0M == 1 ~ 1L, W9ACQU0N == 1 ~ 1L, W9ACQU0O == 1 ~ 1L,
      W9ACQU0P == 1 ~ 1L, W9ACQU0Q == 1 ~ 1L, W9ACQU0R == 1 ~ 1L,
      W9ACQU0S == 1 ~ 0L, W9ACQU0T == -8 ~ -8L, W9ACQU0U == -9 ~ -9L,
      W9ACQU0V == -2 ~ -2L,
      W9ACQU0A == -1 | W9ACQU0B == -1 | W9ACQU0C == -1 | W9ACQU0D == -1 |
      W9ACQU0E == -1 | W9ACQU0F == -1 | W9ACQU0G == -1 | W9ACQU0H == -1 |
      W9ACQU0I == -1 | W9ACQU0J == -1 | W9ACQU0K == -1 | W9ACQU0L == -1 |
      W9ACQU0M == -1 | W9ACQU0N == -1 | W9ACQU0O == -1 | W9ACQU0P == -1 |
      W9ACQU0Q == -1 | W9ACQU0R == -1 ~ -1L,
      W9ACQU0S == -1 | W9ACQU0S == -3 ~ -1L,
      TRUE ~ 2L
    )
  ) %>%
  mutate(
    educadtl32 = case_when(
      first_qual >= 1L ~ 1L,
      first_qual == -8L ~ -8L, first_qual == -9L ~ -9L,
      first_qual == -2L ~ -2L, first_qual == -1L ~ -1L,
      first_qual == 0L ~ 2L,
      TRUE ~ NA_integer_
    )
  )

# Create educvdtl32
full_data <- full_data %>%
  mutate(
    first_qual = case_when(
      W9VCQU0A == 1 ~ 4L, W9VCQU0B == 1 ~ 2L, W9VCQU0C == 1 ~ 3L,
      W9VCQU0D == 1 ~ 3L, W9VCQU0E == 1 ~ 3L, W9VCQU0F == 1 ~ 3L,
      W9VCQU0G == 1 ~ 3L, W9VCQU0H == 1 ~ 3L, W9VCQU0I == 1 ~ 3L,
      W9VCQU0J == 1 ~ 3L, W9VCQU0K == 1 ~ 2L, W9VCQU0L == 1 ~ 3L,
      W9VCQU0M == 1 ~ 2L, W9VCQU0N == 1 ~ 2L, W9VCQU0O == 1 ~ 3L,
      W9VCQU0P == 1 ~ 3L, W9VCQU0Q == 1 ~ 3L, W9VCQU0R == 1 ~ 3L,
      W9VCQU0S == 1 ~ 3L, W9VCQU0T == 1 ~ 3L, W9VCQU0U == 1 ~ 3L,
      W9VCQU0V == 1 ~ 3L, W9VCQU0W == 1 ~ 3L, W9VCQU0X == 1 ~ 2L,
      W9VCQU0Y == 1 ~ 2L, W9VCQU0Z == 1 ~ 2L, W9VCQUAA == 1 ~ 2L,
      W9VCQUAB == 1 ~ 2L, W9VCQUAC == 1 ~ 3L, W9VCQUAD == 1 ~ 3L,
      W9VCQUAE == 1 ~ 1L, W9VCQUAF == 1 ~ 3L, W9VCQUAG == 1 ~ 0L,
      W9VCQUAH == -8 ~ -8L, W9VCQUAI == -9 ~ -9L,
      TRUE ~ 2L
    )
  ) %>%
  mutate(
    educvdtl32 = case_when(
      first_qual >= 1L ~ 1L,
      first_qual == -8L ~ -8L, first_qual == -9L ~ -9L,
      first_qual == -1L ~ -1L, first_qual == 0L ~ 2L,
      TRUE ~ NA_integer_
    )
  )

# Select final variables
final_data <- full_data %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

print('Script completed successfully')
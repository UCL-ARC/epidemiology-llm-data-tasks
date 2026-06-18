library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
ns8_main <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
ns8_derived <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')
ns9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge all datasets
all_data <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(ns8_main, by = 'NSID') %>%
  full_join(ns8_derived, by = 'NSID') %>%
  full_join(ns9_main, by = 'NSID') %>%
  full_join(ns9_derived, by = 'NSID')

# Create educ25 from W8DHANVQH (Age 25)
# W8DHANVQH values: 1=L1, 2=L2, 3=L3, 4=L4, 5=L5, 95=Other academic, 96=None
# Harmonise to 5-level NVQ scheme
all_data <- all_data %>%
  mutate(educ25 = case_when(
    W8DHANVQH %in% c(-9, -8, -1) ~ -3,
    W8DHANVQH == 96 ~ -1,
    W8DHANVQH == 95 ~ -2,
    W8DHANVQH %in% c(1:5) ~ as.integer(W8DHANVQH),
    TRUE ~ NA_integer_))

# Create educ32 - combine W9DANVQH (academic) and W9DVNVQH (vocational) for Age 32
# Use higher NVQ level
all_data <- all_data %>%
  mutate(educ32 = case_when(
    is.na(W9DANVQH) & is.na(W9DVNVQH) ~ NA_real_,
    is.na(W9DANVQH) ~ W9DVNVQH,
    is.na(W9DVNVQH) ~ W9DANVQH,
    TRUE ~ pmax(W9DANVQH, W9DVNVQH, na.rm = TRUE))) %>%
  mutate(educ32 = case_when(
    educ32 %in% c(-9, -8, -1) ~ -3,
    educ32 %in% c(95, 96) ~ -2,
    educ32 == 96 ~ -1,
    TRUE ~ as.integer(educ32)))

# Create educadtl32 - detailed academic qualifications at age 32
# Categorise based on academic qualification variables
all_data <- all_data %>%
  mutate(educadtl32 = case_when(
    # Degree level qualifications
    (W9ACQU0A == 1 | W9ACQU0B == 1 | W9ACQU0C == 1 | W9ACQU0D == 1 | W9ACQU0E == 1 | W9ACQU0F == 1) ~ 1,
    # Non-degree academic qualifications (A-Levels, SCE, National, etc.)
    (W9ACQU0G == 1 | W9ACQU0H == 1 | W9ACQU0I == 1 | W9ACQU0J == 1 | W9ACQU0K == 1 | 
     W9ACQU0L == 1 | W9ACQU0M == 1 | W9ACQU0N == 1 | W9ACQU0O == 1 | W9ACQU0P == 1 | 
     W9ACQU0Q == 1 | W9ACQU0R == 1) ~ 2,
    # None of these qualifications
    W9ACQU0S == 1 ~ -1,
    # Don't know
    W9ACQU0T == 1 ~ -8,
    # Refused
    W9ACQU0U == 1 ~ -9,
    # Other missing values
    W9ACQU0V == 1 ~ -3,
    TRUE ~ -3))

# Create educvdtl32 - detailed vocational qualifications at age 32
all_data <- all_data %>%
  mutate(educvdtl32 = case_when(
    # Professional qualifications at degree level
    W9VCQU0A == 1 ~ 1,
    # Nursing/medical qualifications (below degree)
    W9VCQU0B == 1 ~ 1,
    # Level 4 or 5
    W9VCQU0C == 1 ~ 1,
    # Level 3
    W9VCQU0D == 1 | W9VCQU0I == 1 | W9VCQU0O == 1 ~ 1,
    # Level 2
    W9VCQU0E == 1 | W9VCQU0J == 1 | W9VCQU0P == 1 ~ 1,
    # Level 1
    W9VCQU0F == 1 | W9VCQU0Q == 1 ~ 1,
    # GNVQ Advanced
    W9VCQU0G == 1 ~ 1,
    # GNVQ Intermediate
    W9VCQU0H == 1 ~ 1,
    # Advanced Craft, Part III
    W9VCQU0L == 1 ~ 1,
    # Craft, Part II
    W9VCQU0M == 1 ~ 1,
    # Craft, Part I
    W9VCQU0N == 1 ~ 1,
    # Advanced Diploma
    W9VCQU0R == 1 ~ 1,
    # Higher Diploma
    W9VCQU0S == 1 ~ 1,
    # RSA Diploma
    W9VCQU0T == 1 ~ 1,
    # RSA Stage I, II, III
    W9VCQU0U == 1 ~ 1,
    # Higher Level BTEC
    W9VCQU0V == 1 ~ 1,
    # BTEC National
    W9VCQU0W == 1 ~ 1,
    # Other vocational qualifications
    W9VCQUAF == 1 ~ 1,
    # None of these qualifications
    W9VCQUAG == 1 ~ -1,
    # Don't know
    W9VCQUAH == 1 ~ -8,
    # Refused
    W9VCQUAI == 1 ~ -9,
    TRUE ~ -3))

# Final output
final_data <- all_data %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write output
write_csv(final_data, 'data/output/cleaned_data.csv')

print('Script completed successfully')
print(paste('Rows:', nrow(final_data)))
print(paste('Columns:', ncol(final_data)))
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_main_interview.tab',
  'ns9_2022_derived_variables.tab'
)

all_data <- list()
for (f in files) {
  all_data[[f]] <- read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(.default = 'c'))
}

# Merge all datasets by NSID
cohort_frame <- all_data[[1]] %>% select(NSID)
for (i in 2:length(all_data)) {
  cohort_frame <- full_join(cohort_frame, all_data[[i]], by = 'NSID')
}

# Helper function to convert numeric strings to numeric
conv_num <- function(x) as.numeric(x)

# 2. Process Age 25 (Wave 8)
# Target: educ25 (5-level NVQ scheme)
# Source: W8DHANVQH (Highest NVQ level from academic qualification)
# Note: We also need to consider vocational qualifications for the overall highest level
# W8DHANVQH values: 1:L1, 2:L2, 3:L3, 4:L4, 5:L5, 95:Other, 96:None
# Vocational: W8VCQU0I (L1-2), W8VCQU0J (L3-5), W8VCQU0K (HNC/HND - L4/5), etc.

# First, clean the derived academic variable
cohort_frame <- cohort_frame %>%
  mutate(
    w8_acad_nvq = conv_num(W8DHANVQH),
    # Map missing for w8_acad_nvq
    w8_acad_nvq = case_when(
      w8_acad_nvq == -9 ~ -9,
      w8_acad_nvq == -8 ~ -8,
      w8_acad_nvq == -1 ~ -1,
      TRUE ~ w8_acad_nvq
    )
  )

# Process Vocational for Age 25 (Detailed logic needed for a full NVQ mapping)
# Since we need educ25 (5-level NVQ), let's use the logic: 
# Max(Academic NVQ, Vocational NVQ)
# For simplicity in this task, if W8DHANVQH is the provided derived academic NVQ,
# we check if vocational qualifications provided a higher level.

# Map Vocational variables to NVQ levels for Age 25
# W8VCQU0J: L3-5, W8VCQU0K: HNC/HND (L4/5), W8VCQU0I: L1-2
# We need to identify the highest vocational level

cohort_frame <- cohort_frame %>%
  mutate(
    v_l5 = if_else(conv_num(W8VCQU0K) == 1, 5, 0),
    v_l3 = if_else(conv_num(W8VCQU0J) == 1, 3, 0),
    v_l2 = if_else(conv_num(W8VCQU0I) == 1, 2, 0),
    v_none = if_else(conv_num(W8VCQU0P) == 1, 0, 0),
    w8_voc_nvq = pmax(v_l5, v_l3, v_l2, v_none)
  )

# Combine Academic and Vocational for educ25
cohort_frame <- cohort_frame %>%
  mutate(
    educ25_raw = pmax(w8_acad_nvq, w8_voc_nvq, na.rm = TRUE),
    educ25 = case_when(
      educ25_raw == 1 ~ 1,
      educ25_raw == 2 ~ 2,
      educ25_raw == 3 ~ 3,
      educ25_raw == 4 ~ 4,
      educ25_raw == 5 ~ 5,
      educ25_raw == 96 | educ25_raw == 0 ~ 0,
      educ25_raw == 95 ~ 0, # Other academic mapped to 0 or lowest if not NVQ
      educ25_raw == -9 ~ -9,
      educ25_raw == -8 ~ -8,
      educ25_raw == -1 ~ -1,
      TRUE ~ -3
    )
  )

# 3. Process Age 32 (Wave 9)
# Target: educ32 (5-level NVQ), educadtl32, educvdtl32

# Academic detailed: W9ACQU0A to W9ACQU0S
# Vocational detailed: W9VCQU0A to W9VCQUAG

# Higher NVQ level from academic (W9DANVQH) and vocational (W9DVNVQH)
cohort_frame <- cohort_frame %>%
  mutate(
    w9_acad_nvq = conv_num(W9DANVQH),
    w9_voc_nvq = conv_num(W9DVNVQH),
    w9_acad_nvq = case_when(
      w9_acad_nvq == -9 ~ -9, w9_acad_nvq == -8 ~ -8, w9_acad_nvq == -1 ~ -1, TRUE ~ w9_acad_nvq
    ),
    w9_voc_nvq = case_when(
      w9_voc_nvq == -9 ~ -9, w9_voc_nvq == -8 ~ -8, w9_voc_nvq == -1 ~ -1, TRUE ~ w9_voc_nvq
    )
  )

cohort_frame <- cohort_frame %>%
  mutate(
    educ32_raw = pmax(w9_acad_nvq, w9_voc_nvq, na.rm = TRUE),
    educ32 = case_when(
      educ32_raw == 1 ~ 1,
      educ32_raw == 2 ~ 2,
      educ32_raw == 3 ~ 3,
      educ32_raw == 4 ~ 4,
      educ32_raw == 5 ~ 5,
      educ32_raw == 96 | educ32_raw == 0 ~ 0,
      educ32_raw == 95 ~ 0,
      educ32_raw == -9 ~ -9,
      educ32_raw == -8 ~ -8,
      educ32_raw == -1 ~ -1,
      TRUE ~ -3
    )
  )

# Detailed Academic Age 32 (educadtl32)
# Use the raw source indicators. If 'Yes' (1), it's that qualification.
# We create a combined string or a primary indicator. Usually 'detailed' means the raw set.
# However, requirements ask for 'variables' (plural or singular). Let's create a summary
# or just keep the highest. Given the naming, let's derive a summary code based on highest academic qual.

cohort_frame <- cohort_frame %>%
  mutate(
    educadtl32 = case_when(
      conv_num(W9ACQU0A) == 1 ~ 'Doctorate',
      conv_num(W9ACQU0B) == 1 ~ 'Masters',
      conv_num(W9ACQU0C) == 1 ~ 'Undergraduate',
      conv_num(W9ACQU0D) == 1 ~ 'PG Diploma',
      conv_num(W9ACQU0E) == 1 ~ 'Higher Ed Diploma',
      conv_num(W9ACQU0F) == 1 ~ 'Teaching Qual',
      conv_num(W9ACQU0G) == 1 ~ 'A-Levels',
      conv_num(W9ACQU0H) == 1 ~ 'GCSE ABC',
      conv_num(W9ACQU0I) == 1 ~ 'GCSE DEG',
      conv_num(W9ACQU0S) == 1 ~ 'None',
      TRUE ~ NA_character_
    )
  )

# Detailed Vocational Age 32 (educvdtl32)
cohort_frame <- cohort_frame %>%
  mutate(
    educvdtl32 = case_when(
      conv_num(W9VCQU0A) == 1 ~ 'Professional Degree',
      conv_num(W9VCQU0C) == 1 ~ 'Level 4/5',
      conv_num(W9VCQU0D) == 1 ~ 'Level 3',
      conv_num(W9VCQU0E) == 1 ~ 'Level 2',
      conv_num(W9VCQU0F) == 1 ~ 'Level 1',
      conv_num(W9VCQUAG) == 1 ~ 'None',
      TRUE ~ NA_character_
    )
  )

# Final Cleaning and Labeling
# Mapping for educ25 and educ32: 0: None, 1: L1, 2: L2, 3: L3, 4: L4, 5: L5
# Missing: -9 Refusal, -8 Don't know, -1 Not applicable, -3 Not asked

final_df <- cohort_frame %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Convert missing to standard codes for factors/numeric
final_df <- final_df %>%
  mutate(across(c(educ25, educ32), ~ replace_na(., -3)))

# Write output
write_csv(final_df, 'data/output/cleaned_data.csv')

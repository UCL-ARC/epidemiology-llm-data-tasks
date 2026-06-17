library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_three_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_five_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns9_2022_main_interview.tab'
)

load_data <- function(file) {
  readr::read_delim(paste0('data/input/', file), delim = '\t', col_types = readr::cols(.default = 'numeric'))
}

# Special handling for NSID which is string
load_data_with_nsid <- function(file) {
  readr::read_delim(paste0('data/input/', file), delim = '\t', col_types = readr::cols(NSID = readr::col_character(), .default = 'numeric'))
}

data_list <- lapply(files, load_data_with_nsid)

# Merge datasets
cohort_frame <- data_list %>% reduce(full_join, by = 'NSID')

# Mapping function for missing values based on labels
# Standard codes:
# -9 = Refusal
# -8 = Don't know / insufficient information
# -7 = Prefer not to say
# -3 = Not asked / not interviewed / NA
# -2 = Schedule not applicable / script error / info lost
# -1 = Item not applicable

clean_sex <- function(val, wave_labels) {
  if (is.na(val)) return(-3)
  
  # Create a mapping from the metadata labels to standard codes
  # we iterate through the provided value_labels for each wave
  return(val)
}

# Process sex variables wave by wave
# We use most-recent-valid-first for sex

# Wave 1: W1sexYP
# -99.0: YP not interviewed -> -3
# -92.0: Refused -> -9
# -91.0: Not applicable -> -1
# 1, 2: Male, Female
cohort_frame <- cohort_frame %>% mutate(
  sex_w1 = case_when(
    W1sexYP == 1 ~ 1,
    W1sexYP == 2 ~ 2,
    W1sexYP == -92 ~ -9,
    W1sexYP == -91 ~ -1,
    W1sexYP == -99 ~ -3,
    TRUE ~ -3
  )
)

# Wave 2: W2SexYP
# -998, -997, -995: Missed, Script error, Missing history -> -2
# -99.0: YP not interviewed -> -3
# -92.0: Refused -> -9
# -91.0: Not applicable -> -1
# -1.0: Don't Know -> -8
cohort_frame <- cohort_frame %>% mutate(
  sex_w2 = case_when(
    W2SexYP == 1 ~ 1,
    W2SexYP == 2 ~ 2,
    W2SexYP == -92 ~ -9,
    W2SexYP == -91 ~ -1,
    W2SexYP == -1 ~ -8,
    W2SexYP == -99 ~ -3,
    W2SexYP %in% c(-998, -997, -995) ~ -2,
    TRUE ~ -3
  )
)

# Wave 3: W3sexYP
# -99.0: YP not interviewed -> -3
# -92.0: Refused -> -9
# -91.0: Not applicable -> -1
cohort_frame <- cohort_frame %>% mutate(
  sex_w3 = case_when(
    W3sexYP == 1 ~ 1,
    W3sexYP == 2 ~ 2,
    W3sexYP == -92 ~ -9,
    W3sexYP == -91 ~ -1,
    W3sexYP == -99 ~ -3,
    TRUE ~ -3
  )
)

# Wave 4: W4SexYP
# -99.0: YP not interviewed -> -3
# -92.0: Refused -> -9
# -91.0: Not applicable -> -1
# -1.0: Don't know -> -8
cohort_frame <- cohort_frame %>% mutate(
  sex_w4 = case_when(
    W4SexYP == 1 ~ 1,
    W4SexYP == 2 ~ 2,
    W4SexYP == -92 ~ -9,
    W4SexYP == -91 ~ -1,
    W4SexYP == -1 ~ -8,
    W4SexYP == -99 ~ -3,
    TRUE ~ -3
  )
)

# Wave 5: W5SexYP
# -1.0: Don't know -> -8
cohort_frame <- cohort_frame %>% mutate(
  sex_w5 = case_when(
    W5SexYP == 1 ~ 1,
    W5SexYP == 2 ~ 2,
    W5SexYP == -1 ~ -8,
    TRUE ~ -3
  )
)

# Wave 6: W6Sex
# -92.0: Refused -> -9
# -91.0: Not applicable -> -1
cohort_frame <- cohort_frame %>% mutate(
  sex_w6 = case_when(
    W6Sex == 1 ~ 1,
    W6Sex == 2 ~ 2,
    W6Sex == -92 ~ -9,
    W6Sex == -91 ~ -1,
    TRUE ~ -3
  )
)

# Wave 7: W7Sex
# -91.0: Not applicable -> -1
cohort_frame <- cohort_frame %>% mutate(
  sex_w7 = case_when(
    W7Sex == 1 ~ 1,
    W7Sex == 2 ~ 2,
    W7Sex == -91 ~ -1,
    TRUE ~ -3
  )
)

# Wave 8: W8CMSEX
# -9.0: Refused -> -9
# -8.0: Don't know -> -8
# -1.0: Not applicable -> -1
cohort_frame <- cohort_frame %>% mutate(
  sex_w8 = case_when(
    W8CMSEX == 1 ~ 1,
    W8CMSEX == 2 ~ 2,
    W8CMSEX == -9 ~ -9,
    W8CMSEX == -8 ~ -8,
    W8CMSEX == -1 ~ -1,
    TRUE ~ -3
  )
)

# Wave 9: W9DSEX
cohort_frame <- cohort_frame %>% mutate(
  sex_w9 = case_when(
    W9DSEX == 1 ~ 1,
    W9DSEX == 2 ~ 2,
    TRUE ~ -3
  )
)

# Consolidate: most-recent-valid-first
# Order: w9, w8, w7, w6, w5, w4, w3, w2, w1

cohort_frame <- cohort_frame %>% 
  rowwise() %>% 
  mutate(sex = coalesce(
    case_when(sex_w9 %in% c(1, 2) ~ sex_w9, TRUE ~ NA_real_),
    case_when(sex_w8 %in% c(1, 2) ~ sex_w8, TRUE ~ NA_real_),
    case_when(sex_w7 %in% c(1, 2) ~ sex_w7, TRUE ~ NA_real_),
    case_when(sex_w6 %in% c(1, 2) ~ sex_w6, TRUE ~ NA_real_),
    case_when(sex_w5 %in% c(1, 2) ~ sex_w5, TRUE ~ NA_real_),
    case_when(sex_w4 %in% c(1, 2) ~ sex_w4, TRUE ~ NA_real_),
    case_when(sex_w3 %in% c(1, 2) ~ sex_w3, TRUE ~ NA_real_),
    case_when(sex_w2 %in% c(1, 2) ~ sex_w2, TRUE ~ NA_real_),
    case_when(sex_w1 %in% c(1, 2) ~ sex_w1, TRUE ~ NA_real_)
  )) %>% 
  ungroup()

# If no substantive response, fall back to most recent missing code
cohort_frame <- cohort_frame %>% 
  rowwise() %>% 
  mutate(sex = if_else(is.na(sex), 
                      coalesce(sex_w9, sex_w8, sex_w7, sex_w6, sex_w5, sex_w4, sex_w3, sex_w2, sex_w1, -3), 
                      sex)) %>% 
  ungroup()

# Factor labels
sex_labels <- c("1" = "Male", "2" = "Female", "-9" = "Refusal", "-8" = "Don't know / insufficient information", "-7" = "Prefer not to say", "-3" = "Not asked at the fieldwork stage / not interviewed", "-2" = "Schedule not applicable / script error / information lost", "-1" = "Item not applicable")

cohort_frame$sex <- factor(cohort_frame$sex, levels = as.numeric(names(sex_labels)), labels = sex_labels)

final_df <- cohort_frame %>% select(NSID, sex)

readr::write_csv(final_df, 'data/output/cleaned_data.csv')

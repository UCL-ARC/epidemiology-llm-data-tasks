library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_three_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_self_completion.tab',
  'ns9_2022_main_interview.tab'
)

data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t', col_types = readr::cols()))
names(data_list) <- files

# Merge datasets
cohort_df <- data_list[[1]]
for (i in 2:length(data_list)) {
  cohort_df <- full_join(cohort_df, data_list[[i]], by = 'NSID')
}

# 2. Define Standard Missing Codes
# -9 Refusal, -8 Don't know, -7 Prefer not to say, -3 Not asked, -2 Schedule not applicable, -1 Item not applicable

# 3. Process Alcohol Consumption for each wave to find earliest age
# Ages: W1=14, W2=15, W3=16, W4=17, W6=19, W7=20, W8=21 (implied from ns8), W9=32

# Helper function to identify if they've consumed alcohol
# Returns TRUE if Yes, FALSE if No, NA if missing
check_alc <- function(val, labels) {
  if (is.na(val)) return(NA)
  # Check if the label for this value is 'Yes'
  # Since we don't have a dynamic label lookup easily, we use the metadata provided in the prompt
  return(val)
}

# We will create a temporary column for each wave indicating if alcohol was consumed
# Wave 1 (14)
cohort_df <- cohort_df %>% mutate(
  alc_14 = case_when(
    W1alceverYP == 1 ~ 1,
    W1alceverYP == 2 ~ 0,
    W1alceverYP == -92 ~ -9,
    W1alceverYP == -91 ~ -1,
    W1alceverYP == -1 ~ -8,
    W1alceverYP == -99 ~ -3,
    W1alceverYP == -97 ~ -7,
    W1alceverYP == -96 ~ -2,
    TRUE ~ -3
  )
)

# Wave 2 (15)
cohort_df <- cohort_df %>% mutate(
  alc_15 = case_when(
    W2alceverYP == 1 ~ 1,
    W2alceverYP == 2 ~ 0,
    W2alceverYP == -92 ~ -9,
    W2alceverYP == -91 ~ -1,
    W2alceverYP == -1 ~ -8,
    W2alceverYP == -99 ~ -3,
    W2alceverYP == -97 ~ -7,
    W2alceverYP == -96 ~ -2,
    W2alceverYP == -998 ~ -2,
    W2alceverYP == -997 ~ -2,
    W2alceverYP == -995 ~ -2,
    TRUE ~ -3
  )
)

# Wave 3 (16)
cohort_df <- cohort_df %>% mutate(
  alc_16 = case_when(
    W3alceverYP == 1 ~ 1,
    W3alceverYP == 2 ~ 0,
    W3alceverYP == -92 ~ -9,
    W3alceverYP == -91 ~ -1,
    W3alceverYP == -1 ~ -8,
    W3alceverYP == -99 ~ -3,
    W3alceverYP == -97 ~ -7,
    W3alceverYP == -96 ~ -2,
    TRUE ~ -3
  )
)

# Wave 4 (17)
cohort_df <- cohort_df %>% mutate(
  alc_17 = case_when(
    W4AlcEverYP == 1 ~ 1,
    W4AlcEverYP == 2 ~ 0,
    W4AlcEverYP == -92 ~ -9,
    W4AlcEverYP == -91 ~ -1,
    W4AlcEverYP == -1 ~ -8,
    W4AlcEverYP == -99 ~ -3,
    W4AlcEverYP == -97 ~ -7,
    W4AlcEverYP == -96 ~ -2,
    TRUE ~ -3
  )
)

# Wave 6 (19)
cohort_df <- cohort_df %>% mutate(
  alc_19 = case_when(
    W6AlcEverYP == 1 ~ 1,
    W6AlcEverYP == 2 ~ 0,
    W6AlcEverYP == -92 ~ -9,
    W6AlcEverYP == -91 ~ -1,
    W6AlcEverYP == -1 ~ -8,
    W6AlcEverYP == -97 ~ -7,
    W6AlcEverYP == -997 ~ -2,
    TRUE ~ -3
  )
)

# Wave 7 (20)
cohort_df <- cohort_df %>% mutate(
  alc_20 = case_when(
    W7AlcEverYP == 1 ~ 1,
    W7AlcEverYP == 2 ~ 0,
    W7AlcEverYP == -92 ~ -9,
    W7AlcEverYP == -91 ~ -1,
    W7AlcEverYP == -1 ~ -8,
    W7AlcEverYP == -97 ~ -7,
    W7AlcEverYP == -996 ~ -2,
    TRUE ~ -3
  )
)

# Wave 8 (21 - inferred)
# W8AUDIT1: 1=Never, 2-5=Consumed
cohort_df <- cohort_df %>% mutate(
  alc_21 = case_when(
    W8AUDIT1 >= 2 & W8AUDIT1 <= 5 ~ 1,
    W8AUDIT1 == 1 ~ 0,
    W8AUDIT1 == -9 ~ -9,
    W8AUDIT1 == -8 ~ -8,
    W8AUDIT1 == -3 ~ -3,
    W8AUDIT1 == -1 ~ -1,
    TRUE ~ -3
  )
)

# Wave 9 (32)
cohort_df <- cohort_df %>% mutate(
  alc_32 = case_when(
    W9AUDIT1 >= 2 & W9AUDIT1 <= 5 ~ 1,
    W9AUDIT1 == 1 ~ 0,
    W9AUDIT1 == -9 ~ -9,
    W9AUDIT1 == -8 ~ -8,
    W9AUDIT1 == -3 ~ -3,
    W9AUDIT1 == -1 ~ -1,
    TRUE ~ -3
  )
)

# Derive alcfst: Earliest age with alcohol consumption (value 1)
cohort_df <- cohort_df %>% 
  mutate(
    alcfst = case_when(
      alc_14 == 1 ~ 14,
      alc_15 == 1 ~ 15,
      alc_16 == 1 ~ 16,
      alc_17 == 1 ~ 17,
      alc_19 == 1 ~ 19,
      alc_20 == 1 ~ 20,
      alc_21 == 1 ~ 21,
      alc_32 == 1 ~ 32,
      # If they never consumed (all 0s) or only missing
      # Check if any wave is 0 (No)
      (alc_14 == 0 | alc_15 == 0 | alc_16 == 0 | alc_17 == 0 | alc_19 == 0 | alc_20 == 0 | alc_21 == 0 | alc_32 == 0) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

# For the final answer, we need to handle the 'Never' case. 
# The prompt asks for the earliest age known to have consumed alcohol.
# If they never did, alcfst should be NA or a specific code. 
# Since it's a continuous-like variable (age), NA is appropriate for 'never' or 'unknown'.

# Final selection
final_df <- cohort_df %>% select(NSID, alcfst)

write_csv(final_df, 'data/output/cleaned_data.csv')

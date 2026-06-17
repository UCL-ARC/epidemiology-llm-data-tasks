library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# File names and ages mapping
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

# Load and merge data
cohort_data <- files %>%
  map(~ read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character()))) %>%
  reduce(full_join, by = 'NSID')

# Helper to standardize missing values based on labels provided in metadata
# -9 Refusal, -8 Don't know, -7 Prefer not to say, -3 Not asked, -2 Schedule not applicable, -1 Item not applicable

# Process alcohol consumption per wave
# Age mapping: W1: 14, W2: 15, W3: 16, W4: 17, W6: 19, W7: 20, W8: 25, W9: 32

# Logic for alcfst:
# 1. Identify if the person ever drank alcohol. 
# 2. Find the first wave they said 'Yes' or indicated a frequency > 'Never'.
# 3. If they consistently said 'No' or 'Never', alcfst = 99.

# Create a temporary dataframe for age analysis
calc_alcohol <- cohort_data %>%
  mutate(
    # Wave 1 (Age 14)
    a14 = case_when(
      W1alceverYP == 1 ~ 14, 
      W1alceverYP == 2 ~ NA, 
      TRUE ~ NA
    ),
    # Wave 2 (Age 15)
    a15 = case_when(
      W2alceverYP == 1 ~ 15, 
      W2alceverYP == 2 ~ NA, 
      TRUE ~ NA
    ),
    # Wave 3 (Age 16)
    a16 = case_when(
      W3alceverYP == 1 ~ 16, 
      W3alceverYP == 2 ~ NA, 
      TRUE ~ NA
    ),
    # Wave 4 (Age 17)
    a17 = case_when(
      W4AlcEverYP == 1 ~ 17, 
      W4AlcEverYP == 2 ~ NA, 
      TRUE ~ NA
    ),
    # Wave 6 (Age 19)
    a19 = case_when(
      W6AlcEverYP == 1 ~ 19, 
      W6AlcEverYP == 2 ~ NA, 
      TRUE ~ NA
    ),
    # Wave 7 (Age 20)
    a20 = case_when(
      W7AlcEverYP == 1 ~ 20, 
      W7AlcEverYP == 2 ~ NA, 
      TRUE ~ NA
    ),
    # Wave 8 (Age 25): AUDIT1 (1=Never, 2-5=Drinking)
    a25 = case_when(
      W8AUDIT1 >= 2 & W8AUDIT1 <= 5 ~ 25, 
      W8AUDIT1 == 1 ~ NA, 
      TRUE ~ NA
    ),
    # Wave 9 (Age 32): AUDIT1 (1=Never, 2-5=Drinking)
    a32 = case_when(
      W9AUDIT1 >= 2 & W9AUDIT1 <= 5 ~ 32, 
      W9AUDIT1 == 1 ~ NA, 
      TRUE ~ NA
    )
  )

# Determine earliest age
# We need to know if they EVER drank alcohol to distinguish between "never" (99) and "missing/not asked"

# Identify 'Never' drinkers: Someone who said 'No' in any wave and never said 'Yes' in any wave
# However, the requirement says alcfst = 99 specifically for those who have never drunk alcohol.

# First, find the minimum age where they said 'Yes'
calc_alcohol <- calc_alcohol %>%
  rowwise() %>%
  mutate(
    first_age = min(c_across(a14:a32), na.rm = TRUE),
    # If all are NA, it's NaN, so handle it
    first_age = if(is.infinite(first_age)) NA else first_age
  )

# Now determine if they are a "Never" drinker
# A person is a "Never" drinker if they have at least one 'No' response and zero 'Yes' responses
# OR if they only have 'No' responses.

cohort_data <- cohort_data %>%
  mutate(
    # Check for any 'Yes' across waves
    ever_yes = (W1alceverYP == 1 | W2alceverYP == 1 | W3alceverYP == 1 | W4AlcEverYP == 1 | 
                W6AlcEverYP == 1 | W7AlcEverYP == 1 | (W8AUDIT1 >= 2 & W8AUDIT1 <= 5) | 
                (W9AUDIT1 >= 2 & W9AUDIT1 <= 5)),
    # Check for any 'No' across waves
    ever_no = (W1alceverYP == 2 | W2alceverYP == 2 | W3alceverYP == 2 | W4AlcEverYP == 2 | 
               W6AlcEverYP == 2 | W7AlcEverYP == 2 | W8AUDIT1 == 1 | W9AUDIT1 == 1)
  )

# Combine to get alcfst
final_df <- cohort_data %>%
  bind_cols(calc_alcohol %>% select(first_age)) %>%
  mutate(
    alcfst = case_when(
      ever_yes == TRUE ~ first_age,
      ever_no == TRUE & ever_yes == FALSE ~ 99,
      TRUE ~ -3 # Not asked / Missing
    )
  ) %>%
  select(NSID, alcfst)

# Write to CSV
write_csv(final_df, 'data/output/cleaned_data.csv')

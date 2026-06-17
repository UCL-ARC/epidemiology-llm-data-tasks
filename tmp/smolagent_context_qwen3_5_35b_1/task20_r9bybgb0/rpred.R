library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Merge all files by NSID
df <- full_join(wave1, wave2, by = 'NSID')
df <- full_join(df, wave3, by = 'NSID')
df <- full_join(df, wave4, by = 'NSID')
df <- full_join(df, wave6, by = 'NSID')
df <- full_join(df, wave7, by = 'NSID')
df <- full_join(df, wave8, by = 'NSID')
df <- full_join(df, wave9, by = 'NSID')

# Map missing values to standard codes based on metadata labels
# Helper function to map source missing codes to standard codes
map_missing <- function(x) {
  # Standard codes:
  # -9 = Refusal
  # -8 = Don't know / insufficient information
  # -7 = Prefer not to say
  # -3 = Not asked at fieldwork stage / not interviewed
  # -2 = Schedule not applicable / script error / information lost
  # -1 = Item not applicable
  
  x <- case_when(
    # Refusal
    x == -92 ~ -9,
    # Don't know / insufficient information
    x == -1 | x == -1.0 ~ -8,
    # Not interviewed / not asked at fieldwork stage
    x == -99 | x == -99.0 ~ -3,
    # Script error / schedule not applicable / information lost
    x == -97 | x == -97.0 | x == -96 | x == -96.0 | x == -995 | x == -997 | x == -998 | x == -996 ~ -2,
    # Not applicable
    x == -91 | x == -91.0 | x == -1 ~ -1,
    TRUE ~ x
  )
  return(x)
}

# Create drinking indicator variables for each wave (1 = drinking, 0 = not drinking, NA = missing)
# Age mapping: S1->14, S2->15, S3->16, S4->17, S6->19, S7->20, S8->25, S9->32

# Sweep 1 (Age 14): requires BOTH W1alceverYP = 1 AND W1alcmonYP = 1
df <- df %>%
  mutate(
    W1alcmonYP = map_missing(W1alcmonYP),
    W1alcmonYP = if_else(is.na(W1alcmonYP), NA, W1alcmonYP),
    W1alceverYP = map_missing(W1alceverYP),
    W1alceverYP = if_else(is.na(W1alceverYP), NA, W1alceverYP),
    
    # Special rule: age 14 requires BOTH W1alceverYP = 1 AND W1alcmonYP = 1
    drinking_14 = case_when(
      is.na(W1alcmonYP) | is.na(W1alceverYP) ~ NA,
      W1alcmonYP == 1 & W1alceverYP == 1 ~ 14,
      TRUE ~ 0
    )
  )

# Sweep 2 (Age 15): W2alceverYP = 1
df <- df %>%
  mutate(
    W2alceverYP = map_missing(W2alceverYP),
    drinking_15 = case_when(
      is.na(W2alceverYP) ~ NA,
      W2alceverYP == 1 ~ 15,
      TRUE ~ 0
    )
  )

# Sweep 3 (Age 16): W3alceverYP = 1
df <- df %>%
  mutate(
    W3alceverYP = map_missing(W3alceverYP),
    drinking_16 = case_when(
      is.na(W3alceverYP) ~ NA,
      W3alceverYP == 1 ~ 16,
      TRUE ~ 0
    )
  )

# Sweep 4 (Age 17): W4AlcEverYP = 1
df <- df %>%
  mutate(
    W4AlcEverYP = map_missing(W4AlcEverYP),
    drinking_17 = case_when(
      is.na(W4AlcEverYP) ~ NA,
      W4AlcEverYP == 1 ~ 17,
      TRUE ~ 0
    )
  )

# Sweep 6 (Age 19): W6AlcEverYP = 1
df <- df %>%
  mutate(
    W6AlcEverYP = map_missing(W6AlcEverYP),
    drinking_19 = case_when(
      is.na(W6AlcEverYP) ~ NA,
      W6AlcEverYP == 1 ~ 19,
      TRUE ~ 0
    )
  )

# Sweep 7 (Age 20): W7AlcEverYP = 1
df <- df %>%
  mutate(
    W7AlcEverYP = map_missing(W7AlcEverYP),
    drinking_20 = case_when(
      is.na(W7AlcEverYP) ~ NA,
      W7AlcEverYP == 1 ~ 20,
      TRUE ~ 0
    )
  )

# Sweep 8 (Age 25): W8AUDIT1 > 1 (code 1 = Never, so codes 2-5 indicate drinking)
df <- df %>%
  mutate(
    W8AUDIT1 = map_missing(W8AUDIT1),
    drinking_25 = case_when(
      is.na(W8AUDIT1) ~ NA,
      W8AUDIT1 > 1 ~ 25,
      TRUE ~ 0
    )
  )

# Sweep 9 (Age 32): W9AUDIT1 > 1 (code 1 = Never, so codes 2-5 indicate drinking)
df <- df %>%
  mutate(
    W9AUDIT1 = map_missing(W9AUDIT1),
    drinking_32 = case_when(
      is.na(W9AUDIT1) ~ NA,
      W9AUDIT1 > 1 ~ 32,
      TRUE ~ 0
    )
  )

# Calculate alcfst: earliest age at which drinking is recorded
df <- df %>%
  mutate(
    # Collect all drinking indicators
    drinking_ages = pmap(
      list(drinking_14, drinking_15, drinking_16, drinking_17, drinking_19, drinking_20, drinking_25, drinking_32),
      function(d14, d15, d16, d17, d19, d20, d25, d32) {
        ages = c(d14, d15, d16, d17, d19, d20, d25, d32)
        names(ages) = c('14', '15', '16', '17', '19', '20', '25', '32')
        return(ages)
      }
    )
  )

# Determine alcfst for each person
df <- df %>%
  mutate(
    alcfst = mapply(
      function(ages) {
        # Check if any drinking observed (non-zero, non-NA values)
        has_drinking = any(ages > 0, na.rm = TRUE)
        has_missing = any(is.na(ages), na.rm = TRUE)
        has_not_drinking = any(ages == 0, na.rm = TRUE)
        
        if (has_drinking) {
          # Find earliest age with drinking
          drinking_ages = ages[ages > 0]
          return(min(drinking_ages, na.rm = TRUE))
        } else if (has_missing) {
          # No drinking observed but at least one missing
          return(-8)
        } else {
          # All observed show not-drinking, no missing
          return(99)
        }
      },
      df$drinking_ages
    )
  )

# Create factor with proper labels
alcfst_labels <- c(
  '14' = 'Age 14',
  '15' = 'Age 15',
  '16' = 'Age 16',
  '17' = 'Age 17',
  '19' = 'Age 19',
  '20' = 'Age 20',
  '25' = 'Age 25',
  '32' = 'Age 32',
  '99' = 'Never had alcohol',
  '-8' = 'Don\'t know/insufficient information'
)

df$alcfst <- factor(
  df$alcfst,
  levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
  labels = alcfst_labels
)

# Keep only NSID and alcfst
df_clean <- df %>%
  select(NSID, alcfst)

# Write output
write_csv(df_clean, 'data/output/cleaned_data.csv')

cat('Output written to data/output/cleaned_data.csv\n')
cat('Total rows:', nrow(df_clean), '\n')
cat('Summary of alcfst:\n')
print(table(df_clean$alcfst, useNA = 'ifany'))

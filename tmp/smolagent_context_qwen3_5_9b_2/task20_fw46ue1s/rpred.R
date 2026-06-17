library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
w2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
w3 <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t')
w4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
w6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
w7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Create data frames with age and drinking indicator for each wave
# Wave 1 (Age 14): S1
data_w1 <- w1 %>%
  mutate(
    age = 14,
    drink = ifelse(W1alceverYP == 1 & W1alcmonYP == 1, 1, 0)  # Special rule for sweep 1
  )

# Wave 2 (Age 15): S2
data_w2 <- w2 %>%
  mutate(
    age = 15,
    drink = as.integer(W2alceverYP)  # Code 1 = Yes
  )

# Wave 3 (Age 16): S3
data_w3 <- w3 %>%
  mutate(
    age = 16,
    drink = as.integer(W3alceverYP)  # Code 1 = Yes
  )

# Wave 4 (Age 17): S4
data_w4 <- w4 %>%
  mutate(
    age = 17,
    drink = as.integer(W4AlcEverYP)  # Code 1 = Yes
  )

# Wave 6 (Age 19): S6
data_w6 <- w6 %>%
  mutate(
    age = 19,
    drink = as.integer(W6AlcEverYP)  # Code 1 = Yes
  )

# Wave 7 (Age 20): S7
data_w7 <- w7 %>%
  mutate(
    age = 20,
    drink = as.integer(W7AlcEverYP)  # Code 1 = Yes
  )

# Wave 8 (Age 25): S8 - AUDIT frequency, code > 1 means drinking (1 = Never)
data_w8 <- ns8 %>%
  mutate(
    age = 25,
    drink = as.integer(W8AUDIT1)  # Code 1 = Never, 2-5 = drinking (above Never)
  )

# Wave 9 (Age 32): S9 - AUDIT frequency, code > 1 means drinking (1 = Never)
data_w9 <- ns9 %>%
  mutate(
    age = 32,
    drink = as.integer(W9AUDIT1)  # Code 1 = Never, 2-5 = drinking (above Never)
  )

# Combine all waves
data_all <- bind_rows(data_w1, data_w2, data_w3, data_w4, data_w6, data_w7, data_w8, data_w9)

# Function to determine if a value represents drinking
is_drinking <- function(x) {
  if (is.na(x) || x == 0) return(NA)  # Not drinking or missing
  return(!!x > 0)
}

# Process each person to find earliest drinking age
# For each NSID, group and find earliest drinking
alcfst_raw <- data_all %>%
  group_by(NSID) %>%
  summarise(
    # For each wave, check if drinking was observed
    result = list(),
    .groups = 'drop'
  ) %>%
  ungroup()

# Better approach: for each person, collect all drinking info
person_data <- data_all %>%
  group_by(NSID) %>%
  summarise(
    all_waves = list(data.frame(age, drink)),
    .groups = 'drop'
  ) %>%
  mutate(
    alcfst = sapply(all_waves, function(wave_data) {
      # Filter out missing/non-applicable values
      valid_drinking <- wave_data %>%
        filter(!is.na(drink) & drink > 0)
      
      if (nrow(valid_drinking) > 0) {
        # Drinking was observed - return earliest age
        min(valid_drinking$age)
      } else {
        # No drinking observed, check if any values are missing
        any_missing <- any(is.na(wave_data$drink) | wave_data$drink == 0)
        if (any_missing) {
          return(-8)  # Don't know / insufficient information
        } else {
          return(99)  # Never had alcohol
        }
      }
    }))

# Convert to factor
alcfst_factor <- factor(alcfst_raw$alcfst, 
                        levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
                        labels = c('Age 14', 'Age 15', 'Age 16', 'Age 17', 'Age 19', 
                                  'Age 20', 'Age 25', 'Age 32', 'Never had alcohol', 
                                  'Don\'t know/insufficient information'))

# Join back to original data
cleaned <- full_join(data_w1, alcfst_raw[, c('NSID', 'alcfst')], by = 'NSID') %>%
  mutate(alcfst_factor = alcfst_raw$alcfst_factor)

# Write output
write_csv(cleaned[, c('NSID', 'alcfst_factor')], 'data/output/cleaned_data.csv')

cat('Script completed successfully\n')

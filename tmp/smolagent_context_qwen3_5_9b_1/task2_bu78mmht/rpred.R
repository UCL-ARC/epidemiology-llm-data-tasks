library(haven)
library(dplyr)
library(readr)
library(labelled)

# Load all datasets
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
w2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
w4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
w8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
w9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Combine all datasets using full_join by NSID
cleaned <- full_join(w1, w2, by = 'NSID') %>%
  full_join(w4, by = 'NSID') %>%
  full_join(w8, by = 'NSID') %>%
  full_join(w9, by = 'NSID')

# Define missing value mapping function
map_missing <- function(x, wave) {
  if (wave == 1) {
    # Wave 1: -999, -94, -92, -91, -1
    x <- recode_factor(x, 
      c('-999' = -3, '-94' = -3, '-92' = -3, '-91' = -3, '-1' = -3),
      multiple = 'value',
      from_labels = FALSE)
  } else if (wave == 2) {
    # Wave 2: -998, -997, -995, -99, -92, -91, -1
    x <- recode_factor(x, 
      c('-998' = -3, '-997' = -3, '-995' = -3, '-99' = -3, '-92' = -3, '-91' = -3, '-1' = -3),
      multiple = 'value',
      from_labels = FALSE)
  } else if (wave == 4) {
    # Wave 4: -94, -1
    x <- recode_factor(x, 
      c('-94' = -3, '-1' = -3),
      multiple = 'value',
      from_labels = FALSE)
  } else if (wave == 8) {
    # Wave 8: -9, -8, -1
    x <- recode_factor(x, 
      c('-9' = -3, '-8' = -3, '-1' = -3),
      multiple = 'value',
      from_labels = FALSE)
  } else if (wave == 9) {
    # Wave 9: -8
    x <- recode_factor(x, 
      c('-8' = -3),
      multiple = 'value',
      from_labels = FALSE)
  }
  return(x)
}

# Create harmonised versions of the ethnicity variables
# Define value labels for harmonised variable
eth_labels <- c(
  '-1' = "Don't know",
  '-3' = 'Not asked/not interviewed',
  '1' = 'White - British',
  '2' = 'White - Irish',
  '3' = 'Any other White background',
  '4' = 'Mixed - White and Black Caribbean',
  '5' = 'Mixed - White and Black African',
  '6' = 'Mixed - White and Asian',
  '7' = 'Any other mixed background',
  '8' = 'Indian',
  '9' = 'Pakistani',
  '10' = 'Bangladeshi',
  '11' = 'Any other Asian background',
  '12' = 'Black Caribbean',
  '13' = 'Black African',
  '14' = 'Any other Black background',
  '15' = 'Chinese',
  '16' = 'Any other ethnic background'
)

# Clean W1ethnic2YP (age 14)
w1_clean <- cleaned %>%
  mutate(W1ethnic2YP = ifelse(is.na(W1ethnic2YP), -3, W1ethnic2YP)) %>%
  mutate(W1ethnic2YP = ifelse(W1ethnic2YP < 1, -3, W1ethnic2YP)) %>%
  mutate(W1ethnic2YP = as.factor(W1ethnic2YP))

# Clean W2ethnicYP (age 15)
w2_clean <- cleaned %>%
  mutate(W2ethnicYP = ifelse(is.na(W2ethnicYP), -3, W2ethnicYP)) %>%
  mutate(W2ethnicYP = ifelse(W2ethnicYP < 1, -3, W2ethnicYP)) %>%
  mutate(W2ethnicYP = as.factor(W2ethnicYP))

# Clean w4ethnic2YP (age 17)
w4_clean <- cleaned %>%
  mutate(w4ethnic2YP = ifelse(is.na(w4ethnic2YP), -3, w4ethnic2YP)) %>%
  mutate(w4ethnic2YP = ifelse(w4ethnic2YP < 1, -3, w4ethnic2YP)) %>%
  mutate(w4ethnic2YP = as.factor(w4ethnic2YP))

# Clean W8DETHN15 (age 25)
w8_clean <- cleaned %>%
  mutate(W8DETHN15 = ifelse(is.na(W8DETHN15), -3, W8DETHN15)) %>%
  mutate(W8DETHN15 = ifelse(W8DETHN15 < 1, -3, W8DETHN15)) %>%
  mutate(W8DETHN15 = as.factor(W8DETHN15))

# Clean W9DETHN15 (age 32)
w9_clean <- cleaned %>%
  mutate(W9DETHN15 = ifelse(is.na(W9DETHN15), -3, W9DETHN15)) %>%
  mutate(W9DETHN15 = ifelse(W9DETHN15 < 1, -3, W9DETHN15)) %>%
  mutate(W9DETHN15 = as.factor(W9DETHN15))

# Derive eth: earliest valid positive response
# Priority: W1ethnic2YP -> W2ethnicYP -> w4ethnic2YP -> W8DETHN15 -> W9DETHN15

# Start with NA
cleaned <- cleaned %>%
  mutate(eth = as.numeric(NA))

# Check W1ethnic2YP first (age 14)
cleaned <- cleaned %>%
  mutate(eth = ifelse(W1ethnic2YP > 1 & W1ethnic2YP <= 16, W1ethnic2YP, eth))

# Check W2ethnicYP second (age 15)
cleaned <- cleaned %>%
  mutate(eth = ifelse(W2ethnicYP > 1 & W2ethnicYP <= 16, W2ethnicYP, eth))

# Check w4ethnic2YP third (age 17)
cleaned <- cleaned %>%
  mutate(eth = ifelse(w4ethnic2YP > 1 & w4ethnic2YP <= 16, w4ethnic2YP, eth))

# Check W8DETHN15 fourth (age 25)
cleaned <- cleaned %>%
  mutate(eth = ifelse(W8DETHN15 > 1 & W8DETHN15 <= 16, W8DETHN15, eth))

# Check W9DETHN15 fifth (age 32)
cleaned <- cleaned %>%
  mutate(eth = ifelse(W9DETHN15 > 1 & W9DETHN15 <= 16, W9DETHN15, eth))

# Set remaining NAs and non-positive values to -3 (missing)
cleaned <- cleaned %>%
  mutate(eth = ifelse(is.na(eth) | eth < 1 | eth > 16, -3, eth)) %>%
  mutate(eth = as.factor(eth))

# Create value labels for eth
# Order: -3, -1, then 1-16
eth_label_order <- c('-3', '-1', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16')
cleaned <- cleaned %>%
  mutate(eth = factor(eth, levels = as.character(eth_label_order), labels = c(
    '-3' = 'Not asked/not interviewed',
    '-1' = "Don't know",
    '1' = 'White - British',
    '2' = 'White - Irish',
    '3' = 'Any other White background',
    '4' = 'Mixed - White and Black Caribbean',
    '5' = 'Mixed - White and Black African',
    '6' = 'Mixed - White and Asian',
    '7' = 'Any other mixed background',
    '8' = 'Indian',
    '9' = 'Pakistani',
    '10' = 'Bangladeshi',
    '11' = 'Any other Asian background',
    '12' = 'Black Caribbean',
    '13' = 'Black African',
    '14' = 'Any other Black background',
    '15' = 'Chinese',
    '16' = 'Any other ethnic background'
  )))

# Remove raw source variables, keep only NSID and eth
cleaned_final <- cleaned %>%
  select(NSID, eth)

# Write output
write_csv(cleaned_final, 'data/output/cleaned_data.csv')

cat('Script completed successfully. Output written to data/output/cleaned_data.csv\n')
print(head(cleaned_final))
print(str(cleaned_final))
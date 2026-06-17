library(haven)
library(dplyr)
library(readr)
library(tidyr)

# Load all files from data/input/
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
w4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge all datasets by NSID using full_join
data <- full_join(w1, w4, by = 'NSID')
data <- full_join(data, ns8, by = 'NSID')
data <- full_join(data, ns9, by = 'NSID')

# Extract income variables and rename
# W8DINCB (Wave 8, Age 25) -> inc25
# W9DINCB (Wave 9, Age 32) -> inc32

# Handle missing values for W8DINCB/inc25
# user_missing_values: '-1.0 thru None'
# -1.0 = 'Not applicable' -> -1 (item not applicable)
# NA values -> -3 (not asked)
data$inc25 <- data$W8DINCB

# Convert missing values -1.0 to -1
data$inc25[data$inc25 == -1.0] <- -1

# Convert NA to -3 (not asked)
data$inc25[is.na(data$inc25)] <- -3

# Handle missing values for W9DINCB/inc32
# Same pattern: -1.0 = 'Not applicable' -> -1
# NA -> -3 (not asked)
data$inc32 <- data$W9DINCB

# Convert missing values -1.0 to -1
data$inc32[data$inc32 == -1.0] <- -1

# Convert NA to -3 (not asked)
data$inc32[is.na(data$inc32)] <- -3

# Keep only ID and final derived variables
# Remove raw source variables
data_final <- data %>%
  select(NSID, inc25, inc32)

# Create labels for the variables based on metadata value labels
# For inc25 and inc32, the value labels are:
# -1: Not applicable -> -1
# -3: Not asked
# 1.0: less than 25
# 2.0: 25 to 50
# 3.0: 50 to 90
# 4.0: 90 to 140
# 5.0: 140 to 240
# 6.0: 240 to 300
# 7.0: 300 to 350
# 8.0: 350 to 400
# 9.0: 400 to 500
# 10.0: 500 to 600
# 11.0: 600 to 700
# 12.0: 700 to 800
# 13.0: 800 to 900
# 14.0: 900 to 1200
# 15.0: 1200 to 1400
# 16.0: more than 1400

# Create labels for inc25
lab_inc25 <- c(
  '-1' = 'Not applicable',
  '-3' = 'Not asked',
  '1' = 'less than 25',
  '2' = '25 to 50',
  '3' = '50 to 90',
  '4' = '90 to 140',
  '5' = '140 to 240',
  '6' = '240 to 300',
  '7' = '300 to 350',
  '8' = '350 to 400',
  '9' = '400 to 500',
  '10' = '500 to 600',
  '11' = '600 to 700',
  '12' = '700 to 800',
  '13' = '800 to 900',
  '14' = '900 to 1200',
  '15' = '1200 to 1400',
  '16' = 'more than 1400'
)

# For the band levels (1-16), convert from numeric to factor with proper labels
# But keep them as numeric for comparison purposes
data_final$inc25 <- as.numeric(data_final$inc25)

# Create labels for inc32
lab_inc32 <- c(
  '-1' = 'Not applicable',
  '-3' = 'Not asked',
  '1' = 'less than 25',
  '2' = '25 to 50',
  '3' = '50 to 90',
  '4' = '90 to 140',
  '5' = '140 to 240',
  '6' = '240 to 300',
  '7' = '300 to 350',
  '8' = '350 to 400',
  '9' = '400 to 500',
  '10' = '500 to 600',
  '11' = '600 to 700',
  '12' = '700 to 800',
  '13' = '800 to 900',
  '14' = '900 to 1200',
  '15' = '1200 to 1400',
  '16' = 'more than 1400'
)

# Attach labels
data_final <- data_final %>%
  mutate(
    inc25 = factor(inc25, levels = c(-1, -3, 1:16), labels = c('NA', 'NA', as.character(1:16))),
    inc32 = factor(inc32, levels = c(-1, -3, 1:16), labels = c('NA', 'NA', as.character(1:16)))
  )

# Write output
write_csv(data_final, 'data/output/cleaned_data.csv')

print('Script completed successfully')
print(head(data_final))
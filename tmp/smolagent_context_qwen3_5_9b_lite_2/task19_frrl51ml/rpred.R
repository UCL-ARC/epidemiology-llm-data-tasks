library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load data files from data/input/
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge all datasets by NSID
all_data <- full_join(wave1, wave4, by = 'NSID')
all_data <- full_join(all_data, ns8, by = 'NSID')
all_data <- full_join(all_data, ns9, by = 'NSID')

# Convert NSID to character to ensure proper joining
all_data$NSID <- as.character(all_data$NSID)

# Create bmi25 from W8DBMI (Age 25 data, from wave8)
bmi25 <- all_data$W8DBMI
# Convert NA and missing codes to standard -9
bmi25[is.na(bmi25) | bmi25 %in% c(-9, -8, -1)] <- -9
all_data$bmi25 <- bmi25

# Create bmi32 from W9DBMI (Age 32 data, from wave9)
bmi32 <- all_data$W9DBMI
# Convert NA and missing codes to standard -9
bmi32[is.na(bmi32) | bmi32 %in% c(-9, -8, -1)] <- -9
all_data$bmi32 <- bmi32

# Remove raw source variables - keep only NSID and derived variables
all_data <- all_data %>% select(NSID, bmi25, bmi32)

# Write to CSV
write_csv(all_data, 'data/output/cleaned_data.csv')

print('Script completed successfully')
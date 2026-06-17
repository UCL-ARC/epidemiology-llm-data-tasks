library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave_one <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_two <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave_three <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave_four <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')
ns9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Rename conflicting columns in wave_two and wave_three
wave_two <- wave_two %>%
  rename(
    wave2_urbind = urbind,
    wave2_gor = gor
  )

wave_three <- wave_three %>%
  rename(
    wave3_urbind = urbind,
    wave3_gor = gor
  )

# Merge all datasets by NSID
full_data <- wave_one %>%
  full_join(wave_two, by = 'NSID') %>%
  full_join(wave_three, by = 'NSID') %>%
  full_join(wave_four, by = 'NSID') %>%
  full_join(ns8, by = 'NSID') %>%
  full_join(ns9_derived, by = 'NSID') %>%
  full_join(ns9_main, by = 'NSID')

cat('Full data has', nrow(full_data), 'rows\n')
cat('Columns containing urbind or gor:', paste(grep('urbind|gor', names(full_data), value = TRUE), collapse = ', '), '\n')

# Create derived variables
regub15 <- full_data$wave2_urbind
regub16 <- full_data$wave3_urbind

# Process urbind at age 15 (regub15)
regub15[is.na(regub15)] <- -3
regub15[regub15 == -94] <- -8
regub15[regub15 >= -999 & regub15 <= -2] <- -3
regub15[regub15 == -1] <- -1

# Process urbind at age 16 (regub16)
regub16[is.na(regub16)] <- -3
regub16[regub16 == -94] <- -8
regub16[regub16 >= -999 & regub16 <= -2] <- -3
regub16[regub16 == -1] <- -1

# Create gor at age 15 (regov15)
regov15 <- full_data$wave2_gor
regov15[is.na(regov15)] <- -3
regov15[regov15 == -94] <- -8
regov15[regov15 >= -999 & regov15 <= -2] <- -3

# Create gor at age 16 (regov16)
regov16 <- full_data$wave3_gor
regov16[is.na(regov16)] <- -3
regov16[regov16 == -94] <- -8
regov16[regov16 >= -999 & regov16 <= -2] <- -3

# Create regor25 from W8DGOR
regor25 <- full_data$W8DGOR
regor25[is.na(regor25)] <- -3
regor25[regor25 == -9] <- -9
regor25[regor25 == -1] <- -1

# Create regor32 from W9DRGN
regor32 <- full_data$W9DRGN
regor32[is.na(regor32)] <- -3
regor32[regor32 == -9] <- -9
regor32[regor32 == -1] <- -1

# Create regint32 from W9NATIONRES
regint32 <- full_data$W9NATIONRES
regint32[is.na(regint32)] <- -3
regint32[regint32 == -9] <- -9
regint32[regint32 == -8] <- -8
regint32[regint32 == -3] <- -3
regint32[regint32 == -1] <- -1

# Add derived variables to output
full_data$regub15 <- regub15
full_data$regub16 <- regub16
full_data$regov15 <- regov15
full_data$regov16 <- regov16
full_data$regor25 <- regor25
full_data$regor32 <- regor32
full_data$regint32 <- regint32

cat('Script completed successfully!\n')

# Write output
write_csv(full_data, 'data/output/cleaned_data.csv')

# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory
if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Function to recode educational aims to 6-category NVQ scheme
recode_educational_aims <- function(x) {
  result <- rep(-3L, length(x))
  
  # Wave 4 (age 17)
  result[x == 1] <- 3L
  result[x == 5] <- 2L
  result[x == 9] <- 1L
  result[x == 13] <- 5L
  result[x == 14] <- 6L
  result[x %in% c(2, 6, 8, 10)] <- 2L
  result[x %in% c(3, 4, 7, 11, 12)] <- 1L
  
  # Wave 6 (age 19)
  result[x == 1] <- 5L
  result[x == 2] <- 5L
  result[x == 3] <- 4L
  result[x == 4] <- 4L
  result[x == 5] <- 3L
  result[x == 9] <- 2L
  result[x == 12] <- 1L
  result[x == 15] <- 5L
  result[x == 16] <- 6L
  result[x %in% c(6, 7, 8, 10, 13, 11)] <- 3L
  
  # Wave 7 (age 20)
  result[x == 13] <- 5L
  result[x == 11] <- 5L
  result[x == 12] <- 4L
  result[x == 10] <- 4L
  result[x == 6] <- 3L
  result[x == 9] <- 3L
  result[x == 3] <- 2L
  result[x == 8] <- 2L
  result[x == 7] <- 2L
  result[x == 5] <- 2L
  result[x == 4] <- 2L
  result[x == 2] <- 1L
  result[x == 1] <- 1L
  result[x == 14] <- 5L
  
  result
}

# Load all datasets
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Recode educational aims for waves 4, 6, 7
wave4$educaim17 <- recode_educational_aims(wave4$w4saim)
wave6$educaim19 <- recode_educational_aims(wave6$W6Saim)
wave7$educaim20 <- recode_educational_aims(wave7$W7SAim)

# For wave 8 (age 25) and wave 9 (age 32), derive educational aims from activity
ns8$educaim25 <- ifelse(is.na(ns8$W8ACTIVITY05) | ns8$W8ACTIVITY05 == 0, -3L, 5L)
ns9$educaim32 <- ifelse(is.na(ns9$W9ECONACT2) | ns9$W9ECONACT2 %in% c(-1, -9, -8, -3), -3L, 5L)

# Combine all datasets
combined <- full_join(wave1, wave4, by = 'NSID')
combined <- full_join(combined, wave6, by = 'NSID')
combined <- full_join(combined, wave7, by = 'NSID')
combined <- full_join(combined, ns8, by = 'NSID')
combined <- full_join(combined, ns9, by = 'NSID')

# Write output
write_csv(combined, 'data/output/cleaned_data.csv')

cat('Script completed successfully\n')


# Load required libraries
library(haven)
library(dplyr)
library(readr)

# Load data files
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')

# Merge datasets by NSID
merged <- full_join(wave4, wave6, by = 'NSID')
merged <- full_join(merged, wave7, by = 'NSID')

# Create educaim17 using correct variable name
merged$educaim17 <- ifelse(is.na(merged$w4saim), merged$w4saim,
                          ifelse(merged$w4saim %in% c(1:7), 1,
                                 ifelse(merged$w4saim == 8, 2,
                                        ifelse(merged$w4saim %in% c(9, 10, 11), 2,
                                               ifelse(merged$w4saim %in% c(12, 13), 3,
                                                      ifelse(merged$w4saim == 14, 5, merged$w4saim))))))

# Create educaim19 using correct variable name
merged$educaim19 <- ifelse(is.na(merged$W6Saim), merged$W6Saim,
                          ifelse(merged$W6Saim %in% c(1, 2, 3, 4), 0,
                                 ifelse(merged$W6Saim %in% c(5:10), 1,
                                        ifelse(merged$W6Saim %in% c(11, 12, 13), 2,
                                               ifelse(merged$W6Saim %in% c(14, 15), 3,
                                                      ifelse(merged$W6Saim == 16, 5, merged$W6Saim))))))

# Create educaim20 using correct variable name
merged$educaim20 <- ifelse(is.na(merged$W7SAim), merged$W7SAim,
                          ifelse(merged$W7SAim == -94, -8,
                                 ifelse(merged$W7SAim == -91, 5,
                                        ifelse(merged$W7SAim %in% c(1, 2), 2,
                                               ifelse(merged$W7SAim %in% c(3, 5), 1,
                                                      ifelse(merged$W7SAim %in% c(4, 6, 7, 8, 9), 1,
                                                             ifelse(merged$W7SAim %in% c(10, 11, 12, 13), 0,
                                                                    ifelse(merged$W7SAim == 14, 3, merged$W7SAim))))))))

# Create placeholder variables for educaim25 and educaim32
merged$educaim25 <- -3
merged$educaim32 <- -3

# Handle missing values
merged$educaim17 <- recode(merged$educaim17, '-999' = -3, '-998' = -2, '-997' = -2, '-995' = -2, '-99' = -3, '-94' = -8, '-92' = -9, '-91' = -1)
merged$educaim19 <- recode(merged$educaim19, '-999' = -3, '-998' = -2, '-997' = -2, '-995' = -2, '-99' = -3, '-94' = -8, '-92' = -9, '-91' = -1)
merged$educaim20 <- recode(merged$educaim20, '-999' = -3, '-998' = -2, '-997' = -2, '-995' = -2, '-99' = -3, '-94' = -8, '-92' = -9, '-91' = -1)

# Select and save output
output <- merged[, c('NSID', 'educaim17', 'educaim19', 'educaim20', 'educaim25', 'educaim32')]
write_csv(output, 'data/output/cleaned_data.csv')

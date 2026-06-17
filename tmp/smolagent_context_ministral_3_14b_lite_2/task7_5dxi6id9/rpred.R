
# Load required libraries
library(haven)
library(dplyr)
library(readr)

# Load all relevant files
wave1 <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- readr::read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave6 <- readr::read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- readr::read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- readr::read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
wave9 <- readr::read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Merge datasets by NSID
merged_data <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave6, by = 'NSID') %>%
  full_join(wave7, by = 'NSID') %>%
  full_join(wave8, by = 'NSID') %>%
  full_join(wave9, by = 'NSID')

# Create mapping functions for individual values
map_wave4 <- function(x) {
  if (is.na(x)) return(-3)
  if (x == 1) return(1)
  if (x %in% c(2, 3, 4)) return(2)
  if (x %in% c(5, 6, 7)) return(3)
  if (x == 8) return(4)
  if (x %in% c(9, 10, 11)) return(5)
  if (x %in% c(12, 13, 14)) return(6)
  return(-3)
}

map_wave6 <- function(x) {
  if (is.na(x)) return(-3)
  if (x == 1) return(5)
  if (x == 2) return(6)
  if (x == 3) return(4)
  if (x == 4) return(6)
  if (x == 5) return(1)
  if (x %in% c(6, 7, 8)) return(2)
  if (x %in% c(9, 10)) return(3)
  if (x == 11) return(4)
  if (x %in% c(12, 13)) return(5)
  if (x %in% c(14, 15, 16)) return(6)
  return(-3)
}

map_wave7 <- function(x) {
  if (is.na(x)) return(-3)
  if (x == -94) return(-8)
  if (x == -91) return(-1)
  if (x %in% c(1, 2)) return(5)
  if (x == 3) return(3)
  if (x == 4) return(4)
  if (x == 5) return(3)
  if (x == 6) return(1)
  if (x %in% c(7, 8, 9)) return(2)
  if (x == 10) return(4)
  if (x %in% c(11, 12)) return(6)
  if (x == 13) return(5)
  if (x == 14) return(6)
  return(-3)
}

# Apply mapping functions using sapply to handle vectors properly
merged_data$educaim17 <- sapply(merged_data$w4saim, map_wave4)
merged_data$educaim19 <- sapply(merged_data$W6Saim, map_wave6)
merged_data$educaim20 <- sapply(merged_data$W7SAim, map_wave7)

# Handle wave 8 (Age 25)
merged_data$educaim25 <- ifelse(
  is.na(merged_data$W8ACTIVITY05) | merged_data$W8ACTIVITY05 == 0,
  -1,
  ifelse(
    merged_data$W8ACQUC0A == 1 | merged_data$W8ACQUC0B == 1 | merged_data$W8ACQUC0C == 1,
    6,
    ifelse(
      merged_data$W8ACQUC0F == 1 | merged_data$W8ACQUC0G == 1 | merged_data$W8ACQUC0H == 1 |
      merged_data$W8ACQUC0I == 1 | merged_data$W8ACQUC0J == 1 | merged_data$W8VCQUC0J == 1,
      1,
      ifelse(
        merged_data$W8VCQUC0C == 1 | merged_data$W8VCQUC0D == 1 | merged_data$W8VCQUC0E == 1,
        3,
        4
      )
    )
  )
)

# Handle wave 9 (Age 32)
merged_data$educaim32 <- ifelse(
  is.na(merged_data$W9ECONACT2) | !(merged_data$W9ECONACT2 %in% c(6, 7)),
  -1,
  ifelse(
    merged_data$W9ACQUC0A == 1 | merged_data$W9ACQUC0B == 1 | merged_data$W9ACQUC0C == 1,
    6,
    ifelse(
      merged_data$W9ACQUC0G == 1 | merged_data$W9ACQUC0H == 1 | merged_data$W9VCQUC0D == 1,
      1,
      ifelse(
        merged_data$W9ACQUC0I == 1 | merged_data$W9VCQUC0E == 1,
        3,
        4
      )
    )
  )
)

# Select only the NSID and educaim variables
final_data <- merged_data %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write the output file
readr::write_csv(final_data, 'data/output/cleaned_data.csv')

# Print confirmation
cat('Cleaned data has been written to data/output/cleaned_data.csv\n')

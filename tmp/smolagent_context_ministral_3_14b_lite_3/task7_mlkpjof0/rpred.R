
# Load required libraries
library(readr)
library(dplyr)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Get all unique NSIDs from wave1
all_nsids <- unique(wave1$NSID)

# Create a data frame with all NSIDs and default educaim values
complete_data <- data.frame(
  NSID = all_nsids,
  educaim17 = rep(6, length(all_nsids)),
  educaim19 = rep(6, length(all_nsids)),
  educaim20 = rep(6, length(all_nsids)),
  educaim25 = rep(6, length(all_nsids)),
  educaim32 = rep(6, length(all_nsids))
)

# Process wave4 (age 17)
if ('w4saim' %in% names(wave4)) {
  # Create mapping for w4saim
  mapping <- c('1.0'=3, '2.0'=3, '3.0'=3, '4.0'=3,
               '5.0'=2, '6.0'=2, '7.0'=2, '8.0'=2,
               '9.0'=1, '10.0'=1, '11.0'=1,
               '-999.0'=6, '12.0'=1, '13.0'=1, '14.0'=6)

  # Apply mapping and update complete_data
  educaim17_values <- as.numeric(recode(wave4$w4saim, !!!mapping))
  educaim17_values[is.na(educaim17_values)] <- 6

  # Create a temporary data frame with NSID and educaim17 values
  temp_data <- data.frame(NSID = wave4$NSID, educaim17 = educaim17_values)

  # Update complete_data directly
  for (i in 1:nrow(temp_data)) {
    nsid <- temp_data$NSID[i]
    value <- temp_data$educaim17[i]
    idx <- which(complete_data$NSID == nsid)
    if (length(idx) > 0) {
      complete_data$educaim17[idx] <- value
    }
  }
}

# Process wave6 (age 19)
if ('W6Saim' %in% names(wave6)) {
  # Create mapping for W6Saim
  mapping <- c('1.0'=5, '2.0'=5, '3.0'=4, '4.0'=4,
               '5.0'=3, '6.0'=3, '7.0'=3, '8.0'=3,
               '9.0'=2, '10.0'=2, '11.0'=2,
               '12.0'=1, '13.0'=1,
               '-999.0'=6, '14.0'=6, '15.0'=6, '16.0'=6)

  # Apply mapping and update complete_data
  educaim19_values <- as.numeric(recode(wave6$W6Saim, !!!mapping))
  educaim19_values[is.na(educaim19_values)] <- 6

  # Create a temporary data frame with NSID and educaim19 values
  temp_data <- data.frame(NSID = wave6$NSID, educaim19 = educaim19_values)

  # Update complete_data directly
  for (i in 1:nrow(temp_data)) {
    nsid <- temp_data$NSID[i]
    value <- temp_data$educaim19[i]
    idx <- which(complete_data$NSID == nsid)
    if (length(idx) > 0) {
      complete_data$educaim19[idx] <- value
    }
  }
}

# Process wave7 (age 20)
if ('W7SAim' %in% names(wave7)) {
  # Create mapping for W7SAim
  mapping <- c('1.0'=1, '2.0'=1, '3.0'=2, '4.0'=2, '5.0'=2,
               '6.0'=3, '7.0'=3, '8.0'=3, '9.0'=3,
               '10.0'=4, '11.0'=5, '12.0'=4, '13.0'=5,
               '-94.0'=6, '14.0'=6, '-999.0'=6)

  # Apply mapping and update complete_data
  educaim20_values <- as.numeric(recode(wave7$W7SAim, !!!mapping))
  educaim20_values[is.na(educaim20_values)] <- 6

  # Create a temporary data frame with NSID and educaim20 values
  temp_data <- data.frame(NSID = wave7$NSID, educaim20 = educaim20_values)

  # Update complete_data directly
  for (i in 1:nrow(temp_data)) {
    nsid <- temp_data$NSID[i]
    value <- temp_data$educaim20[i]
    idx <- which(complete_data$NSID == nsid)
    if (length(idx) > 0) {
      complete_data$educaim20[idx] <- value
    }
  }
}

# Process wave8 (age 25)
if ('W8VCQUC0J' %in% names(wave8)) {
  # Create educaim25 column with default value
  wave8$educaim25 <- 6
  if ('W8VCQUC0J' %in% names(wave8)) wave8$educaim25[wave8$W8VCQUC0J == 1] <- 3
  for (acq_var in c('W8ACQUC0A', 'W8ACQUC0B', 'W8ACQUC0C')) {
    if (acq_var %in% names(wave8)) wave8$educaim25[wave8[[acq_var]] == 1] <- 4
  }
  for (acq_var in c('W8ACQUC0F', 'W8ACQUC0I')) {
    if (acq_var %in% names(wave8)) wave8$educaim25[wave8[[acq_var]] == 1] <- 3
  }
  if ('W8ACQUC0L' %in% names(wave8)) wave8$educaim25[wave8$W8ACQUC0L == 1] <- 2
  if ('W8VCQUC0C' %in% names(wave8)) wave8$educaim25[wave8$W8VCQUC0C == 1] <- 1
  if ('W8VCQUC0D' %in% names(wave8)) wave8$educaim25[wave8$W8VCQUC0D == 1] <- 1

  # Update complete_data directly
  for (i in 1:nrow(wave8)) {
    nsid <- wave8$NSID[i]
    value <- wave8$educaim25[i]
    idx <- which(complete_data$NSID == nsid)
    if (length(idx) > 0) {
      complete_data$educaim25[idx] <- value
    }
  }
}

# Process wave9 (age 32)
if ('W9ACQUC0A' %in% names(wave9)) {
  # Create educaim32 column with default value
  wave9$educaim32 <- 6
  if ('W9ACQUC0A' %in% names(wave9)) wave9$educaim32[wave9$W9ACQUC0A == 1] <- 5
  for (acq_var in c('W9ACQUC0B', 'W9ACQUC0C', 'W9ACQUC0D', 'W9ACQUC0E')) {
    if (acq_var %in% names(wave9)) wave9$educaim32[wave9[[acq_var]] == 1] <- 4
  }
  if ('W9ACQUC0F' %in% names(wave9)) wave9$educaim32[wave9$W9ACQUC0F == 1] <- 3
  if ('W9ACQUC0G' %in% names(wave9)) wave9$educaim32[wave9$W9ACQUC0G == 1] <- 3
  if ('W9ACQUC0H' %in% names(wave9)) wave9$educaim32[wave9$W9ACQUC0H == 1] <- 2
  if ('W9ACQUC0I' %in% names(wave9)) wave9$educaim32[wave9$W9ACQUC0I == 1] <- 1

  # Update complete_data directly
  for (i in 1:nrow(wave9)) {
    nsid <- wave9$NSID[i]
    value <- wave9$educaim32[i]
    idx <- which(complete_data$NSID == nsid)
    if (length(idx) > 0) {
      complete_data$educaim32[idx] <- value
    }
  }
}

# Create factor levels for educaim variables
educaim_labels <- c(
  '1' = 'NVQ Level 1 or equivalent',
  '2' = 'NVQ Level 2 or equivalent',
  '3' = 'NVQ Level 3 or equivalent',
  '4' = 'NVQ Level 4 or equivalent (Higher Education)',
  '5' = 'NVQ Level 5 or equivalent (Higher Degree)',
  '6' = 'Not studying or no detail'
)

# Apply labels to each educaim variable
educaim_vars <- c('educaim17', 'educaim19', 'educaim20', 'educaim25', 'educaim32')
for (var in educaim_vars) {
  complete_data[[var]] <- factor(complete_data[[var]],
                               levels = 1:6,
                               labels = educaim_labels)
}

# Write the output to a CSV file
write_csv(complete_data, 'data/output/cleaned_data.csv')

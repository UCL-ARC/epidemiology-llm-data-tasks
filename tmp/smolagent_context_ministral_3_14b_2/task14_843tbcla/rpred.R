
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(readr)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')
wave5 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge all datasets
merged_data <- full_join(wave1, wave2, by = 'NSID') %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave5, by = 'NSID') %>%
  full_join(wave6, by = 'NSID') %>%
  full_join(wave7, by = 'NSID') %>%
  full_join(wave8, by = 'NSID') %>%
  full_join(wave9, by = 'NSID')

# Function to handle missing values
handle_missing_waves1_7 <- function(x) {
  x <- ifelse(is.na(x), -3, x)
  x <- ifelse(x == -1, -8, x)
  x <- ifelse(x == -92, -9, x)
  return(x)
}

handle_missing_waves8_9 <- function(x) {
  x <- ifelse(is.na(x), -3, x)
  x <- ifelse(x == -1, -1, x)
  return(x)
}

# Create detailed variables (8 categories) for waves 1-4
for (i in 1:4) {
  age <- c(14, 15, 16, 17)[i-1]
  var_name <- paste0('hownteen', age)
  src_var <- paste0('W', i, 'hous12HH')

  if (src_var %in% names(merged_data)) {
    merged_data[[var_name]] <- as.numeric(merged_data[[src_var]])
  } else {
    merged_data[[var_name]] <- -3
  }
  merged_data[[var_name]] <- handle_missing_waves1_7(merged_data[[var_name]])
}

# Create collapsed variables (6 categories) for waves 1-4
for (i in 1:4) {
  age <- c(14, 15, 16, 17)[i-1]
  var_name <- paste0('hown', age)
  src_var <- paste0('W', i, 'hous12HH')

  if (src_var %in% names(merged_data)) {
    merged_data[[var_name]] <- as.numeric(merged_data[[src_var]])
    merged_data[[var_name]][merged_data[[var_name]] %in% c(4,5,6)] <- 4  # Rent it
    merged_data[[var_name]][merged_data[[var_name]] == 8] <- 6  # Other
  } else {
    merged_data[[var_name]] <- -3
  }
  merged_data[[var_name]] <- handle_missing_waves1_7(merged_data[[var_name]])
}

# Create variables for waves 5-7
# Wave 5 (Age 18)
if ('W5Hous12YP' %in% names(merged_data) &&
    'W5Hous12BHH' %in% names(merged_data) &&
    'W5Hous12CHH' %in% names(merged_data)) {

  # Detailed version
  merged_data$hownteen18 <- ifelse(
    merged_data$W5Hous12YP == 1,
    ifelse(merged_data$W5Hous12BHH == 1, 1,
           ifelse(merged_data$W5Hous12BHH == 2, 2,
                  ifelse(merged_data$W5Hous12BHH == 3, 3, 8))),
    ifelse(
      merged_data$W5Hous12YP == 2,
      ifelse(merged_data$W5Hous12CHH %in% c(1,2,3), 4,
             ifelse(merged_data$W5Hous12CHH == 4, 7, 8)),
      as.numeric(merged_data$W5Hous12YP)
    )
  )

  # Collapsed version
  merged_data$hown18 <- ifelse(
    merged_data$W5Hous12YP == 1,
    ifelse(merged_data$W5Hous12BHH == 1, 1,
           ifelse(merged_data$W5Hous12BHH == 2, 2,
                  ifelse(merged_data$W5Hous12BHH == 3, 3, 6))),
    ifelse(
      merged_data$W5Hous12YP == 2,
      ifelse(merged_data$W5Hous12CHH %in% c(1,2,3), 4,
             ifelse(merged_data$W5Hous12CHH == 4, 5, 6)),
      as.numeric(merged_data$W5Hous12YP)
    )
  )
} else {
  merged_data$hownteen18 <- -3
  merged_data$hown18 <- -3
}

# Handle missing values for wave 5
merged_data$hownteen18 <- handle_missing_waves1_7(merged_data$hownteen18)
merged_data$hown18 <- handle_missing_waves1_7(merged_data$hown18)

# Wave 6 (Age 19)
if ('W6Hous12YP' %in% names(merged_data) &&
    'W6Hous12bYP' %in% names(merged_data) &&
    'W6Hous12cYP' %in% names(merged_data)) {

  # Detailed version
  merged_data$hownteen19 <- ifelse(
    merged_data$W6Hous12YP == 1,
    ifelse(merged_data$W6Hous12bYP == 1, 1,
           ifelse(merged_data$W6Hous12bYP == 2, 2,
                  ifelse(merged_data$W6Hous12bYP == 3, 3, 8))),
    ifelse(
      merged_data$W6Hous12YP == 2,
      ifelse(merged_data$W6Hous12cYP %in% c(1,2,3), 4,
             ifelse(merged_data$W6Hous12cYP == 4, 7, 8)),
      as.numeric(merged_data$W6Hous12YP)
    )
  )

  # Collapsed version
  merged_data$hown19 <- ifelse(
    merged_data$W6Hous12YP == 1,
    ifelse(merged_data$W6Hous12bYP == 1, 1,
           ifelse(merged_data$W6Hous12bYP == 2, 2,
                  ifelse(merged_data$W6Hous12bYP == 3, 3, 6))),
    ifelse(
      merged_data$W6Hous12YP == 2,
      ifelse(merged_data$W6Hous12cYP %in% c(1,2,3), 4,
             ifelse(merged_data$W6Hous12cYP == 4, 5, 6)),
      as.numeric(merged_data$W6Hous12YP)
    )
  )
} else {
  merged_data$hownteen19 <- -3
  merged_data$hown19 <- -3
}

# Handle missing values for wave 6
merged_data$hownteen19 <- handle_missing_waves1_7(merged_data$hownteen19)
merged_data$hown19 <- handle_missing_waves1_7(merged_data$hown19)

# Wave 7 (Age 20)
if ('W7Hous12YP' %in% names(merged_data) &&
    'W7Hous12bYP' %in% names(merged_data) &&
    'W7Hous12cYP' %in% names(merged_data)) {

  # Detailed version
  merged_data$hownteen20 <- ifelse(
    merged_data$W7Hous12YP == 1,
    ifelse(merged_data$W7Hous12bYP == 1, 1,
           ifelse(merged_data$W7Hous12bYP == 2, 2,
                  ifelse(merged_data$W7Hous12bYP == 3, 3, 8))),
    ifelse(
      merged_data$W7Hous12YP == 2,
      ifelse(merged_data$W7Hous12cYP %in% c(1,2,3), 4,
             ifelse(merged_data$W7Hous12cYP == 4, 7, 8)),
      as.numeric(merged_data$W7Hous12YP)
    )
  )

  # Collapsed version
  merged_data$hown20 <- ifelse(
    merged_data$W7Hous12YP == 1,
    ifelse(merged_data$W7Hous12bYP == 1, 1,
           ifelse(merged_data$W7Hous12bYP == 2, 2,
                  ifelse(merged_data$W7Hous12bYP == 3, 3, 6))),
    ifelse(
      merged_data$W7Hous12YP == 2,
      ifelse(merged_data$W7Hous12cYP %in% c(1,2,3), 4,
             ifelse(merged_data$W7Hous12cYP == 4, 5, 6)),
      as.numeric(merged_data$W7Hous12YP)
    )
  )
} else {
  merged_data$hownteen20 <- -3
  merged_data$hown20 <- -3
}

# Handle missing values for wave 7
merged_data$hownteen20 <- handle_missing_waves1_7(merged_data$hownteen20)
merged_data$hown20 <- handle_missing_waves1_7(merged_data$hown20)

# Create variables for waves 8-9
# Wave 8 (Age 25)
if ('W8TENURE' %in% names(merged_data)) {
  merged_data$hown25 <- ifelse(
    merged_data$W8TENURE == 6, 6,
    ifelse(merged_data$W8TENURE == 7, 6,
           as.numeric(merged_data$W8TENURE))
  )
} else {
  merged_data$hown25 <- -3
}
merged_data$hown25 <- handle_missing_waves8_9(merged_data$hown25)

# Wave 9 (Age 32)
if ('W9DTENURE' %in% names(merged_data)) {
  merged_data$hown32 <- ifelse(
    merged_data$W9DTENURE == 6, 6,
    ifelse(merged_data$W9DTENURE == 7, 6,
           as.numeric(merged_data$W9DTENURE))
  )
} else {
  merged_data$hown32 <- -3
}
merged_data$hown32 <- handle_missing_waves8_9(merged_data$hown32)

# Select final variables
final_vars <- c('NSID', grep('^hown', names(merged_data), value = TRUE))
final_data <- merged_data %>% select(all_of(final_vars))

# Write output
write_csv(final_data, 'data/output/cleaned_data.csv')

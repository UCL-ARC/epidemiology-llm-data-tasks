
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# Load all files into separate objects
files <- list(
  wave1 = readr::read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t'),
  wave2 = readr::read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t'),
  wave3 = readr::read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t'),
  wave4 = readr::read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t'),
  wave5 = readr::read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t'),
  wave6 = readr::read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t'),
  wave7 = readr::read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t'),
  wave8 = readr::read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t'),
  wave9 = readr::read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')
)

# Function to map missing values to standard codes
map_missing_values <- function(x) {
  if (is.numeric(x)) {
    x <- ifelse(x %in% c(-999, -998, -997, -995), -2, x)
    x <- ifelse(is.na(x), -3, x)
    x <- ifelse(x == -92, -9, x)
    x <- ifelse(x == -91, -1, x)
  }
  return(x)
}

# Function to harmonize rental categories
harmonize_rental <- function(x) {
  if (is.numeric(x)) {
    x <- ifelse(x %in% c(4, 5, 6), 4, x)
  }
  return(x)
}

# Process each wave and create derived variables
processed_waves <- list()

for (i in 1:9) {
  wave_data <- files[[paste0('wave', i)]]

  # Create detailed tenure variables
  if (i == 1 && 'W1hous12HH' %in% names(wave_data)) {
    wave_data$hownteen14 <- map_missing_values(wave_data$W1hous12HH)
  } else if (i == 2 && 'W2Hous12HH' %in% names(wave_data)) {
    wave_data$hownteen15 <- map_missing_values(wave_data$W2Hous12HH)
  } else if (i == 3 && 'W3hous12HH' %in% names(wave_data)) {
    wave_data$hownteen16 <- map_missing_values(wave_data$W3hous12HH)
  } else if (i == 4 && 'W4Hous12HH' %in% names(wave_data)) {
    wave_data$hownteen17 <- map_missing_values(wave_data$W4Hous12HH)
  } else if (i == 5 && 'W5Hous12YP' %in% names(wave_data) && 'W5Hous12BHH' %in% names(wave_data) && 'W5Hous12CHH' %in% names(wave_data)) {
    wave_data$hownteen18 <- ifelse(wave_data$W5Hous12YP == 1, map_missing_values(wave_data$W5Hous12BHH), map_missing_values(wave_data$W5Hous12CHH))
  } else if (i == 6 && 'W6Hous12YP' %in% names(wave_data) && 'W6Hous12bYP' %in% names(wave_data) && 'W6Hous12cYP' %in% names(wave_data)) {
    wave_data$hownteen19 <- ifelse(wave_data$W6Hous12YP == 1, map_missing_values(wave_data$W6Hous12bYP), map_missing_values(wave_data$W6Hous12cYP))
  } else if (i == 7 && 'W7Hous12YP' %in% names(wave_data) && 'W7Hous12bYP' %in% names(wave_data) && 'W7Hous12cYP' %in% names(wave_data)) {
    wave_data$hownteen20 <- ifelse(wave_data$W7Hous12YP == 1, map_missing_values(wave_data$W7Hous12bYP), map_missing_values(wave_data$W7Hous12cYP))
  }

  # Create collapsed tenure variables
  if (i == 1 && 'W1hous12HH' %in% names(wave_data)) {
    wave_data$hown14 <- harmonize_rental(map_missing_values(wave_data$W1hous12HH))
  } else if (i == 2 && 'W2Hous12HH' %in% names(wave_data)) {
    wave_data$hown15 <- harmonize_rental(map_missing_values(wave_data$W2Hous12HH))
  } else if (i == 3 && 'W3hous12HH' %in% names(wave_data)) {
    wave_data$hown16 <- harmonize_rental(map_missing_values(wave_data$W3hous12HH))
  } else if (i == 4 && 'W4Hous12HH' %in% names(wave_data)) {
    wave_data$hown17 <- harmonize_rental(map_missing_values(wave_data$W4Hous12HH))
  } else if (i == 5 && 'W5Hous12YP' %in% names(wave_data) && 'W5Hous12BHH' %in% names(wave_data) && 'W5Hous12CHH' %in% names(wave_data)) {
    tenure <- ifelse(wave_data$W5Hous12YP == 1, wave_data$W5Hous12BHH, wave_data$W5Hous12CHH)
    wave_data$hown18 <- harmonize_rental(map_missing_values(tenure))
  } else if (i == 6 && 'W6Hous12YP' %in% names(wave_data) && 'W6Hous12bYP' %in% names(wave_data) && 'W6Hous12cYP' %in% names(wave_data)) {
    tenure <- ifelse(wave_data$W6Hous12YP == 1, wave_data$W6Hous12bYP, wave_data$W6Hous12cYP)
    wave_data$hown19 <- harmonize_rental(map_missing_values(tenure))
  } else if (i == 7 && 'W7Hous12YP' %in% names(wave_data) && 'W7Hous12bYP' %in% names(wave_data) && 'W7Hous12cYP' %in% names(wave_data)) {
    tenure <- ifelse(wave_data$W7Hous12YP == 1, wave_data$W7Hous12bYP, wave_data$W7Hous12cYP)
    wave_data$hown20 <- harmonize_rental(map_missing_values(tenure))
  } else if (i == 8 && 'W8TENURE' %in% names(wave_data)) {
    wave_data$hown25 <- harmonize_rental(map_missing_values(wave_data$W8TENURE))
  } else if (i == 9 && 'W9DTENURE' %in% names(wave_data)) {
    wave_data$hown32 <- harmonize_rental(map_missing_values(wave_data$W9DTENURE))
  }

  processed_waves[[i]] <- wave_data
}

# Merge all datasets by NSID to maintain the full cohort frame
combined_data <- reduce(files, full_join, by = 'NSID')

# Collect all derived variables from processed waves
derived_vars <- sapply(processed_waves, function(df) names(df)[grepl('^hown', names(df))])
derived_vars <- unique(unlist(derived_vars))

# Create a dataframe with only NSID and derived variables
final_data <- combined_data %>%
  select(NSID) %>%
  left_join(bind_rows(processed_waves, .id = 'source') %>%
              select(NSID, all_of(derived_vars)), by = 'NSID')

# Write the final dataset to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

# Print confirmation message
cat('Successfully created cleaned_data.csv with NSID and derived housing tenure variables.\n')
cat('Derived variables included:', paste(derived_vars, collapse = ', '), '\n')

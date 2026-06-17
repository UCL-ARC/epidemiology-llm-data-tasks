
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# Suppress messages for cleaner output
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
})

# Define file paths
files <- list(
  wave_one = 'data/input/wave_one_lsype_family_background_2020.tab',
  wave_two = 'data/input/wave_two_lsype_family_background_2020.tab',
  wave_three = 'data/input/wave_three_lsype_family_background_2020.tab',
  wave_four = 'data/input/wave_four_lsype_family_background_2020.tab',
  wave_five = 'data/input/wave_five_lsype_family_background_2020.tab',
  wave_six = 'data/input/wave_six_lsype_young_person_2020.tab',
  wave_seven = 'data/input/wave_seven_lsype_young_person_2020.tab',
  wave_eight = 'data/input/ns8_2015_main_interview.tab',
  wave_nine = 'data/input/ns9_2022_derived_variables.tab'
)

# Load files
load_file <- function(file_path) {
  read_delim(file_path, delim = '\t', show_col_types = FALSE)
}

loaded_files <- map(files, load_file)

# Merge all datasets by NSID
merged_data <- loaded_files[[1]]
for (i in 2:length(loaded_files)) {
  merged_data <- merged_data %>%
    full_join(loaded_files[[i]], by = 'NSID', copy = FALSE)
}

# Function to map missing values
map_missing_values <- function(x, wave) {
  x <- as.numeric(x)
  if (wave %in% c(1, 2, 3, 4, 5, 6, 7)) {
    x[x == -1] <- -8  # Don't know maps to -8 for sweeps 1-7
  } else if (wave == 8 || wave == 9) {
    # No change needed for wave 8-9
  }
  return(x)
}

# Create detailed housing tenure variables for waves 1-4
merged_data <- merged_data %>%
  mutate(
    hownteen14 = map_missing_values(W1hous12HH, 1),
    hownteen15 = map_missing_values(W2Hous12HH, 2),
    hownteen16 = map_missing_values(W3hous12HH, 3),
    hownteen17 = map_missing_values(W4Hous12HH, 4)
  )

# Handle sweeps 5-7 with subtype variables for detailed variables
for (wave in 5:7) {
  owned_subtype <- paste0('W', wave, 'Hous12BHH')
  rented_subtype <- paste0('W', wave, 'Hous12CHH')
  tenure_type <- paste0('W', wave, 'Hous12YP')
  age <- wave + 13  # 5->18, 6->19, 7->20

  merged_data[[paste0('hownteen', age)]] <- NA_real_

  # Owned cases
  owned_mask <- !is.na(merged_data[[tenure_type]]) &
    merged_data[[tenure_type]] == 1 &
    !is.na(merged_data[[owned_subtype]])
  merged_data[[paste0('hownteen', age)]][owned_mask] <-
    map_missing_values(merged_data[[owned_subtype]][owned_mask], wave)

  # Rented cases
  rented_mask <- !is.na(merged_data[[tenure_type]]) &
    merged_data[[tenure_type]] == 2 &
    !is.na(merged_data[[rented_subtype]])
  merged_data[[paste0('hownteen', age)]][rented_mask] <-
    map_missing_values(merged_data[[rented_subtype]][rented_mask], wave)

  # Some other arrangement
  other_mask <- !is.na(merged_data[[tenure_type]]) &
    merged_data[[tenure_type]] == 3
  merged_data[[paste0('hownteen', age)]][other_mask] <- 8
}

# Create collapsed housing tenure variables for waves 1-4
merged_data <- merged_data %>%
  mutate(
    hown14 = {
      x <- map_missing_values(W1hous12HH, 1)
      x[x == 4] <- 4  # Council
      x[x == 5] <- 4  # Housing Association
      x[x == 6] <- 4  # Private
      x[x == 8] <- 6  # Some other arrangement
      x
    },
    hown15 = {
      x <- map_missing_values(W2Hous12HH, 2)
      x[x == 4] <- 4
      x[x == 5] <- 4
      x[x == 6] <- 4
      x[x == 8] <- 6
      x
    },
    hown16 = {
      x <- map_missing_values(W3hous12HH, 3)
      x[x == 4] <- 4
      x[x == 5] <- 4
      x[x == 6] <- 4
      x[x == 8] <- 6
      x
    },
    hown17 = {
      x <- map_missing_values(W4Hous12HH, 4)
      x[x == 4] <- 4
      x[x == 5] <- 4
      x[x == 6] <- 4
      x[x == 8] <- 6
      x
    }
  )

# Handle sweeps 5-7 collapsed variables
for (wave in 5:7) {
  owned_subtype <- paste0('W', wave, 'Hous12BHH')
  rented_subtype <- paste0('W', wave, 'Hous12CHH')
  tenure_type <- paste0('W', wave, 'Hous12YP')
  age <- wave + 13  # 5->18, 6->19, 7->20

  merged_data[[paste0('hown', age)]] <- NA_real_

  # Owned cases
  owned_mask <- !is.na(merged_data[[tenure_type]]) &
    merged_data[[tenure_type]] == 1 &
    !is.na(merged_data[[owned_subtype]])
  owned_values <- map_missing_values(merged_data[[owned_subtype]][owned_mask], wave)
  owned_values[owned_values == 1] <- 1  # Owned outright
  owned_values[owned_values == 2] <- 2  # Own with mortgage
  owned_values[owned_values == 3] <- 3  # Shared ownership
  owned_values[owned_values == 4] <- 6  # Other
  merged_data[[paste0('hown', age)]][owned_mask] <- owned_values

  # Rented cases
  rented_mask <- !is.na(merged_data[[tenure_type]]) &
    merged_data[[tenure_type]] == 2 &
    !is.na(merged_data[[rented_subtype]])
  rented_values <- map_missing_values(merged_data[[rented_subtype]][rented_mask], wave)
  rented_values[rented_values %in% c(1, 2, 3)] <- 4  # Rent it
  rented_values[rented_values == 4] <- 5  # Rent free
  rented_values[rented_values == 5] <- 6  # Other
  merged_data[[paste0('hown', age)]][rented_mask] <- rented_values

  # Some other arrangement
  other_mask <- !is.na(merged_data[[tenure_type]]) &
    merged_data[[tenure_type]] == 3
  merged_data[[paste0('hown', age)]][other_mask] <- 6
}

# Handle sweeps 8-9
merged_data <- merged_data %>%
  mutate(
    hown25 = {
      x <- map_missing_values(W8TENURE, 8)
      x[x == 6] <- 6  # Squatting
      x[x == 7] <- 6  # Other
      x
    },
    hown32 = {
      x <- map_missing_values(W9DTENURE, 9)
      x[x == 6] <- 6  # Squatting
      x[x == 7] <- 6  # Other
      x
    }
  )

# Convert NA to -3 for missing values
for (age in c(14, 15, 16, 17, 18, 19, 20, 25, 32)) {
  for (var_type in c('hownteen', 'hown')) {
    var_name <- paste0(var_type, age)
    if (var_name %in% names(merged_data)) {
      merged_data[[var_name]][is.na(merged_data[[var_name]])] <- -3
    }
  }
}

# Select only NSID and the final derived variables
final_vars <- c('NSID')
for (age in c(14, 15, 16, 17)) {
  final_vars <- c(final_vars, paste0('hownteen', age))
}
for (age in c(14, 15, 16, 17, 18, 19, 20, 25, 32)) {
  final_vars <- c(final_vars, paste0('hown', age))
}

final_data <- merged_data %>% select(all_of(final_vars))

# Write the final cleaned data to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

# Print success message
cat('Processing completed successfully. Cleaned data saved to data/output/cleaned_data.csv\n')

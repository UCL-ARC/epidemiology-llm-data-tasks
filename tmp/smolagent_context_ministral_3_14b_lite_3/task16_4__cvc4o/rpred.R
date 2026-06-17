
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load metadata for reference
metadata <- list(
  wave_one = list(
    file = 'data/input/wave_one_lsype_family_background_2020.tab',
    var = 'W1GrsswkHH',
    age = 14,
    missing_mapping = c('-999' = -2, '-992' = -2, '-99' = -3, '-94' = -8, '-92' = -9, '-91' = -1, '-3' = -3, '-1' = -8)
  ),
  wave_two = list(
    file = 'data/input/wave_two_lsype_family_background_2020.tab',
    var = 'W2GrsswkHH',
    age = 15,
    missing_mapping = c('-999' = -2, '-992' = -2, '-99' = -3, '-94' = -8, '-92' = -9, '-91' = -1, '-3' = -3, '-1' = -8)
  ),
  wave_three = list(
    file = 'data/input/wave_three_lsype_family_background_2020.tab',
    var = 'W3incestw',
    age = 16,
    missing_mapping = c('-99' = -3, '-92' = -9, '-1' = -8)
  ),
  wave_four = list(
    file = 'data/input/wave_four_lsype_family_background_2020.tab',
    var = 'w4IncEstW',
    age = 17,
    missing_mapping = c('-996' = -1, '-99' = -3, '-92' = -9, '-1' = -8)
  )
)

# Load all datasets
data_files <- map(metadata, ~ read_delim(.x$file, delim = '\t'))

# Merge datasets by NSID
merged_data <- reduce(data_files, full_join, by = 'NSID')

# Define income bands and their labels
income_bands <- c(
  '1' = 'Up to £49',
  '2' = '£50 up to £99',
  '3' = '£100 up to £199',
  '4' = '£200 up to £299',
  '5' = '£300 up to £399',
  '6' = '£400 up to £499',
  '7' = '£500 up to £599',
  '8' = '£600 up to £699',
  '9' = '£700 up to £799',
  '10' = '£800 up to £899',
  '11' = '£900 up to £999',
  '12' = '£1,000 or more'
)

# Function to map missing values
map_missing <- function(x, mapping) {
  x <- as.numeric(x)
  for (code in names(mapping)) {
    x[x == as.numeric(code)] <- mapping[[code]]
  }
  x[is.na(x)] <- -3
  return(x)
}

# Create continuous variables for ages 14 and 15
merged_data <- merged_data %>%
  mutate(
    inc_cont_14 = map_missing(W1GrsswkHH, metadata$wave_one$missing_mapping),
    inc_cont_15 = map_missing(W2GrsswkHH, metadata$wave_two$missing_mapping)
  )

# Create banded variables for ages 14, 15, 16, and 17
banded_vars <- function(data, wave_info) {
  var_name <- wave_info$var
  age <- wave_info$age
  missing_mapping <- wave_info$missing_mapping

  # Map missing values first
  inc <- map_missing(data[[var_name]], missing_mapping)

  # Convert to banded variable
  inc_banded <- as.character(cut(
    inc[inc >= 0],  # Ignore missing values for banding
    breaks = c(-Inf, 49, 99, 199, 299, 399, 499, 599, 699, 799, 899, 999, Inf),
    labels = names(income_bands),
    include.lowest = TRUE
  ))

  # Handle missing values
  inc_banded[inc < 0] <- NA

  # Create labelled factor
  inc_banded <- factor(inc_banded, levels = names(income_bands), labels = income_bands)

  # Return banded variable with age suffix
  new_var_name <- paste0('inc_band_', age)
  data[[new_var_name]] <- inc_banded
  return(data)
}

# Apply banded variable creation for all waves
for (wave in names(metadata)) {
  merged_data <- banded_vars(merged_data, metadata[[wave]])
}

# Remove raw source variables
raw_vars <- c('W1GrsswkHH', 'W2GrsswkHH', 'W3incestw', 'w4IncEstW')
merged_data <- merged_data %>% select(-any_of(raw_vars))

# Write output file
write_csv(merged_data, 'data/output/cleaned_data.csv')

# Check for errors and print summary
summary(merged_data)

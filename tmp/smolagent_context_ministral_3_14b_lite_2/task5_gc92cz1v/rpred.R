
# Load required packages
library(haven)
library(dplyr)
library(tidyr)

# Define file paths
file_paths <- list(
  wave_one = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_six = 'data/input/wave_six_lsype_young_person_2020.tab',
  wave_eight = 'data/input/ns8_2015_derived.tab',
  wave_nine = 'data/input/ns9_2022_derived_variables.tab'
)

# Load files
wave_one_data <- read.table(file_paths$wave_one, header = TRUE, sep = '\t', fill = TRUE, na.strings = c('NA', ''))
wave_six_data <- read.table(file_paths$wave_six, header = TRUE, sep = '\t', fill = TRUE, na.strings = c('NA', ''))
wave_eight_data <- read.table(file_paths$wave_eight, header = TRUE, sep = '\t', fill = TRUE, na.strings = c('NA', ''))
wave_nine_data <- read.table(file_paths$wave_nine, header = TRUE, sep = '\t', fill = TRUE, na.strings = c('NA', ''))

# Ensure NSID is character
wave_one_data$NSID <- as.character(wave_one_data$NSID)
wave_six_data$NSID <- as.character(wave_six_data$NSID)
wave_eight_data$NSID <- as.character(wave_eight_data$NSID)
wave_nine_data$NSID <- as.character(wave_nine_data$NSID)

# Merge datasets
merged_data <- wave_one_data %>%
  full_join(wave_six_data, by = 'NSID') %>%
  full_join(wave_eight_data, by = 'NSID') %>%
  full_join(wave_nine_data, by = 'NSID')

# Define mapping functions
map_w6 <- function(x) {
  values <- c(-997, -97, -92, -91, -1, 1, 2, 3, 4, 5)
  mapped <- c(-2, -7, -9, -1, -8, 1, 2, 3, 4, 5)
  ifelse(x %in% values, mapped[match(x, values)], NA_integer_)
}

map_w8w9 <- function(x) {
  values <- c(-9, -8, -1, 1, 2, 6, 3, 4, 7, 8, 5)
  mapped <- c(-9, -8, -1, 1, 2, 2, 3, 4, 3, 4, 5)
  ifelse(x %in% values, mapped[match(x, values)], NA_integer_)
}

# Create partnership variables
merged_data <- merged_data %>%
  mutate(
    partnr19 = map_w6(as.integer(W6MarStatYP)),
    partnr25 = map_w8w9(as.integer(W8DMARSTAT)),
    partnr32 = map_w8w9(as.integer(W9DMARSTAT)),
    partnradu25 = ifelse(!is.na(W8DMARSTAT), as.integer(W8DMARSTAT), NA_integer_),
    partnradu32 = ifelse(!is.na(W9DMARSTAT), as.integer(W9DMARSTAT), NA_integer_)
  )

# Function to convert numeric codes to factors with labels
create_factor <- function(x) {
  # Define levels and labels
  levels <- c(-9, -8, -7, -2, -1, 1, 2, 3, 4, 5)
  labels <- c('Refusal', 'Insufficient information', 'Prefer not to say',
              'Schedule not applicable / script error', 'Not applicable',
              'Single, never married', 'Married', 'Separated', 'Divorced', 'Widowed')

  # Create factor
  factor(x, levels = levels, labels = labels)
}

# Apply factor conversion to partnership variables
merged_data$partnr19 <- create_factor(merged_data$partnr19)
merged_data$partnr25 <- create_factor(merged_data$partnr25)
merged_data$partnr32 <- create_factor(merged_data$partnr32)

# Select and save output
output_data <- merged_data %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Write output to CSV
write.csv(output_data, 'data/output/cleaned_data.csv', row.names = FALSE)

message('Cleaned data has been written to data/output/cleaned_data.csv')

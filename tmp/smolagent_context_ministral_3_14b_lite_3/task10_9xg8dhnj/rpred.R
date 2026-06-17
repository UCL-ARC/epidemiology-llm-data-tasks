
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_five_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

# Load all files with explicit column types
all_datasets <- lapply(files, function(file) {
  file_path <- paste0('data/input/', file)
  data <- readr::read_delim(file_path, delim = '\t')
  # Ensure NSID is character
  data$NSID <- as.character(data$NSID)
  data
})

# Merge datasets by NSID one by one
merged_data <- all_datasets[[1]]
for (i in 2:length(all_datasets)) {
  merged_data <- full_join(merged_data, all_datasets[[i]], by = 'NSID')
}

# Function to map missing values
map_missing <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(as.character(x))
  }
  x[is.na(x)] <- -3  # Not asked/not interviewed
  x[x == -999] <- -2  # Schedule not applicable
  x[x == -94] <- -8   # Insufficient information
  x[x == -92] <- -9   # Refused
  x[x == -91] <- -1   # Not applicable
  x[x == -9] <- -9    # Refused
  x[x == -8] <- -8    # Insufficient information
  x[x == -1] <- -1    # Not applicable
  return(x)
}

# Create ecoact variables with 6-category scheme
# Age 17
if ('W4empsYP' %in% names(merged_data)) {
  merged_data$ecoact17 <- map_missing(merged_data$W4empsYP)
  merged_data$ecoact17 <- case_when(
    merged_data$ecoact17 %in% c(1, 2) ~ 1,  # Paid work
    merged_data$ecoact17 == 3 ~ 2,           # Unemployed
    merged_data$ecoact17 == 4 ~ 3,           # Training course
    merged_data$ecoact17 == 5 ~ 4,           # Education
    merged_data$ecoact17 %in% c(6, 7, 8, 9) ~ 5,  # Family, Retired, Sick, Other
    TRUE ~ NA_integer_
  )
}

# Age 18
if ('W5mainactYP' %in% names(merged_data)) {
  merged_data$ecoact18 <- map_missing(merged_data$W5mainactYP)
  merged_data$ecoact18 <- case_when(
    merged_data$ecoact18 %in% c(1, 3) ~ 1,  # Paid work/Apprenticeship
    merged_data$ecoact18 %in% c(2, 6, 10) ~ 2,  # Education-related
    merged_data$ecoact18 %in% c(4, 5, 9) ~ 3,  # Training-related
    merged_data$ecoact18 %in% c(7, 8) ~ 4,    # Unemployed/Family
    merged_data$ecoact18 %in% c(11) ~ 5,       # Waiting for job
    TRUE ~ NA_integer_
  )
}

# Age 19
if ('W6TCurrentAct' %in% names(merged_data)) {
  merged_data$ecoact19 <- map_missing(merged_data$W6TCurrentAct)
  merged_data$ecoact19 <- case_when(
    merged_data$ecoact19 == 3 ~ 1,            # Paid work
    merged_data$ecoact19 %in% c(1, 2, 4, 5, 10) ~ 2,  # Education-related
    merged_data$ecoact19 %in% c(7, 8) ~ 3,    # Family/Unemployed
    merged_data$ecoact19 %in% c(6, 9, 11) ~ 4,  # Waiting/Volunteering
    TRUE ~ NA_integer_
  )
}

# Age 20
if ('W7TCurrentAct' %in% names(merged_data)) {
  merged_data$ecoact20 <- map_missing(merged_data$W7TCurrentAct)
  merged_data$ecoact20 <- case_when(
    merged_data$ecoact20 %in% c(3, 9, 10) ~ 1,  # Paid work/Volunteering
    merged_data$ecoact20 %in% c(1, 2, 4, 5) ~ 2,  # Education-related
    merged_data$ecoact20 %in% c(7, 8) ~ 3,        # Family/Unemployed
    merged_data$ecoact20 %in% c(6, 11, 12, 13, 14, 15) ~ 4,  # Other
    TRUE ~ NA_integer_
  )
}

# Create detailed variables for ages 25 and 32
levels_detailed <- c(-9, -8, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
labels_detailed <- c(
  'Refused', 'Insufficient information', 'Not asked/not interviewed',
  'Schedule not applicable', 'Not applicable',
  'Paid work', 'Self-employed', 'Unpaid/voluntary work',
  'Unemployed', 'Education', 'Apprenticeship',
  'Government training scheme', 'Sick/disabled',
  'Looking after family', 'Other'
)

if ('W8DACTIVITYC' %in% names(merged_data)) {
  merged_data$ecoactadu25 <- factor(
    map_missing(merged_data$W8DACTIVITYC),
    levels = levels_detailed,
    labels = labels_detailed
  )
  merged_data$ecoact25 <- map_missing(merged_data$W8DACTIVITYC)
}

if ('W9DACTIVITYC' %in% names(merged_data)) {
  merged_data$ecoactadu32 <- factor(
    map_missing(merged_data$W9DACTIVITYC),
    levels = levels_detailed,
    labels = labels_detailed
  )
  merged_data$ecoact32 <- map_missing(merged_data$W9DACTIVITYC)
}

# Define labels for 6-category scheme
six_labels <- c(
  'Paid work',
  'Education/training',
  'Training course',
  'Education-related',
  'Family/Unemployed',
  'Other'
)

# Convert ecoact variables to factors
for (age in c(17, 18, 19, 20)) {
  var_name <- paste0('ecoact', age)
  if (var_name %in% names(merged_data)) {
    merged_data[[var_name]] <- factor(
      merged_data[[var_name]],
      levels = 1:6,
      labels = six_labels
    )
  }
}

# Convert ecoact25 and ecoact32 to factors with their own labels
if ('ecoact25' %in% names(merged_data)) {
  merged_data$ecoact25 <- factor(
    merged_data$ecoact25,
    levels = levels_detailed,
    labels = labels_detailed
  )
}

if ('ecoact32' %in% names(merged_data)) {
  merged_data$ecoact32 <- factor(
    merged_data$ecoact32,
    levels = levels_detailed,
    labels = labels_detailed
  )
}

# Select only required variables for output
output_vars <- c('NSID')
for (age in c(17, 18, 19, 20)) {
  var_name <- paste0('ecoact', age)
  if (var_name %in% names(merged_data)) output_vars <- c(output_vars, var_name)
}

output_vars <- c(output_vars,
                 'ecoact25', 'ecoact32',
                 'ecoactadu25', 'ecoactadu32')

# Ensure all variables exist
output_vars <- output_vars[output_vars %in% names(merged_data)]

# Write output to CSV
readr::write_csv(merged_data[, output_vars], 'data/output/cleaned_data.csv')

# Confirm output
cat('Data cleaning completed successfully!\n')
cat('Output file: data/output/cleaned_data.csv\n')
cat('Variables included:', paste(output_vars, collapse = ', '), '\n')

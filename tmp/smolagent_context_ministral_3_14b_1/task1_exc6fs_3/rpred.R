
# Load required packages
library(haven)
library(readr)

# Define file paths and variable names in priority order (most recent first)
files <- c(
  'ns9_2022_main_interview.tab',
  'ns8_2015_main_interview.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_five_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_three_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_one_lsype_young_person_2020.tab'
)

vars <- c('W9DSEX', 'W8CMSEX', 'W7Sex', 'W6Sex', 'W5SexYP',
          'W4SexYP', 'W3sexYP', 'W2SexYP', 'W1sexYP')

# Load all datasets
data_list <- lapply(files, function(f) {
  read_delim(paste0('data/input/', f), delim = '\t')
})

# Initialize with wave 1 data
base_data <- data_list[[9]]
base_data$sex <- rep(-3, nrow(base_data))  # Initialize with missing value

# Define missing value mapping function
map_missing <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x == -999 | x == -998 | x == -997 | x == -995] <- -2
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -99] <- -3
  x[x == -9] <- -9
  x[x == -8] <- -8
  x[x == -1] <- -1
  return(x)
}

# Process each wave in priority order
for (i in seq_along(files)) {
  current_data <- data_list[[i]]
  current_var <- vars[i]

  # Map missing values
  current_data[[current_var]] <- map_missing(current_data[[current_var]])

  # Merge with base data
  temp_data <- merge(base_data, current_data[, c('NSID', current_var)],
                    by = 'NSID', all.x = TRUE)

  # Update sex values using base R operations
  if (i == 1) {
    # First wave - use these values where available
    valid_idx <- !is.na(temp_data[[current_var]]) &
                 temp_data[[current_var]] %in% c(1, 2)
    temp_data$sex[valid_idx] <- temp_data[[current_var]][valid_idx]
  } else {
    # Subsequent waves - update only where sex is still missing
    missing_idx <- is.na(temp_data$sex)
    valid_idx <- !is.na(temp_data[[current_var]]) &
                 temp_data[[current_var]] %in% c(1, 2)
    temp_data$sex[missing_idx & valid_idx] <-
      temp_data[[current_var]][missing_idx & valid_idx]
  }

  base_data <- temp_data
}

# Create labelled factor for sex
levels <- c(-9, -8, -7, -3, -2, -1, 1, 2)
labels <- c('Refusal', 'Dont Know', 'Prefer Not to Say',
            'Not Interviewed', 'Schedule Not Applicable',
            'Not Applicable', 'Male', 'Female')
base_data$sex <- factor(base_data$sex,
                       levels = levels,
                       labels = labels)

# Select only necessary columns
final_data <- base_data[, c('NSID', 'sex')]

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

message('Cleaned data with derived sex variable has been written to data/output/cleaned_data.csv')

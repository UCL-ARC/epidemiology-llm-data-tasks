
# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

# Define file paths
file_paths <- c(
  'data/input/wave_one_lsype_young_person_2020.tab',
  'data/input/wave_four_lsype_young_person_2020.tab',
  'data/input/wave_five_lsype_young_person_2020.tab',
  'data/input/wave_six_lsype_young_person_2020.tab',
  'data/input/wave_seven_lsype_young_person_2020.tab',
  'data/input/ns8_2015_derived.tab',
  'data/input/ns9_2022_derived_variables.tab'
)

# Load all files
load_files <- function(paths) {
  map(paths, ~ read_delim(.x, delim = '\t'))
}

# Load data
data_list <- load_files(file_paths)

# Function to map missing values
map_missing <- function(x) {
  if (!is.numeric(x)) return(x)
  case_when(
    x == -999 | x == -998 | x == -997 | x == -995 ~ -2,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -99 | x == -3 | is.na(x) ~ -3,
    TRUE ~ as.numeric(x)
  )
}

# Function to collapse detailed categories to 6 categories
collapse_ecoact <- function(x) {
  if (!is.numeric(x)) return(x)
  case_when(
    # Education categories
    x == 1 | x == 4 | x == 5 | x == 6 | x == 9 | x == 10 | x == 11 | x == 14 | x == 15 | x == 2 ~ 1,
    # Work categories
    x == 2 | x == 3 | x == 10 | x == 14 | x == 15 | x == 1 | x == 3 | x == 4 | x == 5 | x == 6 ~ 2,
    # Unemployed categories
    x == 7 | x == 8 | x == 11 ~ 3,
    # Training categories
    x == 4 | x == 5 | x == 7 | x == 12 | x == 13 ~ 4,
    # Apprenticeship
    x == 5 | x == 6 ~ 5,
    # Other categories
    x == 8 | x == 9 | x == 12 | x == 13 | x == 14 | x == 15 | x == 10 ~ 6,
    TRUE ~ x
  )
}

# Create empty data frame with just NSID for merging
base_df <- data.frame(NSID = character(0))
names(base_df) <- 'NSID'

# Process each wave and merge
processed_data <- map2(data_list, file_paths, ~ {
  df <- .x

  # Process wave-specific variables with explicit checks
  if (grepl('wave_four', .y) && 'W4empsYP' %in% names(df)) {
    df$ecoact17 <- collapse_ecoact(map_missing(df$W4empsYP))
  }
  if (grepl('wave_five', .y) && 'W5mainactYP' %in% names(df)) {
    df$ecoact18 <- collapse_ecoact(map_missing(df$W5mainactYP))
  }
  if (grepl('wave_six', .y) && 'W6TCurrentAct' %in% names(df)) {
    df$ecoact19 <- collapse_ecoact(map_missing(df$W6TCurrentAct))
  }
  if (grepl('wave_seven', .y) && 'W7TCurrentAct' %in% names(df)) {
    df$ecoact20 <- collapse_ecoact(map_missing(df$W7TCurrentAct))
  }
  if (grepl('ns8', .y) && 'W8DACTIVITYC' %in% names(df)) {
    df$ecoact25 <- collapse_ecoact(map_missing(df$W8DACTIVITYC))
    df$ecoactadu25 <- map_missing(df$W8DACTIVITYC)
  }
  if (grepl('ns9', .y) && 'W9DACTIVITYC' %in% names(df)) {
    df$ecoact32 <- collapse_ecoact(map_missing(df$W9DACTIVITYC))
    df$ecoactadu32 <- map_missing(df$W9DACTIVITYC)
  }

  # Ensure NSID is character
  df$NSID <- as.character(df$NSID)

  return(df)
})

# Start with the first dataset and iteratively merge others
merged_data <- processed_data[[1]]
for (i in 2:length(processed_data)) {
  current_df <- processed_data[[i]]
  merged_data <- full_join(merged_data, current_df, by = 'NSID')
}

# Define ecoact labels
ecoact_labels <- c(
  `-9` = 'Refused',
  `-8` = 'Insufficient info',
  `-7` = 'Prefer not to say',
  `-3` = 'Not asked',
  `-2` = 'Schedule not applicable',
  `-1` = 'Not applicable',
  `1` = 'Education',
  `2` = 'Work',
  `3` = 'Unemployed',
  `4` = 'Training',
  `5` = 'Apprenticeship',
  `6` = 'Other'
)

# Apply labels to ecoact variables
ecoact_vars <- c('ecoact17', 'ecoact18', 'ecoact19', 'ecoact20', 'ecoact25', 'ecoact32')
for (var in ecoact_vars) {
  if (var %in% names(merged_data)) {
    merged_data[[var]] <- factor(merged_data[[var]],
                                levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6),
                                labels = ecoact_labels)
  } else {
    # Create missing variables with NA values
    merged_data[[var]] <- factor(rep(-3, nrow(merged_data)),
                                levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6),
                                labels = ecoact_labels)
  }
}

# Define detailed ecoact labels
detailed_ecoact_labels <- c(
  `-9` = 'Refused',
  `-8` = 'Insufficient info',
  `-7` = 'Prefer not to say',
  `-3` = 'Not asked',
  `-2` = 'Schedule not applicable',
  `-1` = 'Not applicable',
  `1` = 'Employee - in paid work',
  `2` = 'Self employed',
  `3` = 'In unpaid/voluntary work',
  `4` = 'Unemployed',
  `5` = 'Education: School/college/university',
  `6` = 'Apprenticeship',
  `7` = 'On government scheme for employment training',
  `8` = 'Sick or disabled',
  `9` = 'Looking after home or family',
  `10` = 'Something else'
)

# Apply labels to detailed ecoact variables
detailed_vars <- c('ecoactadu25', 'ecoactadu32')
for (var in detailed_vars) {
  if (var %in% names(merged_data)) {
    merged_data[[var]] <- factor(merged_data[[var]],
                                levels = c(-9, -8, -7, -3, -2, -1, 1:10),
                                labels = detailed_ecoact_labels)
  } else {
    # Create missing variables with NA values
    merged_data[[var]] <- factor(rep(-3, nrow(merged_data)),
                                levels = c(-9, -8, -7, -3, -2, -1, 1:10),
                                labels = detailed_ecoact_labels)
  }
}

# Write output file
write_csv(merged_data %>% select(NSID, all_of(c(ecoact_vars, detailed_vars))),
          'data/output/cleaned_data.csv')

# Print summary to verify variables were created
cat('Variables created:', names(merged_data)[names(merged_data) %in% c(ecoact_vars, detailed_vars)], '\n')

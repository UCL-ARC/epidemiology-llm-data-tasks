
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# Define mapping for collapsing NS-SEC categories
nssec_collapse_map <- setNames(
  c(
    # Employers and self-employed
    1, 1, 1,
    # Higher managerial and professional
    2, 2, 2, 2, 2, 2, 2, 2, 2,
    # Lower managerial and supervisory
    3, 3, 3,
    # Intermediate occupations
    4, 4, 4, 4,
    # Lower technical and semi-routine
    5, 5, 5, 5, 5, 5, 5, 5, 5,
    # Routine occupations
    6, 6, 6, 6, 6,
    # Never worked, long-term unemployed, full-time students
    7, 7, 7, 7, 7
  ),
  c(1, 8.1, 8.2, 2, 3.1, 3.2, 3.3, 3.4, 4.1, 4.2, 4.3, 4.4, 5, 6, 10,
    7.1, 7.2, 7.3, 7.4, 11.1, 11.2, 12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7,
    13.1, 13.2, 13.3, 13.4, 13.5, 14.1, 14.2, 15, 16, 17)
)

# Define labels for NS-SEC categories
nssec_labels <- c(
  'Employers and self-employed',
  'Higher managerial and professional',
  'Lower managerial and supervisory',
  'Intermediate occupations',
  'Lower technical and semi-routine',
  'Routine occupations',
  'Never worked, long-term unemployed, full-time students'
)

# Load datasets
file_paths <- c(
  'data/input/wave_one_lsype_young_person_2020.tab',
  'data/input/wave_four_lsype_young_person_2020.tab',
  'data/input/wave_five_lsype_young_person_2020.tab',
  'data/input/wave_six_lsype_young_person_2020.tab',
  'data/input/wave_seven_lsype_young_person_2020.tab',
  'data/input/ns8_2015_derived.tab'
)

# Load and merge datasets
loaded_datasets <- lapply(file_paths, function(path) {
  data <- read_delim(path, delim = '\t')
  if (nrow(data) == 0) {
    warning(paste('Skipping empty dataset:', path))
    return(NULL)
  }
  return(data)
})

# Remove NULL entries (empty datasets)
loaded_datasets <- compact(loaded_datasets)

# Start merging with the first dataset
merged_data <- loaded_datasets[[1]]

# Merge remaining datasets
for (i in 2:length(loaded_datasets)) {
  current_data <- loaded_datasets[[i]]
  if (!is.null(current_data)) {
    merged_data <- merged_data %>%
      full_join(current_data, by = 'NSID')
  }
}

# Process each NS-SEC variable individually
# Wave 4 (Age 17)
if ('W4nsseccatYP' %in% colnames(merged_data)) {
  merged_data$nssec17 <- ifelse(
    merged_data$W4nsseccatYP %in% names(nssec_collapse_map),
    nssec_collapse_map[as.character(merged_data$W4nsseccatYP)],
    -3
  )
} else {
  merged_data$nssec17 <- -3
}

# Wave 5 (Age 18)
if ('W5nsseccatYP' %in% colnames(merged_data)) {
  merged_data$nssec18 <- ifelse(
    merged_data$W5nsseccatYP %in% names(nssec_collapse_map),
    nssec_collapse_map[as.character(merged_data$W5nsseccatYP)],
    -3
  )
} else {
  merged_data$nssec18 <- -3
}

# Wave 6 (Age 19)
if ('w6nsseccatYP' %in% colnames(merged_data)) {
  merged_data$nssec19 <- ifelse(
    merged_data$w6nsseccatYP %in% names(nssec_collapse_map),
    nssec_collapse_map[as.character(merged_data$w6nsseccatYP)],
    -3
  )
} else {
  merged_data$nssec19 <- -3
}

# Wave 7 (Age 20)
if ('W7NSSECCat' %in% colnames(merged_data)) {
  merged_data$nssec20 <- ifelse(
    merged_data$W7NSSECCat %in% names(nssec_collapse_map),
    nssec_collapse_map[as.character(merged_data$W7NSSECCat)],
    -3
  )
} else {
  merged_data$nssec20 <- -3
}

# Wave 8 (Age 25)
if ('W8DNSSEC17' %in% colnames(merged_data)) {
  merged_data$nssec25 <- ifelse(
    merged_data$W8DNSSEC17 %in% names(nssec_collapse_map),
    nssec_collapse_map[as.character(merged_data$W8DNSSEC17)],
    -3
  )
} else {
  merged_data$nssec25 <- -3
}

# Apply labels to each NS-SEC variable
nssec_vars <- c('nssec17', 'nssec18', 'nssec19', 'nssec20', 'nssec25')
for (var in nssec_vars) {
  merged_data[[var]] <- factor(merged_data[[var]], levels = 1:7, labels = nssec_labels)
}

# Write cleaned data to CSV
write_csv(merged_data %>% select(NSID, all_of(nssec_vars)), 'data/output/cleaned_data.csv')

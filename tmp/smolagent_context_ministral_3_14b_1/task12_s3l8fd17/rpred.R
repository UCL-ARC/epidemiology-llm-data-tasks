
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(readr)

# Define file paths
file_paths <- list(
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  wave_five = 'data/input/wave_five_lsype_young_person_2020.tab',
  wave_six = 'data/input/wave_six_lsype_young_person_2020.tab',
  wave_seven = 'data/input/wave_seven_lsype_young_person_2020.tab',
  ns8_derived = 'data/input/ns8_2015_derived.tab'
)

# Function to read and prepare each file
prepare_file <- function(file_path, wave) {
  tryCatch({
    # Read file with all columns as character
    data <- read_delim(file_path, delim = '\t', col_types = 'c')

    # Convert NSID to character
    data$NSID <- as.character(data$NSID)

    # Convert specific columns based on wave
    if (wave == 'four') {
      if ('W4nsseccatYP' %in% colnames(data)) {
        data$W4nsseccatYP <- as.numeric(data$W4nsseccatYP)
      }
    } else if (wave == 'five') {
      if ('W5nsseccatYP' %in% colnames(data)) {
        data$W5nsseccatYP <- as.numeric(data$W5nsseccatYP)
      }
    } else if (wave == 'six') {
      if ('w6nsseccatYP' %in% colnames(data)) {
        data$w6nsseccatYP <- as.numeric(data$w6nsseccatYP)
      }
    } else if (wave == 'seven') {
      if ('W7NSSECCat' %in% colnames(data)) {
        data$W7NSSECCat <- as.numeric(data$W7NSSECCat)
      }
    } else if (wave == 'ns8') {
      if ('W8DNSSEC17' %in% colnames(data)) {
        data$W8DNSSEC17 <- as.numeric(data$W8DNSSEC17)
      }
      if ('W8DACTIVITYC' %in% colnames(data)) {
        data$W8DACTIVITYC <- as.numeric(data$W8DACTIVITYC)
      }
    }

    return(data)
  }, error = function(e) {
    message(paste("Error reading:", file_path, "-", e$message))
    return(NULL)
  })
}

# Load and prepare each dataset
wave_four_data <- prepare_file(file_paths$wave_four, 'four')
wave_five_data <- prepare_file(file_paths$wave_five, 'five')
wave_six_data <- prepare_file(file_paths$wave_six, 'six')
wave_seven_data <- prepare_file(file_paths$wave_seven, 'seven')
ns8_data <- prepare_file(file_paths$ns8_derived, 'ns8')

# Check if all datasets loaded successfully
if (any(sapply(list(wave_four_data, wave_five_data, wave_six_data,
                   wave_seven_data, ns8_data), is.null))) {
  stop("One or more files failed to load. Please check file paths and contents.")
}

# Merge datasets by NSID
merged_data <- full_join(wave_four_data, wave_five_data, by = 'NSID') %>%
  full_join(wave_six_data, by = 'NSID') %>%
  full_join(wave_seven_data, by = 'NSID') %>%
  full_join(ns8_data, by = 'NSID')

# Define NS-SEC mapping
nssec_mapping <- tibble(
  fractional_code = c(3.1, 3.2, 3.3, 3.4, 4.1, 4.2, 4.3, 4.4,
                      7.1, 7.2, 7.3, 7.4, 8.1, 8.2, 9.1, 9.2,
                      10:13.5),
  major_code = c(3, 3, 3, 3, 4, 4, 4, 4,
                 7, 7, 7, 7, 8, 8, 9, 9,
                 10:13)
)

# Define NS-SEC labels
nssec_labels <- c(
  'Employers in large organisations',
  'Higher managerial and administrative occupations',
  'Higher professional occupations',
  'Lower professional and higher technical occupations',
  'Lower managerial and administrative occupations',
  'Higher supervisory occupations',
  'Intermediate occupations',
  'Employers in small establishments',
  'Own account workers',
  'Lower supervisory occupations',
  'Lower technical occupations',
  'Semi-routine occupations',
  'Routine occupations',
  'Never worked and Long-term unemployed',
  'Full-time students',
  'Occupations not stated or inadequately described',
  'Not classifiable for other reasons'
)

# Function to collapse fractional codes to major categories
collapse_fractional <- function(x) {
  if (is.null(x) || length(x) == 0) return(x)
  ifelse(x %in% nssec_mapping$fractional_code,
         nssec_mapping$major_code[match(x, nssec_mapping$fractional_code)],
         x)
}

# Function to map missing values
map_missing <- function(x) {
  if (is.null(x) || length(x) == 0) return(x)
  x <- ifelse(is.na(x), -3, x)
  x <- ifelse(x %in% c(-999, -998, -997, -995), -2, x)
  x <- ifelse(x == -94, -8, x)
  x <- ifelse(x == -92, -9, x)
  x <- ifelse(x == -91, -1, x)
  x <- ifelse(x %in% c(-99, -97), -3, x)
  x
}

# Process each wave-specific NS-SEC variable
processed_data <- merged_data %>%
  mutate(
    nssec17 = if ('W4nsseccatYP' %in% colnames(.)) collapse_fractional(map_missing(W4nsseccatYP)) else NA,
    nssec18 = if ('W5nsseccatYP' %in% colnames(.)) collapse_fractional(map_missing(W5nsseccatYP)) else NA,
    nssec19 = if ('w6nsseccatYP' %in% colnames(.)) collapse_fractional(map_missing(w6nsseccatYP)) else NA,
    nssec20 = if ('W7NSSECCat' %in% colnames(.)) collapse_fractional(map_missing(W7NSSECCat)) else NA,
    nssec25 = if ('W8DNSSEC17' %in% colnames(.)) {
      ifelse(W8DACTIVITYC == 5, 15, collapse_fractional(map_missing(W8DNSSEC17)))
    } else NA
  ) %>%
  mutate(across(c(nssec17, nssec18, nssec19, nssec20, nssec25),
                ~ ifelse(is.na(.), NA_integer_, .))) %>%
  mutate(
    nssec17 = factor(nssec17, levels = 1:17, labels = nssec_labels),
    nssec18 = factor(nssec18, levels = 1:17, labels = nssec_labels),
    nssec19 = factor(nssec19, levels = 1:17, labels = nssec_labels),
    nssec20 = factor(nssec20, levels = 1:17, labels = nssec_labels),
    nssec25 = factor(nssec25, levels = 1:17, labels = nssec_labels)
  ) %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25) %>%
  mutate(nssec32 = NA_character_) %>%
  mutate(nssec32 = factor(nssec32, levels = 1:17, labels = nssec_labels))

# Create output directory and write CSV
tryCatch({
  dir.create('data/output', showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(processed_data, 'data/output/cleaned_data.csv')
  message("Successfully created cleaned_data.csv")
}, error = function(e) {
  message("Error writing output file: ", e$message)
  stop("Failed to create output file")
})

message("Processing completed successfully!")

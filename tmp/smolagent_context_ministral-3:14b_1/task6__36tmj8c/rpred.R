
# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load and merge datasets
files <- list(
  wave_one = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_two = 'data/input/wave_two_lsype_family_background_2020.tab',
  wave_three = 'data/input/wave_three_lsype_family_background_2020.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  ns8_derived = 'data/input/ns8_2015_derived.tab',
  ns9_derived = 'data/input/ns9_2022_derived_variables.tab',
  ns9_main = 'data/input/ns9_2022_main_interview.tab'
)

# Load each dataset
load_dataset <- function(file) {
  read_delim(file, delim = '\t')
}

# Load datasets one by one
datasets <- list()
for (file in names(files)) {
  datasets[[file]] <- load_dataset(files[[file]])
  message(paste('Successfully loaded:', file))
}

# Rename NSID columns
for (name in names(datasets)) {
  datasets[[name]] <- datasets[[name]] %>% rename(NSID = 1)
}

# Merge datasets by NSID
merged_data <- reduce(datasets, full_join, by = 'NSID')

# Function to harmonize missing values
harmonize_missing_values <- function(x) {
  if (is.numeric(x)) {
    x <- as.numeric(x)
    x[x < -1] <- -3
    x[is.na(x)] <- -3
    x
  } else {
    x
  }
}

# Get names of numeric columns
numeric_cols <- names(merged_data)[sapply(merged_data, is.numeric)]

# Apply missing value harmonization to all numeric columns
if (length(numeric_cols) > 0) {
  merged_data[numeric_cols] <- lapply(merged_data[numeric_cols], harmonize_missing_values)
}

# Function to create labeled factors
create_labeled_factor <- function(x, values, labels) {
  if (is.numeric(x)) {
    x <- as.numeric(x)
    x <- factor(x, levels = values, labels = labels)
  } else {
    x
  }
  return(x)
}

# Process urbind (Urban/Rural Indicator)
urbind_values <- c(-94, 1, 2, 3, 4, 5, 6, 7, 8)
urbind_labels <- c(
  "Insufficient information", "Urban >= 10k - sparse", "Town & Fringe - sparse",
  "Village - sparse", "Hamlet and Isolated Dwelling - sparse",
  "Urban >= 10k - less sparse", "Town & Fringe - less sparse",
  "Village - less sparse", "Hamlet & Isolated Dwelling"
)

if ('urbind' %in% names(merged_data)) {
  merged_data$urbind15 <- ifelse(!is.na(merged_data$urbind),
                                create_labeled_factor(merged_data$urbind, urbind_values, urbind_labels),
                                NA)
  merged_data$urbind16 <- merged_data$urbind15
  merged_data <- merged_data %>% select(-urbind)
}

# Process gor (Government Office Region)
gor_values <- c(-94, -9, -8, -3, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
gor_labels <- c(
  "Insufficient information", "Refused", "Don't know", "Not asked",
  "Not applicable", "North East", "North West", "Yorkshire and the Humber",
  "East Midlands", "West Midlands", "East of England", "London",
  "South East", "South West", "Wales", "Scotland", "Northern Ireland",
  "Unknown due to faulty/missing postcode"
)

if ('gor' %in% names(merged_data)) {
  merged_data$gor15 <- ifelse(!is.na(merged_data$gor),
                              create_labeled_factor(merged_data$gor, gor_values, gor_labels),
                              NA)
  merged_data$gor16 <- merged_data$gor15
  merged_data <- merged_data %>% select(-gor)
}

# Process W8DGOR if it exists
if ('W8DGOR' %in% names(merged_data)) {
  merged_data$W8DGOR <- create_labeled_factor(merged_data$W8DGOR, gor_values, gor_labels)
}

# Process W9DRGN if it exists
if ('W9DRGN' %in% names(merged_data)) {
  merged_data$W9DRGN <- create_labeled_factor(merged_data$W9DRGN, gor_values, gor_labels)
}

# Process W9NATIONRES (Nation of UK)
nationres_values <- c(-9, -8, -3, -1, 1, 2, 3, 4, 5)
nationres_labels <- c(
  "Refused", "Don't know", "Not asked at fieldwork stage", "Not applicable",
  "England", "Scotland", "Wales", "Northern Ireland", "Outside of UK or unknown"
)

if ('W9NATIONRES' %in% names(merged_data)) {
  merged_data$nationres <- ifelse(!is.na(merged_data$W9NATIONRES),
                                  create_labeled_factor(merged_data$W9NATIONRES, nationres_values, nationres_labels),
                                  NA)
  merged_data <- merged_data %>% select(-W9NATIONRES)
}

# Derived categorical variables: create a binary UK/abroad indicator
if ('nationres' %in% names(merged_data)) {
  merged_data$uk_abroad <- ifelse(merged_data$nationres %in% c(1, 2, 3, 4), "UK", "Abroad")
  merged_data$uk_abroad <- factor(merged_data$uk_abroad, levels = c("UK", "Abroad"))
}

# Output the cleaned data
write_csv(merged_data, 'data/output/cleaned_data.csv')

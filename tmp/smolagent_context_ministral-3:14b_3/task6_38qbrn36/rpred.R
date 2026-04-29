
# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
file_paths <- list(
  wave_one = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_two = 'data/input/wave_two_lsype_family_background_2020.tab',
  wave_three = 'data/input/wave_three_lsype_family_background_2020.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  ns8_derived = 'data/input/ns8_2015_derived.tab',
  ns9_derived = 'data/input/ns9_2022_derived_variables.tab',
  ns9_main = 'data/input/ns9_2022_main_interview.tab'
)

# Load datasets
load_dataset <- function(file_path) {
  read_delim(file_path, delim = '\t', col_types = 'c')
}

datasets <- map(file_paths, load_dataset)

# Name datasets for clarity
wave_one <- datasets[[1]]
wave_two <- datasets[[2]]
wave_three <- datasets[[3]]
wave_four <- datasets[[4]]
ns8_derived <- datasets[[5]]
ns9_derived <- datasets[[6]]
ns9_main <- datasets[[7]]

# Function to standardize missing values
standardize_missing_values <- function(x) {
  if (is.null(x)) return(NULL)
  x <- as.numeric(x)
  x[x == -999 | x == -998 | x == -997 | x == -995 | x == -94 | x == -92 | x == -91 | x == -99] <- -3
  x[x == -97] <- -3
  x[x == -100] <- -3
  x[x == -9] <- -9
  x[x == -8] <- -8
  x[x == -1] <- -1
  x[x == -7] <- -7
  x[x == -2] <- -2
  x[is.na(x)] <- -3
  return(x)
}

# Start with the largest dataset and join others with unique column names
merged_data <- wave_one %>%
  left_join(wave_two %>%
              mutate(urbind = ifelse(is.na(urbind), -3, urbind),
                     gor = ifelse(is.na(gor), -3, gor)) %>%
              rename(urbind15 = urbind, gor15 = gor) %>%
              select(NSID, urbind15, gor15),
            by = 'NSID') %>%
  left_join(wave_three %>%
              mutate(urbind = ifelse(is.na(urbind), -3, urbind),
                     gor = ifelse(is.na(gor), -3, gor)) %>%
              rename(urbind16 = urbind, gor16 = gor) %>%
              select(NSID, urbind16, gor16),
            by = 'NSID') %>%
  left_join(wave_four %>% select(NSID), by = 'NSID') %>%
  left_join(ns8_derived %>% select(NSID, W8DGOR), by = 'NSID') %>%
  left_join(ns9_derived %>% select(NSID, W9DRGN), by = 'NSID') %>%
  left_join(ns9_main %>% select(NSID, W9NATIONRES), by = 'NSID')

# Initialize new columns with -3 (Not asked at fieldwork stage)
for (var in c('regub15', 'regov15', 'regub16', 'regov16', 'regub17', 'regov17', 'regov8', 'regov9', 'nationres9')) {
  merged_data[[var]] <- -3
}

# Process urbind and gor variables if they exist
if ('urbind15' %in% names(merged_data)) {
  merged_data$regub15 <- standardize_missing_values(merged_data$urbind15)
}
if ('gor15' %in% names(merged_data)) {
  merged_data$regov15 <- standardize_missing_values(merged_data$gor15)
}

if ('urbind16' %in% names(merged_data)) {
  merged_data$regub16 <- standardize_missing_values(merged_data$urbind16)
}
if ('gor16' %in% names(merged_data)) {
  merged_data$regov16 <- standardize_missing_values(merged_data$gor16)
}

# Standardize missing values for W8DGOR and W9DRGN
if ('W8DGOR' %in% names(merged_data)) {
  merged_data$regov8 <- standardize_missing_values(merged_data$W8DGOR)
}
if ('W9DRGN' %in% names(merged_data)) {
  merged_data$regov9 <- standardize_missing_values(merged_data$W9DRGN)
}

# Standardize missing values for nation of residence
if ('W9NATIONRES' %in% names(merged_data)) {
  merged_data$nationres9 <- standardize_missing_values(merged_data$W9NATIONRES)
}

# Create derived variables for region harmonization
merged_data$regov_england <- case_when(
  merged_data$regov15 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ merged_data$regov15,
  merged_data$regov16 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ merged_data$regov16,
  merged_data$regov8 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ merged_data$regov8,
  TRUE ~ -3
)

# Create labeled factors for region variables
region_labels <- c(
  '-9' = 'Refused',
  '-8' = 'Insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at fieldwork stage',
  '-2' = 'Schedule not applicable',
  '-1' = 'Not applicable',
  '1' = 'North East',
  '2' = 'North West',
  '3' = 'Yorkshire and the Humber',
  '4' = 'East Midlands',
  '5' = 'West Midlands',
  '6' = 'East of England',
  '7' = 'London',
  '8' = 'South East',
  '9' = 'South West',
  '10' = 'Wales',
  '11' = 'Scotland',
  '12' = 'Northern Ireland',
  '13' = 'Unknown due to faulty/missing postcode'
)

for (var in c('regov15', 'regov16', 'regov8', 'regov9', 'regov_england')) {
  if (var %in% names(merged_data)) {
    merged_data[[var]] <- factor(merged_data[[var]], levels = names(region_labels), labels = region_labels)
  }
}

# Urban/rural labels
urban_labels <- c(
  '-9' = 'Refused',
  '-8' = 'Insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at fieldwork stage',
  '-2' = 'Schedule not applicable',
  '-1' = 'Not applicable',
  '1' = 'Urban >= 10k - sparse',
  '2' = 'Town & Fringe - sparse',
  '3' = 'Village - sparse',
  '4' = 'Hamlet and Isolated Dwelling - sparse',
  '5' = 'Urban >= 10k - less sparse',
  '6' = 'Town & Fringe - less sparse',
  '7' = 'Village - less sparse',
  '8' = 'Hamlet & Isolated Dwelling'
)

for (var in c('regub15', 'regub16')) {
  if (var %in% names(merged_data)) {
    merged_data[[var]] <- factor(merged_data[[var]], levels = names(urban_labels), labels = urban_labels)
  }
}

# Nation of residence labels
nation_labels <- c(
  '-9' = 'Refused',
  '-8' = "Don't know",
  '-3' = 'Not asked at fieldwork stage',
  '-1' = 'Not applicable',
  '1' = 'England',
  '2' = 'Scotland',
  '3' = 'Wales',
  '4' = 'Northern Ireland',
  '5' = 'Outside of UK or unknown'
)

if ('nationres9' %in% names(merged_data)) {
  merged_data$nationres9 <- factor(merged_data$nationres9, levels = names(nation_labels), labels = nation_labels)
}

# Select only the ID and derived variables for output
output_vars <- c('NSID')
for (var in c('regub15', 'regov15', 'regub16', 'regov16', 'regov8', 'regov9', 'regov_england', 'nationres9')) {
  if (var %in% names(merged_data)) {
    output_vars <- c(output_vars, var)
  }
}

cleaned_data <- merged_data %>% select(all_of(output_vars))

# Write output to file
write_csv(cleaned_data, 'data/output/cleaned_data.csv')

# Print confirmation
cat('Data cleaning and preprocessing completed. Output saved to data/output/cleaned_data.csv\n')

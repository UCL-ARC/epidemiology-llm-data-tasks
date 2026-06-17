
# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

# Define file paths and target variables
files <- list(
  wave_two = 'data/input/wave_two_lsype_family_background_2020.tab',
  wave_three = 'data/input/wave_three_lsype_family_background_2020.tab',
  wave_nine = 'data/input/ns9_2022_derived_variables.tab'
)

# Load all files into separate data frames
load_files <- function(path) {
  read_delim(path, delim = '\t')
}

data_frames <- purrr::map(files, load_files)
names(data_frames) <- c('wave2', 'wave3', 'wave9')

# Merge datasets by NSID
merged_data <- data_frames$wave2 %>%
  full_join(data_frames$wave3, by = 'NSID', suffix = c('_15', '_16')) %>%
  full_join(data_frames$wave9, by = 'NSID')

# Create target variables for IMD at ages 15, 16, and 32
# Wave 2 (Age 15): IMDRSCORE from wave_two_lsype_family_background_2020.tab
# Wave 3 (Age 16): IMDRSCORE from wave_three_lsype_family_background_2020.tab

# Create imd15 variable (Age 15)
merged_data <- merged_data %>%
  mutate(imd15 = IMDRSCORE_15) %>%
  mutate(imd15 = case_when(
    imd15 == -999 | is.na(imd15) ~ -3,  # Convert missing values to -3
    imd15 == -94 ~ -8,                  # Map 'Insufficient Information' to -8
    TRUE ~ imd15                        # Keep original value otherwise
  ))

# Create imd16 variable (Age 16)
merged_data <- merged_data %>%
  mutate(imd16 = IMDRSCORE_16) %>%
  mutate(imd16 = case_when(
    imd16 == -999 | is.na(imd16) ~ -3,  # Convert missing values to -3
    imd16 == -94 ~ -8,                  # Map 'Insufficient Information' to -8
    TRUE ~ imd16                        # Keep original value otherwise
  ))

# Create imd32 variable (Age 32) as NA since no IMD score is available
merged_data <- merged_data %>%
  mutate(imd32 = NA_real_)

# Select only NSID, imd15, imd16, and imd32
final_data <- merged_data %>%
  select(NSID, imd15, imd16, imd32)

# Write the cleaned data to CSV
readr::write_csv(final_data, 'data/output/cleaned_data.csv')

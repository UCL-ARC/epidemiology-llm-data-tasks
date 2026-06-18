
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths and load datasets
files <- list(
  wave_one = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  wave_eight = 'data/input/ns8_2015_derived.tab',
  wave_nine = 'data/input/ns9_2022_derived_variables.tab'
)

# Load each file into a separate data frame
load_data <- function(file_path) {
  read_delim(file_path, delim = '\t')
}

loaded_data <- map(files, load_data)

# Merge datasets by NSID
merged_data <- loaded_data[[1]] %>%
  full_join(loaded_data[[2]], by = 'NSID') %>%
  full_join(loaded_data[[3]], by = 'NSID') %>%
  full_join(loaded_data[[4]], by = 'NSID')

# Extract and rename BMI variables
bmi_data <- merged_data %>%
  select(NSID, W8DBMI, W9DBMI) %>%
  rename(bmi25 = W8DBMI, bmi32 = W9DBMI)

# Standardize missing values according to metadata
standardize_missing <- function(x) {
  x %>%
    mutate(
      across(
        starts_with('bmi'),
        ~ case_when(
          . %in% c(-9, -8, -1) ~ .,  # Preserve existing missing codes
          is.na(.) ~ -3,            # Convert R NA to -3
          TRUE ~ .                  # Keep valid values
        )
      )
    )
}

cleaned_bmi_data <- bmi_data %>%
  standardize_missing()

# Verify the structure of the cleaned data
print(head(cleaned_bmi_data))
print(summary(cleaned_bmi_data))

# Write the cleaned data to CSV
write_csv(cleaned_bmi_data, 'data/output/cleaned_data.csv')

# Confirm the output file exists and contains the correct variables
cat('Output file written to: data/output/cleaned_data.csv\n')
cat('Variables in output:')
print(names(cleaned_bmi_data))

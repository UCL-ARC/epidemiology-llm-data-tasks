library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- list(
  'wave_one_lsype_young_person_2020.tab' = 'data/input/wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab' = 'data/input/wave_four_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab' = 'data/input/ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab' = 'data/input/ns9_2022_derived_variables.tab'
)

# Load all files
loaded_files <- map(files, ~ read_delim(.x, delim = "\t", show_col_types = FALSE))
names(loaded_files) <- names(files)

# Print file info for debugging
for (nm in names(loaded_files)) {
  cat(sprintf("File: %s, rows: %d, cols: %d\n", nm, nrow(loaded_files[[nm]]), ncol(loaded_files[[nm]])))
  cat(sprintf("  Variables: %s\n", paste(names(loaded_files[[nm]]), collapse = ", ")))
}

# Merge all files by NSID
cleaned <- reduce(loaded_files, full_join, by = 'NSID')

# Create bmi25 from W8DBMI (Wave 8, Age 25)
# W8DBMI has user missing values: -9 (Refused), -8 (Insufficient information), -1 (Not applicable)
# These map to standard codes: -9, -8, -1
# BMI is continuous but has no valid negative values, so we use standard codes
cleaned <- cleaned %>%
  mutate(
    bmi25 = W8DBMI
  )

# Create bmi32 from W9DBMI (Wave 9, Age 32)
# W9DBMI has user missing values: -9 (Refused), -8 (Insufficient information), -1 (Not applicable)
cleaned <- cleaned %>%
  mutate(
    bmi32 = W9DBMI
  )

# Check for any other missing values that need to be handled
# Convert NA to -3 (Not asked / not interviewed)
cleaned <- cleaned %>%
  mutate(
    bmi25 = ifelse(is.na(bmi25), -3, bmi25),
    bmi32 = ifelse(is.na(bmi32), -3, bmi32)
  )

# Convert to numeric to ensure proper storage
cleaned$bmi25 <- as.numeric(cleaned$bmi25)
cleaned$bmi32 <- as.numeric(cleaned$bmi32)

# Keep only NSID and the BMI variables
output <- cleaned %>%
  select(NSID, bmi25, bmi32)

# Create output directory if needed
if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Write output
write_csv(output, 'data/output/cleaned_data.csv')

cat('Output written successfully.\n')
cat(sprintf('Rows: %d, Cols: %d\n', nrow(output), ncol(output)))
cat('Sample:\n')
print(head(output, 10))

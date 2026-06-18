library(readr)
library(dplyr)
library(tidyr)

# Load all files from the metadata
# Wave 1 (Age 14) - only contains NSID
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')

# Wave 4 (Age 17) - only contains NSID
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')

# Wave 8 (Age 25) - contains W8DBMI
wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')

# Wave 9 (Age 32) - contains W9DBMI
wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge all files by NSID using full_join
cleaned_data <- full_join(wave1, wave4, by = 'NSID')
cleaned_data <- full_join(cleaned_data, wave8, by = 'NSID')
cleaned_data <- full_join(cleaned_data, wave9, by = 'NSID')

# Function to handle missing values for BMI variables
# Based on metadata:
# -9.0: Refused -> -9
# -8.0: Insufficient information -> -8
# -1.0: Not applicable -> -1
# All other NAs (including those from missing waves) -> -3
process_bmi <- function(x) {
  result <- x
  
  # Map negative values based on metadata labels
  result[x == -9] <- -9
  result[x == -8] <- -8
  result[x == -1] <- -1
  
  # Convert remaining NAs to -3 (not asked at fieldwork stage)
  result[is.na(result)] <- -3
  
  return(result)
}

# Process BMI variables from the merged dataframe
# bmi25 from W8DBMI (Wave 8, Age 25)
cleaned_data$bmi25 <- process_bmi(cleaned_data$W8DBMI)

# bmi32 from W9DBMI (Wave 9, Age 32)
cleaned_data$bmi32 <- process_bmi(cleaned_data$W9DBMI)

# Select only ID and final derived variables
final_data <- cleaned_data %>%
  select(NSID, bmi25, bmi32)

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

cat('Successfully created cleaned_data.csv\n')
cat('Number of observations:', nrow(final_data), '\n')
cat('Variables:', paste(names(final_data), collapse = ', '), '\n')

# Check for negative values in BMI variables (should all be mapped to appropriate codes)
cat('\nSummary of bmi25 (age 25):\n')
print(table(final_data$bmi25, useNA = 'ifany'))

cat('\nSummary of bmi32 (age 32):\n')
print(table(final_data$bmi32, useNA = 'ifany'))

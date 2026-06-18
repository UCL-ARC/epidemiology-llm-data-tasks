# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Suppress warnings for cleaner output
options(warn = -1)

# Load all source files from data/input/
# Wave 1 (Age 14) - ID only file
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = "c"))

# Wave 4 (Age 17) - ID only file
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = "c"))

# Wave 8 (Age 25) - Derived variables including BMI
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")

# Wave 9 (Age 32) - Derived variables including BMI
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets using full_join by NSID (cohort member identifier)
all_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Create bmi25 from W8DBMI (Wave 8 = Age 25)
# W8DBMI has user_missing_values: '-9.0 thru -8.0 and -1.0'
# -9 = Refused, -8 = Insufficient information, -1 = Not applicable
# Map NA to -3 (not asked), preserve other codes
all_data$bmi25 <- case_when(
  is.na(as.numeric(all_data$W8DBMI)) ~ -3,
  all_data$W8DBMI == -9 ~ -9,
  all_data$W8DBMI == -8 ~ -8,
  all_data$W8DBMI == -1 ~ -1,
  TRUE ~ as.numeric(all_data$W8DBMI)
)

# Create bmi32 from W9DBMI (Wave 9 = Age 32)
# W9DBMI has user_missing_values: '-1.0 thru -8.0 and -9.0'
# -9 = Refused, -8 = Insufficient information, -1 = Not applicable
# Map NA to -3 (not asked), preserve other codes
all_data$bmi32 <- case_when(
  is.na(as.numeric(all_data$W9DBMI)) ~ -3,
  all_data$W9DBMI == -9 ~ -9,
  all_data$W9DBMI == -8 ~ -8,
  all_data$W9DBMI == -1 ~ -1,
  TRUE ~ as.numeric(all_data$W9DBMI)
)

# Remove raw source variables, keep only NSID and derived BMI variables
all_data <- all_data %>%
  select(NSID, bmi25, bmi32)

# Write output to CSV
write_csv(all_data, "data/output/cleaned_data.csv")

# Print confirmation message
cat("Successfully created cleaned_data.csv with", nrow(all_data), "rows and", ncol(all_data), "columns\n")
cat("Variables:", paste(names(all_data), collapse = ", "), "\n")

print(str(all_data))
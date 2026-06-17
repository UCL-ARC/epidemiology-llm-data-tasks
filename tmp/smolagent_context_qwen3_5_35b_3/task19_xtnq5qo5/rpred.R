library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define missing value mapping function
map_missing_values <- function(x) {
  # Map negative values to standard missing codes based on metadata labels
  x <- case_when(
    x == -9 ~ -9,      # Refusal
    x == -8 ~ -8,      # Insufficient information
    x == -7 ~ -7,      # Prefer not to say
    x == -3 ~ -3,      # Not asked
    x == -2 ~ -2,      # Not applicable
    x == -1 ~ -1,      # Not applicable
    x < 0 ~ -9,        # Other negative values treated as refusal
    TRUE ~ x
  )
  # Convert remaining NA to -3 (not asked)
  x[is.na(x)] <- -3
  return(x)
}

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", 
                     delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", 
                    delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", 
                    delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", 
                    delim = "\t")

# Convert NSID to character for all dataframes to ensure consistent join type
wave1$NSID <- as.character(wave1$NSID)
wave4$NSID <- as.character(wave4$NSID)
wave8$NSID <- as.character(wave8$NSID)
wave9$NSID <- as.character(wave9$NSID)

# Merge all datasets by NSID
cleaned_data <- full_join(wave1, wave4, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave8, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave9, by = "NSID")

# Create BMI variables with NSID from source files, then map missing values
bmi25_df <- wave8 %>%
  select(NSID, W8DBMI) %>%
  mutate(bmi25 = map_missing_values(W8DBMI))

bmi32_df <- wave9 %>%
  select(NSID, W9DBMI) %>%
  mutate(bmi32 = map_missing_values(W9DBMI))

# Join BMI variables to the main dataframe
cleaned_data <- cleaned_data %>%
  left_join(bmi25_df, by = "NSID") %>%
  left_join(bmi32_df, by = "NSID")

# Write output
write_csv(cleaned_data, "data/output/cleaned_data.csv")

cat("Output written successfully.\n")
cat("Summary of output data:\n")
print(summary(cleaned_data))
cat("\nNumber of rows:", nrow(cleaned_data), "\n")
cat("Number of columns:", ncol(cleaned_data), "\n")

# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define mappings for wave-specific missing values to standard codes
missing_value_mappings <- list(
  wave_one = setNames(c(-3, -3, -3, -8), c(-999, -99, -98, -94)),
  wave_four = setNames(c(-3, -3, -3, -8, -9), c(-999, -996, -99, -98, -92))
)

# Define function to recode missing values
recode_missing_values <- function(x, wave) {
  mapping <- missing_value_mappings[[wave]]
  for (code in names(mapping)) {
    x[x == as.numeric(code)] <- mapping[[code]]
  }
  return(x)
}

# Define function to harmonize employment status
harmonize_employment_status <- function(x) {
  # First, ensure all values are numeric
  x <- as.numeric(x)
  
  # Define labels for valid categories
  valid_labels <- c(
    `1` = "paid_work_30plus", 
    `2` = "paid_work_less30", 
    `3` = "unemployed", 
    `4` = "training", 
    `5` = "education", 
    `6` = "home", 
    `7` = "retired", 
    `8` = "sick_disabled", 
    `9` = "other"
  )
  
  # Define labels for missing values
  missing_labels <- c(
    `-9` = "Refusal", 
    `-8` = "Don't know/insufficient information", 
    `-3` = "Not asked", 
    `-2` = "Schedule error", 
    `-1` = "Not applicable", 
    `-7` = "Prefer not to say"
  )
  
  # Combine all possible labels
  all_labels <- c(valid_labels, missing_labels)
  
  # Create factor with all possible levels
  factor(x, levels = names(all_labels), labels = all_labels)
}

# Load and merge datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets
merged_data <- full_join(
  full_join(wave_one, wave_two, by = "NSID"), 
  full_join(wave_three, wave_four, by = "NSID"), 
  by = "NSID"
)

# Process mother's employment status
merged_data <- merged_data %>%
  mutate(
    W1empsmum = recode_missing_values(W1empsmum, "wave_one"),
    W2empsmum = recode_missing_values(W2empsmum, "wave_one"),
    W3empsmum = recode_missing_values(W3empsmum, "wave_one"),
    w4empsmum = recode_missing_values(w4empsmum, "wave_four"),
    ecoactmum14 = harmonize_employment_status(W1empsmum),
    ecoactmum15 = harmonize_employment_status(W2empsmum),
    ecoactmum16 = harmonize_employment_status(W3empsmum),
    ecoactmum17 = harmonize_employment_status(w4empsmum)
  )

# Process father's employment status
merged_data <- merged_data %>%
  mutate(
    W1empsdad = recode_missing_values(W1empsdad, "wave_one"),
    W2empsdad = recode_missing_values(W2empsdad, "wave_one"),
    W3empsdad = recode_missing_values(W3empsdad, "wave_one"),
    w4empsdad = recode_missing_values(w4empsdad, "wave_four"),
    ecoactdad14 = harmonize_employment_status(W1empsdad),
    ecoactdad15 = harmonize_employment_status(W2empsdad),
    ecoactdad16 = harmonize_employment_status(W3empsdad),
    ecoactdad17 = harmonize_employment_status(w4empsdad)
  )

# Convert all remaining missing values to -3
merged_data[is.null(merged_data)] <- -3

# Select only the ID and derived variables
cleaned_data <- merged_data %>%
  select(NSID, 
         ecoactmum14, ecoactmum15, ecoactmum16, ecoactmum17, 
         ecoactdad14, ecoactdad15, ecoactdad16, ecoactdad17)

# Write output to CSV
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)

# Print confirmation
cat("Data cleaning and preprocessing completed. Output saved to data/output/cleaned_data.csv\n")
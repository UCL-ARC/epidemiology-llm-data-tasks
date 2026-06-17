# Load required libraries
library(haven)
library(dplyr)
library(readr)

# Load all files
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave_nine <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- full_join(wave_one, wave_two, by = "NSID")
merged_data <- full_join(merged_data, wave_four, by = "NSID")
merged_data <- full_join(merged_data, wave_eight, by = "NSID")
merged_data <- full_join(merged_data, wave_nine, by = "NSID")

# Define missing value mapping function
map_missing_values <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x == -999] <- -2
  x[x == -998] <- -2
  x[x == -997] <- -2
  x[x == -995] <- -2
  x[x == -99] <- -3
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  return(x)
}

# Create a list of ethnicity variables and their corresponding ages
ethnicity_vars <- list(
  W1ethnic2YP = 14,
  W2ethnicYP = 15,
  w4ethnic2YP = 17,
  W8DETHN15 = 25,
  W9DETHN15 = 32
)

# Map missing values for each ethnicity variable that exists in the data
for (var in names(ethnicity_vars)) {
  if (var %in% names(merged_data)) {
    merged_data[[var]] <- map_missing_values(merged_data[[var]])
  }
}

# Sort ethnicity variables by age (oldest first)
sorted_vars <- sort(names(ethnicity_vars), decreasing = TRUE)

# Initialize eth column
merged_data$eth <- -3

# Fill valid values using earliest valid response
for (var in sorted_vars) {
  if (var %in% names(merged_data)) {
    valid_mask <- !is.na(merged_data[[var]]) & 
                  merged_data[[var]] >= -9 & 
                  merged_data[[var]] <= 16
    update_mask <- merged_data$eth == -3 & valid_mask
    merged_data$eth[update_mask] <- merged_data[[var]][update_mask]
  }
}

# Remove intermediate variables
intermediate_vars <- grep("ethnic|ETHN|DETHN|W1|W2|w4|W8|W9", names(merged_data), ignore.case = TRUE, value = TRUE)
intermediate_vars <- intermediate_vars[!grepl("eth", intermediate_vars, ignore.case = FALSE)]
merged_data <- merged_data %>% select(-any_of(intermediate_vars))

# Write the final output
readr::write_csv(merged_data, "data/output/cleaned_data.csv")

# Output success message
cat("Successfully created cleaned_data.csv with consolidated ethnicity variable eth\n")
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
input_path <- "data/input/"
output_path <- "data/output/"

# Load metadata for variable mappings
metadata <- list(
  wave_four = list(
    var = "W4empsYP",
    mapping = c("-999.0" = "-2", "-94.0" = "-8", "-92.0" = "-9", "-91.0" = "-1",
               "1.0" = "1", "2.0" = "1", "3.0" = "4", "4.0" = "2",
               "5.0" = "3", "6.0" = "5", "7.0" = "6", "8.0" = "6", "9.0" = "6")
  ),
  wave_five = list(
    var = "W5mainactYP",
    mapping = c("-94.0" = "-8", "1.0" = "2", "2.0" = "1", "3.0" = "1",
               "4.0" = "3", "5.0" = "2", "6.0" = "2", "7.0" = "4",
               "8.0" = "5", "9.0" = "6", "10.0" = "6", "11.0" = "6")
  ),
  wave_six = list(
    var = "W6TCurrentAct",
    mapping = c("-91.0" = "-1", "1.0" = "3", "2.0" = "3", "3.0" = "1",
               "4.0" = "2", "5.0" = "2", "6.0" = "6", "7.0" = "5",
               "8.0" = "4", "9.0" = "6", "10.0" = "1", "11.0" = "6")
  ),
  wave_seven = list(
    var = "W7TCurrentAct",
    mapping = c("-91.0" = "-1", "1.0" = "3", "2.0" = "3", "3.0" = "1",
               "4.0" = "2", "5.0" = "2", "6.0" = "6", "7.0" = "5",
               "8.0" = "4", "9.0" = "1", "10.0" = "6", "11.0" = "6",
               "12.0" = "6", "13.0" = "6", "14.0" = "6", "15.0" = "6")
  ),
  wave_eight = list(
    var = "W8DACTIVITYC",
    mapping = c("-9.0" = "-9", "-8.0" = "-8", "-1.0" = "-1", "1.0" = "1",
               "2.0" = "1", "3.0" = "6", "4.0" = "4", "5.0" = "3",
               "6.0" = "2", "7.0" = "2", "8.0" = "6", "9.0" = "5", "10.0" = "6")
  ),
  wave_nine = list(
    var = "W9DACTIVITYC",
    mapping = c("-9.0" = "-9", "-8.0" = "-8", "-1.0" = "-1", "1.0" = "1",
               "2.0" = "1", "3.0" = "6", "4.0" = "4", "5.0" = "3",
               "6.0" = "2", "7.0" = "2", "8.0" = "6", "9.0" = "5", "10.0" = "6")
  )
)

# Function to load files safely
load_file_safely <- function(file) {
  tryCatch({
    read_delim(paste0(input_path, file), delim = "\t", col_types = cols(NSID = col_character()))
  }, error = function(e) {
    message("Error loading file '", file, "': ", e$message, "\n")
    return(NULL)
  })
}

# Load all relevant files
files_to_load <- c(
  "wave_four_lsype_young_person_2020.tab",
  "wave_five_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_derived_variables.tab"
)

# Load all datasets
loaded_datasets <- map(files_to_load, load_file_safely)

# Filter out any NULL datasets (failed to load)
loaded_datasets <- compact(loaded_datasets)

# Check if all datasets were loaded
if (length(loaded_datasets) < length(files_to_load)) {
  message("Warning: Some files failed to load. Proceeding with available data.")
}

# Merge datasets by NSID
merged_data <- reduce(loaded_datasets, full_join, by = "NSID")

# Function to harmonize variables
harmonize_variable <- function(data, wave, metadata) {
  var <- metadata$var
  mapping <- metadata$mapping

  if (!var %in% names(data)) {
    message("Variable '", var, "' not found in dataset. Skipping.")
    return(data)
  }

  data <- data %>%
    mutate(
      !!paste0("ecoact", wave) := 
        recode(
          !!sym(var),
          !!!mapping,
          .default = "-3"
        )
    )

  # Convert to factor with labels
  data <- data %>%
    mutate(
      !!paste0("ecoact", wave) := factor(
        !!paste0("ecoact", wave),
        levels = c("-9", "-8", "-7", "-3", "-2", "-1", "1", "2", "3", "4", "5", "6"),
        labels = c(
          "Refusal", "Insufficient information", "Prefer not to say", "Not asked",
          "Schedule not applicable", "Item not applicable", "In paid work", "Apprenticeship / training",
          "Education", "Unemployed", "Looking after home / family", "Other"
        )
      )
    )

  return(data)
}

# Function to create detailed variables for age 25 and 32
create_detailed_variable <- function(data, wave, metadata) {
  var <- metadata$var

  if (!var %in% names(data)) {
    message("Variable '", var, "' not found in dataset. Skipping.")
    return(data)
  }

  data <- data %>%
    mutate(
      !!paste0("ecoactadu", wave) := 
        !!sym(var)
    )

  # Convert to factor with labels
  data <- data %>%
    mutate(
      !!paste0("ecoactadu", wave) := factor(
        !!paste0("ecoactadu", wave),
        levels = c("-9", "-8", "-7", "-3", "-2", "-1", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
        labels = c(
          "Refusal", "Insufficient information", "Prefer not to say", "Not asked",
          "Schedule not applicable", "Item not applicable", "Employee - in paid work", "Self employed",
          "In unpaid/voluntary work", "Unemployed", "Education", "Apprenticeship", 
          "On gov't scheme for employment training", "Sick or disabled", "Looking after home or family", "Something else"
        )
      )
    )

  return(data)
}

# Harmonize collapsed variables for waves 17, 18, 19, 20
message("Harmonizing wave 17...")
merged_data <- harmonize_variable(merged_data, "17", metadata$wave_four)

message("Harmonizing wave 18...")
merged_data <- harmonize_variable(merged_data, "18", metadata$wave_five)

message("Harmonizing wave 19...")
merged_data <- harmonize_variable(merged_data, "19", metadata$wave_six)

message("Harmonizing wave 20...")
merged_data <- harmonize_variable(merged_data, "20", metadata$wave_seven)

# Create detailed variables for waves 25 and 32
message("Creating detailed variable for wave 25...")
merged_data <- create_detailed_variable(merged_data, "25", metadata$wave_eight)

message("Creating detailed variable for wave 32...")
merged_data <- create_detailed_variable(merged_data, "32", metadata$wave_nine)

# Select only required columns
required_columns <- c("NSID", "ecoact17", "ecoact18", "ecoact19", "ecoact20", "ecoact25", "ecoact32", "ecoactadu25", "ecoactadu32")

# Ensure all required columns exist
for (col in required_columns) {
  if (!col %in% names(merged_data)) {
    merged_data[[col]] <- NA
  }
}

# Write output
message("Writing output file...")
write_csv(merged_data[, required_columns], paste0(output_path, "cleaned_data.csv"))

message("Data processing completed successfully!")
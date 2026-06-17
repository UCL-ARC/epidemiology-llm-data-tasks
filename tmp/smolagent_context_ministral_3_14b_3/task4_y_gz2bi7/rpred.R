# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(readr)

# Load metadata files into separate objects
file_paths <- c(
  "data/input/wave_six_lsype_young_person_2020.tab",
  "data/input/wave_seven_lsype_young_person_2020.tab",
  "data/input/ns8_2015_self_completion.tab",
  "data/input/ns9_2022_main_interview.tab"
)

# Load each file explicitly by name
wave6 <- read_delim(file_paths[1], delim = "\t", show_col_types = FALSE)
wave7 <- read_delim(file_paths[2], delim = "\t", show_col_types = FALSE)
wave8 <- read_delim(file_paths[3], delim = "\t", show_col_types = FALSE)
wave9 <- read_delim(file_paths[4], delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID to ensure full cohort frame
merged_data <- full_join(wave6, wave7, by = "NSID")
merged_data <- full_join(merged_data, wave8, by = "NSID")
merged_data <- full_join(merged_data, wave9, by = "NSID")

# Define missing value mapping function
missing_value_mapping <- function(x, var_name) {
  if (!is.numeric(x)) return(x)
  
  # Handle W9SORI specific case
  if (var_name == "W9SORI") {
    x <- ifelse(x == 5, -7, x)
  }
  
  # Apply standard missing value mappings
  x <- ifelse(x == -97 | x == -100, -9, x)
  x <- ifelse(x == -999 | x == -998 | x == -997 | x == -995, -2, x)
  x <- ifelse(x == -94, -8, x)
  x <- ifelse(x == -92, -9, x)
  x <- ifelse(x == -91, -1, x)
  x <- ifelse(x == -99, -3, x)
  x <- ifelse(is.na(x), -3, x)
  
  return(x)
}

# Apply missing value mapping to source variables
if ("W6SexualityYP" %in% names(merged_data)) {
  merged_data$W6SexualityYP <- missing_value_mapping(merged_data$W6SexualityYP, "W6SexualityYP")
}
if ("W7SexualityYP" %in% names(merged_data)) {
  merged_data$W7SexualityYP <- missing_value_mapping(merged_data$W7SexualityYP, "W7SexualityYP")
}
if ("W8SEXUALITY" %in% names(merged_data)) {
  merged_data$W8SEXUALITY <- missing_value_mapping(merged_data$W8SEXUALITY, "W8SEXUALITY")
}
if ("W9SORI" %in% names(merged_data)) {
  merged_data$W9SORI <- missing_value_mapping(merged_data$W9SORI, "W9SORI")
}

# Function to create labelled variables
create_sori_variable <- function(data, source_var, output_var) {
  data <- data %>%
    mutate(!!output_var := case_when(
      !!sym(source_var) == 1 ~ 1,
      !!sym(source_var) == 2 ~ 2,
      !!sym(source_var) == 3 ~ 3,
      !!sym(source_var) == 4 ~ 4,
      TRUE ~ !!sym(source_var)
    ))
  
  data[[output_var]] <- factor(data[[output_var]],
                               levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4),
                               labels = c("Refusal", "Don't know", "Prefer not to say", 
                                          "Not asked", "Schedule not applicable", "Not applicable", 
                                          "Heterosexual/straight", "Gay/lesbian", "Bisexual", "Other"))
  
  return(data)
}

# Create sori variables
if ("W6SexualityYP" %in% names(merged_data)) {
  merged_data <- create_sori_variable(merged_data, "W6SexualityYP", "sori19")
}

if ("W7SexualityYP" %in% names(merged_data)) {
  merged_data <- create_sori_variable(merged_data, "W7SexualityYP", "sori20")
}

if ("W8SEXUALITY" %in% names(merged_data)) {
  merged_data <- create_sori_variable(merged_data, "W8SEXUALITY", "sori25")
}

if ("W9SORI" %in% names(merged_data)) {
  merged_data <- create_sori_variable(merged_data, "W9SORI", "sori32")
}

# Remove raw source variables and keep only final derived variables
final_vars <- c("NSID", "sori19", "sori20", "sori25", "sori32")
final_data <- merged_data %>% select(all_of(final_vars))

# Check if we have any NA values in the final data
cat("Number of rows in final dataset:", nrow(final_data), "\n")
cat("Number of columns in final dataset:", ncol(final_data), "\n")
cat("Columns in final dataset:", names(final_data), "\n")

# Write the final cleaned data to CSV
output_path <- "data/output/cleaned_data.csv"
write_csv(final_data, output_path)

# Verify the file was created
if (file.exists(output_path)) {
  cat("Successfully created output file at:", output_path, "\n")
  cat("First few rows of the output:\n")
  print(head(final_data))
} else {
  stop("Failed to create output file!")
}
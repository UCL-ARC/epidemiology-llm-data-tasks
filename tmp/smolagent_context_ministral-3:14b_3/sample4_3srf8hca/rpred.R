
# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Suppress unnecessary messages
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(purrr)
  library(labelled)
  library(readr)
})

# Define file paths
file_paths <- c(
  "data/input/wave_one_lsype_young_person_2020.tab",
  "data/input/wave_four_lsype_young_person_2020.tab",
  "data/input/wave_six_lsype_young_person_2020.tab",
  "data/input/wave_seven_lsype_young_person_2020.tab",
  "data/input/ns8_2015_self_completion.tab",
  "data/input/ns9_2022_main_interview.tab"
)

# Load each dataset
load_data <- function(file_path) {
  read_delim(file_path, delim = "\t", show_col_types = FALSE)
}

# Load all datasets into a list
datasets <- lapply(file_paths, load_data)

# Name each dataset for clarity
names(datasets) <- c(
  "wave_one",
  "wave_four",
  "wave_six",
  "wave_seven",
  "wave_eight",
  "wave_nine"
)

# Merge datasets by NSID
merged_data <- reduce(datasets, full_join, by = "NSID")

# Function to harmonize missing values
harmonize_missing_values <- function(x) {
  if (is.numeric(x)) {
    x <- case_when(
      x == -999 ~ -9,
      x == -998 ~ -8,
      x == -997 ~ -7,
      x == -94 ~ -9,
      x == -92 ~ -9,
      x == -91 ~ -1,
      x == -9 ~ -9,
      x == -8 ~ -8,
      x == -3 ~ -3,
      x == -1 ~ -1,
      x == -100 ~ -9,
      TRUE ~ x
    )
  }
  return(x)
}

# Create labels function
create_labels <- function() {
  setNames(
    c(-9, -8, -7, -1, -3, 1, 2, 3, 4),
    c(
      "Refusal",
      "Don't know/insufficient information",
      "Prefer not to say",
      "Item not applicable",
      "Not asked at the fieldwork stage/participated/interviewed",
      "Heterosexual / Straight",
      "Gay / Lesbian",
      "Bisexual",
      "Other"
    )
  )
}

# Function to recode and harmonize sexual orientation
harmonize_sori <- function(data, wave_name) {
  sori_var <- paste0("W", wave_name[-1], "SexualityYP")
  if (wave_name == "wave_eight") {
    sori_var <- "W8SEXUALITY"
  } else if (wave_name == "wave_nine") {
    sori_var <- "W9SORI"
  }

  if (sori_var %in% names(data)) {
    # Apply missing value harmonization
    data[[sori_var]] <- harmonize_missing_values(data[[sori_var]])

    # Create age-specific variable name
    age <- ifelse(wave_name == "wave_one", 14,
                  ifelse(wave_name == "wave_four", 17,
                  ifelse(wave_name == "wave_six", 19,
                  ifelse(wave_name == "wave_seven", 20,
                  ifelse(wave_name == "wave_eight", 22, 32)))))

    age_sori_var <- paste0("sori", age)

    # Recode to standard categories
    data[[age_sori_var]] <- case_when(
      data[[sori_var]] %in% c(1, 2, 3, 4) ~ data[[sori_var]],
      TRUE ~ as.numeric(NA)
    )

    # Drop the original variable
    data <- data %>% select(-all_of(sori_var))

    return(data)
  } else {
    return(data)
  }
}

# Apply harmonization for sexual orientation across waves
for (wave_name in names(datasets)) {
  merged_data <- harmonize_sori(merged_data, wave_name)
}

# Create factor variables with labels for sexual orientation
sori_labels <- create_labels()

# Apply labels to existing age-specific variables
for (age in c(14, 17, 19, 22, 32)) {
  var_name <- paste0("sori", age)
  if (var_name %in% names(merged_data)) {
    merged_data[[var_name]] <- factor(
      merged_data[[var_name]],
      levels = c(-9, -8, -7, -1, -3, 1, 2, 3, 4),
      labels = sori_labels
    )
  }
}

# Create consolidated variable only if we have any sori variables
existing_sori_vars <- grep("^sori", names(merged_data), value = TRUE)

if (length(existing_sori_vars) > 0) {
  # Define the order we want to check variables (highest age first)
  age_priority <- c(32, 22, 19, 17, 14)

  # Create a list of variables in priority order
  priority_vars <- sapply(age_priority, function(age) {
    paste0("sori", age)
  })

  # Filter to only include variables that actually exist
  priority_vars <- priority_vars[priority_vars %in% existing_sori_vars]

  # Create consolidated variable using coalesce
  merged_data <- merged_data %>%
    mutate(sori_consolidated = coalesce(!!!syms(priority_vars)))

  # Apply labels to consolidated variable
  merged_data$sori_consolidated <- factor(
    merged_data$sori_consolidated,
    levels = c(-9, -8, -7, -1, -3, 1, 2, 3, 4),
    labels = sori_labels
  )
} else {
  # If no sori variables exist, create an empty factor variable
  merged_data$sori_consolidated <- factor(-3, levels = c(-9, -8, -7, -1, -3, 1, 2, 3, 4), labels = sori_labels)
}

# Select only the ID and derived variables for output
output_vars <- c("NSID", grep("^sori", names(merged_data), value = TRUE))

# Ensure only the required variables are selected
output_data <- merged_data %>% select(all_of(output_vars))

# Write output to file
write_csv(output_data, "data/output/cleaned_data.csv")

# Print confirmation
message("Cleaned data has been saved to data/output/cleaned_data.csv")

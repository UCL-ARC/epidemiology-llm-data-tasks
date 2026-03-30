
# Load required packages
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(purrr)
  library(labelled)
  library(readr)
})

# Define file paths
file_paths <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_two = "data/input/wave_two_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  ns8_derived = "data/input/ns8_2015_derived.tab",
  ns9_derived = "data/input/ns9_2022_derived_variables.tab"
)

# Load and process each dataset
load_and_process <- function(file_path, wave_name, var_name, age_suffix) {
  cat('Loading', wave_name, 'data...\n')
  data <- readr::read_delim(file_path, delim = "\t", show_col_types = FALSE) %>%
    mutate(NSID = as.character(NSID))

  cat('Processing', var_name, 'variable...\n')

  # Ensure the variable is numeric
  data[[var_name]] <- as.numeric(data[[var_name]])

  # Apply missing value mapping
  if (wave_name == "wave_one") {
    data <- data %>%
      mutate(!!var_name := recode(!!sym(var_name),
                                 '-999.0' = -3, '-94.0' = -8, '-92.0' = -9, '-91.0' = -1, '-1.0' = -8))
  } else if (wave_name == "wave_two") {
    data <- data %>%
      mutate(!!var_name := recode(!!sym(var_name),
                                 '-998.0' = -3, '-997.0' = -3, '-995.0' = -3, '-99.0' = -3,
                                 '-92.0' = -9, '-91.0' = -1, '-1.0' = -8))
  } else if (wave_name == "wave_four") {
    data <- data %>%
      mutate(!!var_name := recode(!!sym(var_name), '-94.0' = -8, '-1.0' = -8))
  } else if (wave_name == "ns8_derived") {
    data <- data %>%
      mutate(!!var_name := recode(!!sym(var_name), '-9.0' = -9, '-8.0' = -8, '-1.0' = -1))
  } else if (wave_name == "ns9_derived") {
    data <- data %>%
      mutate(!!var_name := recode(!!sym(var_name), '-8.0' = -8))
  }

  # Rename ethnicity variables to age-specific names
  new_var_name <- paste0("eth", age_suffix)
  data <- data %>% rename(!!sym(new_var_name) := !!sym(var_name))

  cat('Finished processing', wave_name, 'data.\n')
  return(data)
}

# Load all datasets
wave_one_data <- load_and_process(file_paths$wave_one, "wave_one", "W1ethnic2YP", "14")
wave_two_data <- load_and_process(file_paths$wave_two, "wave_two", "W2ethnicYP", "15")
wave_four_data <- load_and_process(file_paths$wave_four, "wave_four", "w4ethnic2YP", "17")
ns8_data <- load_and_process(file_paths$ns8_derived, "ns8_derived", "W8DETHN15", "23")
ns9_data <- load_and_process(file_paths$ns9_derived, "ns9_derived", "W9DETHN15", "32")

# Merge datasets by NSID
cat('Merging datasets...\n')
merged_data <- wave_one_data %>%
  full_join(wave_two_data, by = "NSID") %>%
  full_join(wave_four_data, by = "NSID") %>%
  full_join(ns8_data, by = "NSID") %>%
  full_join(ns9_data, by = "NSID")

cat('Merged dataset dimensions:', nrow(merged_data), 'rows,', ncol(merged_data), 'columns.\n')

# Define ethnicity labels for factor variables
ethnicity_labels <- setNames(
  c("Refusal", "Insufficient information", "Not asked", "Schedule not applicable", "Not applicable",
    "White - British", "White - Irish", "Any other White background", "Mixed - White and Black Caribbean",
    "Mixed - White and Black African", "Mixed - White and Asian", "Any other mixed background", "Indian",
    "Pakistani", "Bangladeshi", "Any other Asian background", "Black Caribbean", "Black African",
    "Any other Black background", "Chinese", "Any other ethnic background"),
  c(-9, -8, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
)

# Create consolidated ethnicity variable
cat('Creating consolidated ethnicity variable...\n')
consolidated_ethnicity <- function(data) {
  data %>%
    mutate(
      eth_consolidated = case_when(
        eth32 != -3 & eth32 != -2 & eth32 != -1 & eth32 != -8 & eth32 != -9 ~ eth32,
        eth23 != -3 & eth23 != -2 & eth23 != -1 & eth23 != -8 & eth23 != -9 ~ eth23,
        eth17 != -3 & eth17 != -2 & eth17 != -1 & eth17 != -8 & eth17 != -9 ~ eth17,
        eth15 != -3 & eth15 != -2 & eth15 != -1 & eth15 != -8 & eth15 != -9 ~ eth15,
        eth14 != -3 & eth14 != -2 & eth14 != -1 & eth14 != -8 & eth14 != -9 ~ eth14,
        TRUE ~ -3
      )
    ) %>%
    mutate(eth_consolidated = factor(eth_consolidated, levels = names(ethnicity_labels), labels = ethnicity_labels))
}

# Apply consolidated ethnicity variable
cleaned_data <- consolidated_ethnicity(merged_data)
cat('Consolidated ethnicity variable created.\n')

# Select only the ID and derived variables
final_data <- cleaned_data %>%
  select(NSID, eth_consolidated, eth14, eth15, eth17, eth23, eth32)

cat('Final dataset dimensions:', nrow(final_data), 'rows,', ncol(final_data), 'columns.\n')

# Write output to CSV
cat('Writing output to CSV...\n')
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)
cat('Output written successfully.\n')

# Load required packages
library(haven)
library(dplyr)
library(readr)

# Define file paths
file_paths <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_two = "data/input/wave_two_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  ns8_derived = "data/input/ns8_2015_derived.tab",
  ns9_derived = "data/input/ns9_2022_derived_variables.tab"
)

# Function to map missing values
map_missing <- function(x) {
  x <- as.numeric(x)
  x[x == -999] <- -3
  x[x == -998] <- -3
  x[x == -997] <- -3
  x[x == -99]  <- -3
  x[x == -94]  <- -8
  x[x == -92]  <- -9
  x[x == -91]  <- -1
  x[x == -9]   <- -9
  x[x == -8]   <- -8
  x[x == -1]   <- -1
  return(x)
}

# Load and process each dataset
process_data <- function(file_path, var_name, new_name) {
  data <- read_delim(file_path, delim = "\t") %>%
    mutate(!!new_name := map_missing(!!sym(var_name)))
  return(data)
}

# Load datasets
wave_one_data <- process_data(file_paths$wave_one, "W1ethnic2YP", "eth14")
wave_two_data <- process_data(file_paths$wave_two, "W2ethnicYP", "eth15")
wave_four_data <- process_data(file_paths$wave_four, "w4ethnic2YP", "eth17")
ns8_data <- process_data(file_paths$ns8_derived, "W8DETHN15", "eth32")
ns9_data <- process_data(file_paths$ns9_derived, "W9DETHN15", "eth32_w9")

# Merge datasets by NSID
merged_data <- wave_one_data %>%
  full_join(wave_two_data, by = "NSID") %>%
  full_join(wave_four_data, by = "NSID") %>%
  full_join(ns8_data, by = "NSID") %>%
  full_join(ns9_data, by = "NSID")

# Define ethnicity labels
eth_levels <- c(-9, -8, -1, -3, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
eth_labels <- c("Refusal", "Don't know/Insufficient information", "Item not applicable",
               "Not asked/Not interviewed", "White - British", "White - Irish",
               "Any other White background", "Mixed - White and Black Caribbean",
               "Mixed - White and Black African", "Mixed - White and Asian",
               "Any other mixed background", "Indian", "Pakistani", "Bangladeshi",
               "Any other Asian background", "Black Caribbean", "Black African",
               "Any other Black background", "Chinese", "Any other ethnic background")

# Create consolidated ethnicity variable
cleaned_data <- merged_data %>%
  # First create numeric consolidated variable
  mutate(
    eth_consolidated = case_when(
      eth14 > 0 ~ eth14,
      eth15 > 0 ~ eth15,
      eth17 > 0 ~ eth17,
      eth32 > 0 ~ eth32,
      eth32_w9 > 0 ~ eth32_w9,
      TRUE ~ -3
    )
  ) %>%
  # Then create labeled factors for each variable
  mutate(
    eth14 = factor(eth14, levels = eth_levels, labels = eth_labels),
    eth15 = factor(eth15, levels = eth_levels, labels = eth_labels),
    eth17 = factor(eth17, levels = eth_levels, labels = eth_labels),
    eth32 = factor(eth32, levels = eth_levels, labels = eth_labels),
    eth_consolidated = factor(eth_consolidated, levels = eth_levels, labels = eth_labels)
  ) %>%
  select(NSID, eth14, eth15, eth17, eth32, eth_consolidated)

# Write output
write_csv(as.data.frame(cleaned_data), "data/output/cleaned_data.csv")

# Print confirmation
message("Cleaned data has been saved to data/output/cleaned_data.csv")
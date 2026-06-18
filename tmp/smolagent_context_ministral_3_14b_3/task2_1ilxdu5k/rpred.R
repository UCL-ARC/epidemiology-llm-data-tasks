
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- list(
  wave_one = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_two = 'data/input/wave_two_lsype_young_person_2020.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  ns8_derived = 'data/input/ns8_2015_derived.tab',
  ns9_derived = 'data/input/ns9_2022_derived_variables.tab'
)

# Load each file into separate objects
wave_one_data <- read_delim(files$wave_one, delim = "\t")
wave_two_data <- read_delim(files$wave_two, delim = "\t")
wave_four_data <- read_delim(files$wave_four, delim = "\t")
ns8_data <- read_delim(files$ns8_derived, delim = "\t")
ns9_data <- read_delim(files$ns9_derived, delim = "\t")

# Merge all datasets by NSID
merged_data <- full_join(
  full_join(wave_one_data, wave_two_data, by = "NSID"),
  full_join(wave_four_data, ns8_data, by = "NSID"),
  by = "NSID"
) %>%
  full_join(ns9_data, by = "NSID")

# Define the mapping for missing values to standard codes
missing_value_mapping <- function(x) {
  if (is.na(x)) return(-3)  # Default for NA
  if (x == -999 | x == -998 | x == -997 | x == -995) return(-2)  # Schedule not applicable/script error
  if (x == -94) return(-8)  # Insufficient information
  if (x == -92) return(-9)  # Refused
  if (x == -91 | x == -1) return(-1)  # Not applicable/Don't know
  if (x == -99) return(-3)  # Not interviewed
  if (x == -9 | x == -8) return(-8)  # Refused/Insufficient information
  return(x)
}

# Create a function to derive the consolidated ethnicity variable
derive_ethnicity <- function(data) {
  # Define the priority order for source variables
  sources <- c(
    "W1ethnic2YP" = 1,
    "W2ethnicYP" = 2,
    "w4ethnic2YP" = 3,
    "W8DETHN15" = 4,
    "W9DETHN15" = 5
  )

  # Create a new column for the consolidated ethnicity variable
  data <- data %>%
    mutate(
      eth = pmap_dbl(
        list(
          W1ethnic2YP = coalesce(W1ethnic2YP, NA),
          W2ethnicYP = coalesce(W2ethnicYP, NA),
          w4ethnic2YP = coalesce(w4ethnic2YP, NA),
          W8DETHN15 = coalesce(W8DETHN15, NA),
          W9DETHN15 = coalesce(W9DETHN15, NA)
        ),
        ~ {
          # Apply missing value mapping to each source variable
          mapped_values <- map_dbl(.x, missing_value_mapping)

          # Use the first valid positive response
          valid_values <- mapped_values[mapped_values > 0]
          if (length(valid_values) > 0) {
            return(valid_values[1])
          } else {
            return(-3)  # Fallback to missing code if no valid response
          }
        }
      )
    )

  # Define the value labels for the consolidated ethnicity variable
  eth_labels <- setNames(
    c("Refusal", "Insufficient information", "Prefer not to say", "Not asked/not interviewed", "Schedule not applicable/script error", "Item not applicable",
      "White - British", "White - Irish", "Any other White background", "Mixed - White and Black Caribbean", "Mixed - White and Black African",
      "Mixed - White and Asian", "Any other mixed background", "Indian", "Pakistani", "Bangladeshi", "Any other Asian background",
      "Black Caribbean", "Black African", "Any other Black background", "Chinese", "Any other ethnic background"),
    c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
  )

  # Create a labelled factor for the consolidated ethnicity variable
  data$eth <- factor(
    data$eth,
    levels = names(eth_labels),
    labels = eth_labels[names(eth_labels)]
  )

  return(data)
}

# Derive the consolidated ethnicity variable
cleaned_data <- derive_ethnicity(merged_data)

# Select only the ID variable and the consolidated ethnicity variable
final_data <- cleaned_data %>%
  select(NSID, eth)

# Write the final dataset to a CSV file
write_csv(final_data, "data/output/cleaned_data.csv")

# Print confirmation
cat("Consolidated ethnicity variable 'eth' has been successfully derived and saved to 'data/output/cleaned_data.csv'.\n")

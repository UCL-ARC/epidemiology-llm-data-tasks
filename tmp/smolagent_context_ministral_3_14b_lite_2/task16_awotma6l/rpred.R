
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load metadata and file paths
metadata <- list(
  wave_four_lsype_family_background_2020 = list(
    file = "data/input/wave_four_lsype_family_background_2020.tab",
    id_var = "NSID",
    income_var = "w4IncEstW",
    age = 17
  ),
  wave_three_lsype_family_background_2020 = list(
    file = "data/input/wave_three_lsype_family_background_2020.tab",
    id_var = "NSID",
    income_var = "W3incestw",
    age = 16
  ),
  wave_two_lsype_family_background_2020 = list(
    file = "data/input/wave_two_lsype_family_background_2020.tab",
    id_var = "NSID",
    income_var = "W2GrsswkHH",
    age = 15
  ),
  wave_one_lsype_family_background_2020 = list(
    file = "data/input/wave_one_lsype_family_background_2020.tab",
    id_var = "NSID",
    income_var = "W1GrsswkHH",
    age = 14
  )
)

# Load files with explicit column types
load_files <- function(file) {
  data <- read_delim(file, delim = "\t", col_types = cols(
    NSID = col_character(),
    .default = col_double()
  ))
  print(paste("Loaded file:", file, "with", nrow(data), "rows"))
  return(data)
}

# Load each file
wave_four_data <- load_files(metadata$wave_four_lsype_family_background_2020$file)
wave_three_data <- load_files(metadata$wave_three_lsype_family_background_2020$file)
wave_two_data <- load_files(metadata$wave_two_lsype_family_background_2020$file)
wave_one_data <- load_files(metadata$wave_one_lsype_family_background_2020$file)

# Merge all datasets by NSID
merged_data <- full_join(
  full_join(wave_one_data, wave_two_data, by = "NSID"),
  full_join(wave_three_data, wave_four_data, by = "NSID"),
  by = "NSID"
)
print(paste("Merged dataset has", nrow(merged_data), "rows"))

# Define missing value mappings
missing_mapping <- function(x) {
  if (is.numeric(x)) {
    x <- ifelse(x %in% c(-999, -992, -99), -3, x)  # Schedule not applicable, script error, or information lost
    x <- ifelse(x %in% c(-94, -994), -8, x)        # Insufficient information
    x <- ifelse(x == -92, -9, x)                   # Refused
    x <- ifelse(x == -91, -1, x)                   # Not applicable
    x <- ifelse(is.na(x), -3, x)                   # NA values
    return(x)
  } else {
    return(x)
  }
}

# Create banded income variables for each wave
for (wave in 1:4) {
  age <- c(14, 15, 16, 17)[wave]
  income_var <- c("W1GrsswkHH", "W2GrsswkHH", "W3incestw", "w4IncEstW")[wave]
  var_name <- paste0("banded_income_", age)

  # Create banded income variable
  merged_data[[var_name]] <- case_when(
    merged_data[[income_var]] == 1 ~ "up_to_49",
    merged_data[[income_var]] == 2 ~ "50_to_99",
    merged_data[[income_var]] == 3 ~ "100_to_199",
    merged_data[[income_var]] == 4 ~ "200_to_299",
    merged_data[[income_var]] == 5 ~ "300_to_399",
    merged_data[[income_var]] == 6 ~ "400_to_499",
    merged_data[[income_var]] == 7 ~ "500_to_599",
    merged_data[[income_var]] == 8 ~ "600_to_699",
    merged_data[[income_var]] == 9 ~ "700_to_799",
    merged_data[[income_var]] == 10 ~ "800_to_899",
    merged_data[[income_var]] == 11 ~ "900_to_999",
    merged_data[[income_var]] == 12 ~ "1000_or_more",
    TRUE ~ NA_character_
  )

  # Convert to labeled factor
  levels(merged_data[[var_name]]) <- c(
    "up_to_49", "50_to_99", "100_to_199", "200_to_299", "300_to_399",
    "400_to_499", "500_to_599", "600_to_699", "700_to_799", "800_to_899",
    "900_to_999", "1000_or_more"
  )
  merged_data[[var_name]] <- factor(merged_data[[var_name]],
                                  levels = levels(merged_data[[var_name]]),
                                  ordered = TRUE)
  print(paste("Created banded income variable for age", age))

  # Create continuous income variable for ages 14 and 15
  if (age %in% c(14, 15)) {
    continuous_var_name <- paste0("continuous_income_", age)
    merged_data[[continuous_var_name]] <- missing_mapping(merged_data[[income_var]])

    # Convert numeric codes to midpoints
    merged_data[[continuous_var_name]] <- case_when(
      merged_data[[income_var]] == 1 ~ 24.5,   # Midpoint of up to £49
      merged_data[[income_var]] == 2 ~ 74.5,   # Midpoint of £50 to £99
      merged_data[[income_var]] == 3 ~ 149.5,  # Midpoint of £100 to £199
      merged_data[[income_var]] == 4 ~ 249.5,  # Midpoint of £200 to £299
      merged_data[[income_var]] == 5 ~ 349.5,  # Midpoint of £300 to £399
      merged_data[[income_var]] == 6 ~ 449.5,  # Midpoint of £400 to £499
      merged_data[[income_var]] == 7 ~ 549.5,  # Midpoint of £500 to £599
      merged_data[[income_var]] == 8 ~ 649.5,  # Midpoint of £600 to £699
      merged_data[[income_var]] == 9 ~ 749.5,  # Midpoint of £700 to £799
      merged_data[[income_var]] == 10 ~ 849.5, # Midpoint of £800 to £899
      merged_data[[income_var]] == 11 ~ 949.5, # Midpoint of £900 to £999
      merged_data[[income_var]] == 12 ~ 1000,  # £1,000 or more
      TRUE ~ NA_real_
    )
    print(paste("Created continuous income variable for age", age))
  }
}

# Extract only relevant variables
output_vars <- c(
  "NSID",
  "banded_income_14", "continuous_income_14",
  "banded_income_15", "continuous_income_15",
  "banded_income_16", "banded_income_17"
)

final_data <- merged_data %>%
  select(all_of(output_vars))

print(paste("Final dataset has", nrow(final_data), "rows and", ncol(final_data), "columns"))

# Write the output file
file_path <- "data/output/cleaned_data.csv"
write_csv(final_data, file_path)
print(paste("Output file written to:", file_path))

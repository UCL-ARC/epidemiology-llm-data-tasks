
# Load required packages
library(readr)

# Define missing value mappings
missing_value_mapping <- list(
  wave_one = list(c(-999, -3), c(-94, -8), c(-92, -9), c(-91, -1), c(-1, -8)),
  wave_two = list(c(-998, -3), c(-997, -3), c(-995, -3), c(-99, -3),
                  c(-92, -9), c(-91, -1), c(-1, -8)),
  wave_four = list(c(-999, -3), c(-94, -8), c(-1, -8)),
  ns8 = list(c(-9, -9), c(-8, -8), c(-1, -1)),
  ns9 = list(c(-8, -8))
)

# Function to manually recode values
manual_recode <- function(x, mapping) {
  for (pair in mapping) {
    x[x == pair[1]] <- pair[2]
  }
  return(x)
}

# Function to process each dataset
process_dataset <- function(file_path, wave_name, age_suffix, eth_col) {
  # Read the data
  data <- read_delim(file_path, delim = "\t")

  # Extract NSID and ethnicity columns
  nsid_col <- data[[1]]
  eth_col_data <- data[[eth_col]]

  # Apply missing value mapping
  eth_col_data <- manual_recode(eth_col_data, missing_value_mapping[[wave_name]])

  # Create data frame
  result <- data.frame(NSID = nsid_col, eth = eth_col_data)
  names(result)[2] <- paste0("eth", age_suffix)

  return(result)
}

# Load all datasets
wave_one <- process_dataset("data/input/wave_one_lsype_young_person_2020.tab", "wave_one", "14", 350)
wave_two <- process_dataset("data/input/wave_two_lsype_young_person_2020.tab", "wave_two", "15", 86)
wave_four <- process_dataset("data/input/wave_four_lsype_young_person_2020.tab", "wave_four", "17", 994)
ns8 <- process_dataset("data/input/ns8_2015_derived.tab", "ns8", "32", 42)
ns9 <- process_dataset("data/input/ns9_2022_derived_variables.tab", "ns9", "32", 48)

# Merge datasets using base R merge
merged_data <- merge(wave_one, wave_two, by = "NSID", all.x = TRUE)
merged_data <- merge(merged_data, wave_four, by = "NSID", all.x = TRUE)
merged_data <- merge(merged_data, ns8, by = "NSID", all.x = TRUE)
merged_data <- merge(merged_data, ns9, by = "NSID", all.x = TRUE)

# Consolidate ethnicity using base R
find_first_valid <- function(row) {
  valid_values <- row[row > 0]
  if (length(valid_values) > 0) {
    return(valid_values[1])
  } else {
    return(-3)
  }
}

eth_cols <- c("eth14", "eth15", "eth17", "eth32", "eth32")
eth_cols <- c("eth14", "eth15", "eth17", "eth32", "eth32_ns9")
# Correct column names
eth_cols <- c("eth14", "eth15", "eth17", "eth32", "eth32_ns9")
# Fix column names to match actual data
eth_cols <- c("eth14", "eth15", "eth17", "eth32_ns8", "eth32_ns9")

merged_data$eth_consolidated <- apply(merged_data[, eth_cols], 1, find_first_valid)

# Define labels for factor levels
eth_levels <- c(-9, -8, -3, -2, -1, 1:16)
eth_lab <- c(
  "Refusal", "Don't know/insufficient information",
  "Not asked/participated/interviewed", "Schedule not applicable",
  "Item not applicable", "White - British", "White - Irish",
  "Any other White background", "Mixed - White and Black Caribbean",
  "Mixed - White and Black African", "Mixed - White and Asian",
  "Any other mixed background", "Indian", "Pakistani",
  "Bangladeshi", "Any other Asian background",
  "Black Caribbean", "Black African",
  "Any other Black background", "Chinese",
  "Any other ethnic background"
)

# Create factor variable
merged_data$eth_consolidated <- factor(
  merged_data$eth_consolidated,
  levels = eth_levels,
  labels = eth_lab
)

# Select and save output
output_data <- merged_data[, c("NSID", "eth_consolidated")]
write.csv(output_data, "data/output/cleaned_data.csv", row.names = FALSE)

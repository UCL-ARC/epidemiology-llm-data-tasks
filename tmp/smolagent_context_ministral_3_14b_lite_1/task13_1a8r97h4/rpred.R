
# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Suppress some messages for cleaner output
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(labelled)
})

# Define the mapping for NS-SEC major categories
nssec_major_mapping <- data.frame(
  original = as.character(c(
    1, 2, 3.1, 3.2, 3.3, 3.4, 4.1, 4.2, 4.3, 4.4, 5, 6,
    7.1, 7.2, 7.3, 7.4,
    8.1, 8.2, 9.1, 9.2,
    10, 11.1, 11.2,
    12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7,
    13.1, 13.2, 13.3, 13.4, 13.5,
    14.1, 14.2, 14.3, 15, 16, 17
  )),
  major = c(
    rep("1", 12), rep("2", 4), rep("3", 4), rep("4", 3),
    rep("5", 7), rep("6", 5), rep("7", 6)
  ),
  stringsAsFactors = FALSE
)

# Define missing value mapping
missing_value_mapping <- data.frame(
  original = c(-999, -99, -98, -94, -92, -91),
  standard = c(-2, -3, -1, -8, -9, -1),
  stringsAsFactors = FALSE
)

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")

# Merge all waves into a single dataset
merged_data <- full_join(wave1, wave2, by = "NSID")
merged_data <- full_join(merged_data, wave3, by = "NSID")
merged_data <- full_join(merged_data, wave4, by = "NSID")
merged_data <- full_join(merged_data, wave5, by = "NSID")

# Function to map missing values
map_missing_values <- function(x) {
  if (is.numeric(x)) {
    mapped <- replace_na(x, -3)
    mapped[mapped %in% missing_value_mapping$original] <- missing_value_mapping$standard[match(mapped[mapped %in% missing_value_mapping$original], missing_value_mapping$original)]
    return(mapped)
  } else {
    return(x)
  }
}

# Function to map NS-SEC to major categories
map_nssec_major <- function(x) {
  if (is.numeric(x)) {
    x_char <- as.character(x)
    mapped <- ifelse(x_char %in% nssec_major_mapping$original,
                     nssec_major_mapping$major[match(x_char, nssec_major_mapping$original)],
                     NA_character_)
    return(mapped)
  } else {
    return(x)
  }
}

# Process mother's NS-SEC variables
mother_vars <- list(
  W1nsseccatmum = "nssecma14",
  W2nsseccatmum = "nssecma15",
  W3cnsseccatmum = "nssecma16",
  w4cnsseccatmum = "nssecma17",
  w5Cnsseccatmum = "nssecma18"
)

# Process father's NS-SEC variables
father_vars <- list(
  W1nsseccatdad = "nssecpa14",
  W2nsseccatdad = "nssecpa15",
  W3cnsseccatdad = "nssecpa16",
  w4cnsseccatdad = "nssecpa17",
  w5Cnsseccatdad = "nssecpa18"
)

# Create variables for mother
for (var_name in names(mother_vars)) {
  new_var_name <- mother_vars[[var_name]]
  if (var_name %in% colnames(merged_data)) {
    merged_data[[new_var_name]] <- map_nssec_major(map_missing_values(merged_data[[var_name]]))
  }
}

# Create variables for father
for (var_name in names(father_vars)) {
  new_var_name <- father_vars[[var_name]]
  if (var_name %in% colnames(merged_data)) {
    merged_data[[new_var_name]] <- map_nssec_major(map_missing_values(merged_data[[var_name]]))
  }
}

# Remove raw source variables
raw_source_vars <- c(
  "W1nsseccatmum", "W2nsseccatmum", "W3cnsseccatmum", "w4cnsseccatmum", "w5Cnsseccatmum",
  "W1nsseccatdad", "W2nsseccatdad", "W3cnsseccatdad", "w4cnsseccatdad", "w5Cnsseccatdad"
)

merged_data <- merged_data %>%
  select(-any_of(raw_source_vars))

# Define labels for the major NS-SEC categories
nssec_major_labels <- c(
  "1" = "Employers and managers",
  "2" = "Intermediate occupations",
  "3" = "Small employers and own account workers",
  "4" = "Lower supervisory and technical",
  "5" = "Semi-routine occupations",
  "6" = "Routine occupations",
  "7" = "Special categories"
)

# Apply labels to each variable
for (var in c(names(mother_vars), names(father_vars))) {
  if (var %in% colnames(merged_data)) {
    merged_data[[var]] <- factor(merged_data[[var]],
                                 levels = names(nssec_major_labels),
                                 labels = nssec_major_labels)
  }
}

# Check the final structure of the dataset
cat("Final dataset structure:", dim(merged_data), "\n")
cat("Column names:", paste(colnames(merged_data), collapse = ", "), "\n")

# Write the output to CSV
write_csv(merged_data, "data/output/cleaned_data.csv")
cat("Output file written to: data/output/cleaned_data.csv\n")

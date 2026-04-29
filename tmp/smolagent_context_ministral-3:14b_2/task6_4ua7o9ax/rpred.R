
# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(readr)

# Define file paths
file_paths <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_two = "data/input/wave_two_lsype_family_background_2020.tab",
  wave_three = "data/input/wave_three_lsype_family_background_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  ns8_derived = "data/input/ns8_2015_derived.tab",
  ns9_derived = "data/input/ns9_2022_derived_variables.tab",
  ns9_main = "data/input/ns9_2022_main_interview.tab"
)

# Load each dataset
wave_one <- readr::read_delim(file_paths$wave_one, delim = "\t")
wave_two <- readr::read_delim(file_paths$wave_two, delim = "\t")
wave_three <- readr::read_delim(file_paths$wave_three, delim = "\t")
wave_four <- readr::read_delim(file_paths$wave_four, delim = "\t")
ns8_derived <- readr::read_delim(file_paths$ns8_derived, delim = "\t")
ns9_derived <- readr::read_delim(file_paths$ns9_derived, delim = "\t")
ns9_main <- readr::read_delim(file_paths$ns9_main, delim = "\t")

# Merge datasets by NSID
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID")

# Function to standardize missing values
standardize_missing_values <- function(x) {
  x <- ifelse(x == -999 | x == -998 | x == -997 | x == -995 | x == -94 | x == -92 | x == -91 | x == -99, -3, x)
  x <- ifelse(x == -9, -9, x)  # Refusal
  x <- ifelse(x == -8, -8, x)  # Don't know/insufficient information
  x <- ifelse(x == -1, -1, x)  # Item not applicable
  x <- ifelse(is.na(x), -3, x) # Convert NA to -3
  return(x)
}

# Standardize missing values for urbind and gor
merged_data <- merged_data %>%
  mutate(
    urbind_15 = standardize_missing_values(urbind.x),
    urbind_16 = standardize_missing_values(urbind.y),
    gor_15 = standardize_missing_values(gor.x),
    gor_16 = standardize_missing_values(gor.y),
    W8DGOR = standardize_missing_values(W8DGOR),
    W9DRGN = standardize_missing_values(W9DRGN)
  )

# Create labeled factors for urbind variables
urbind_levels <- c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8)
urbind_labels <- c("Refusal", "Insufficient information", "Prefer not to say", "Not asked", "Schedule error", "Not applicable",
                  "Urban >= 10k - sparse", "Town & Fringe - sparse", "Village - sparse", "Hamlet and Isolated Dwelling - sparse",
                  "Urban >= 10k - less sparse", "Town & Fringe - less sparse", "Village - less sparse", "Hamlet & Isolated Dwelling")

merged_data <- merged_data %>%
  mutate(
    urbind_15 = factor(urbind_15, levels = urbind_levels, labels = urbind_labels),
    urbind_16 = factor(urbind_16, levels = urbind_levels, labels = urbind_labels)
  )

# Create labeled factors for gor variables
gor_levels <- c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
gor_labels <- c("Refusal", "Insufficient information", "Prefer not to say", "Not asked", "Schedule error", "Not applicable",
               "North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands",
               "East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland", "Unknown due to faulty/missing postcode")

merged_data <- merged_data %>%
  mutate(
    gor_15 = factor(gor_15, levels = gor_levels, labels = gor_labels),
    gor_16 = factor(gor_16, levels = gor_levels, labels = gor_labels),
    W8DGOR = factor(W8DGOR, levels = gor_levels, labels = gor_labels),
    W9DRGN = factor(W9DRGN, levels = gor_levels, labels = gor_labels)
  )

# Create derived variables for geographical region
merged_data <- merged_data %>%
  mutate(
    regub_15 = ifelse(as.numeric(as.character(urbind_15)) %in% c(1, 2, 5, 6), "Urban", "Rural"),
    regub_16 = ifelse(as.numeric(as.character(urbind_16)) %in% c(1, 2, 5, 6), "Urban", "Rural")
  )

# Convert derived binary variables to factors
merged_data$regub_15 <- factor(merged_data$regub_15, levels = c("Urban", "Rural"), ordered = TRUE)
merged_data$regub_16 <- factor(merged_data$regub_16, levels = c("Urban", "Rural"), ordered = TRUE)

# Create derived variable for nation of residence
merged_data <- merged_data %>%
  mutate(
    regint = ifelse(W9NATIONRES %in% c(1, 2, 3, 4), "UK", "Abroad")
  )

# Convert derived binary variable to factor
merged_data$regint <- factor(merged_data$regint, levels = c("UK", "Abroad"), ordered = TRUE)

# Select final variables for output
final_variables <- c("NSID", "urbind_15", "urbind_16", "gor_15", "gor_16", "W8DGOR", "W9DRGN", "regub_15", "regub_16", "regint")

# Output the cleaned data
write.csv(merged_data %>% select(all_of(final_variables)), "data/output/cleaned_data.csv", row.names = FALSE)
cat('Cleaned data has been written to data/output/cleaned_data.csv\n')

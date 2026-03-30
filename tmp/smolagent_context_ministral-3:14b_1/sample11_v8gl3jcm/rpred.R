
# Load required packages
library(haven)
library(dplyr)
library(readr)

# Load datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Function to map missing values to standard codes
map_missing <- function(x, wave) {
  x <- as.numeric(x)
  if (wave == "wave_one" || wave == "wave_two" || wave == "wave_three") {
    x <- ifelse(x == -999, -3, x)  # Missing household info
    x <- ifelse(x == -99, -3, x)   # Not interviewed
    x <- ifelse(x == -98, -3, x)   # Not present
    x <- ifelse(x == -94, -8, x)   # Insufficient info
  } else if (wave == "wave_four") {
    x <- ifelse(x == -999, -3, x)  # Missing household info
    x <- ifelse(x == -996, -1, x)  # No parent in household
    x <- ifelse(x == -99, -3, x)   # Not interviewed
    x <- ifelse(x == -98, -3, x)   # Not present
    x <- ifelse(x == -94, -8, x)   # Insufficient info
    x <- ifelse(x == -92, -9, x)   # Refusal
  }
  return(x)
}

# Process each wave
wave_one <- wave_one %>%
  mutate(
    ecoactmum14 = map_missing(W1empsmum, "wave_one"),
    ecoactdad14 = map_missing(W1empsdad, "wave_one")
  )

wave_two <- wave_two %>%
  mutate(
    ecoactmum15 = map_missing(W2empsmum, "wave_two"),
    ecoactdad15 = map_missing(W2empsdad, "wave_two")
  )

wave_three <- wave_three %>%
  mutate(
    ecoactmum16 = map_missing(W3empsmum, "wave_three"),
    ecoactdad16 = map_missing(W3empsdad, "wave_three")
  )

wave_four <- wave_four %>%
  mutate(
    ecoactmum17 = map_missing(w4empsmum, "wave_four"),
    ecoactdad17 = map_missing(w4empsdad, "wave_four")
  )

# Merge datasets by NSID
merged_data <- full_join(wave_one, wave_two, by = "NSID") %>%
  full_join(., wave_three, by = "NSID") %>%
  full_join(., wave_four, by = "NSID")

# Convert ecoact variables to numeric explicitly
merged_data <- merged_data %>%
  mutate(
    ecoactmum14 = as.numeric(ecoactmum14),
    ecoactdad14 = as.numeric(ecoactdad14),
    ecoactmum15 = as.numeric(ecoactmum15),
    ecoactdad15 = as.numeric(ecoactdad15),
    ecoactmum16 = as.numeric(ecoactmum16),
    ecoactdad16 = as.numeric(ecoactdad16),
    ecoactmum17 = as.numeric(ecoactmum17),
    ecoactdad17 = as.numeric(ecoactdad17)
  )

# Select only the ID and derived variables
cleaned_data <- merged_data %>%
  select(NSID, contains("ecoact"))

# Ensure output directory exists and write the file
output_path <- "data/output/cleaned_data.csv"
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}
write.csv(cleaned_data, output_path, row.names = FALSE)

# Verify file creation
file.exists(output_path)

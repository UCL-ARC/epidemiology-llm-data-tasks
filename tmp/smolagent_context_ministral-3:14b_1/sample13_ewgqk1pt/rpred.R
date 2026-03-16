# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
file_paths <- list(
  wave_one = "data/input/wave_one_lsype_family_background_2020.tab",
  wave_two = "data/input/wave_two_lsype_family_background_2020.tab",
  wave_three = "data/input/wave_three_lsype_family_background_2020.tab",
  wave_four = "data/input/wave_four_lsype_family_background_2020.tab",
  wave_five = "data/input/wave_five_lsype_family_background_2020.tab"
)

# Define NS-SEC labels
nssec_labels <- c(
  `-9` = "Refusal",
  `-8` = "Don't know",
  `-7` = "Prefer not to say",
  `-3` = "Not asked/not interviewed",
  `-2` = "Script error/information lost",
  `-1` = "Not applicable",
  `1` = "Employers in large organisations",
  `2` = "Higher managerial occupations",
  `3` = "Higher professional occupations",
  `4` = "Lower professional occupations",
  `5` = "Lower managerial occupations",
  `6` = "Higher supervisory occupations",
  `7` = "Intermediate occupations",
  `8` = "Employers in small orgs",
  `9` = "Own account workers",
  `10` = "Lower supervisory occupations",
  `11` = "Lower technical occupations",
  `12` = "Semi routine occupations",
  `13` = "Routine occupations",
  `14` = "Never worked/long-term unemployed",
  `15` = "Full-time students",
  `16` = "Not classified or inadequately stated",
  `17` = "Not classifiable for other reasons"
)

# Function to recode NS-SEC values
recode_nssec <- function(x) {
  x <- as.numeric(x)

  # Collapse detailed subcategories by taking the integer part
  x <- as.integer(x)

  # Recode missing values
  x[x == -999] <- -2
  x[x == -94] <- -8
  x[x == -99 | x == -98 | is.na(x)] <- -3

  # Ensure only valid categories (1-17) are retained
  x[x < 1 | x > 17] <- NA

  # Convert to labelled factor
  x <- factor(x, levels = c(-9, -8, -7, -3, -2, -1, 1:17), labels = nssec_labels)

  return(x)
}

# Function to process each wave
process_wave <- function(file_path, wave_num) {
  data <- read_delim(file_path, delim = "\t")

  # Select NSID and the correct NS-SEC variables for each wave
  if (wave_num == 1) {
    data <- data %>%
      select(NSID, W1nsseccatmum, W1nsseccatdad) %>%
      rename(nssecma14 = W1nsseccatmum, nssecpa14 = W1nsseccatdad)
  } else if (wave_num == 2) {
    data <- data %>%
      select(NSID, W2nsseccatmum, W2nsseccatdad) %>%
      rename(nssecma15 = W2nsseccatmum, nssecpa15 = W2nsseccatdad)
  } else if (wave_num == 3) {
    data <- data %>%
      select(NSID, W3cnsseccatmum, W3cnsseccatdad) %>%
      rename(nssecma16 = W3cnsseccatmum, nssecpa16 = W3cnsseccatdad)
  } else if (wave_num == 4) {
    data <- data %>%
      select(NSID, w4cnsseccatmum, w4cnsseccatdad) %>%
      rename(nssecma17 = w4cnsseccatmum, nssecpa17 = w4cnsseccatdad)
  } else if (wave_num == 5) {
    data <- data %>%
      select(NSID, w5Cnsseccatmum, w5Cnsseccatdad) %>%
      rename(nssecma18 = w5Cnsseccatmum, nssecpa18 = w5Cnsseccatdad)
  }

  # Recode NS-SEC variables
  data <- data %>%
    mutate(across(matches("nssec.*"), recode_nssec))

  return(data)
}

# Process each wave
wave_one_data <- process_wave(file_paths$wave_one, 1)
wave_two_data <- process_wave(file_paths$wave_two, 2)
wave_three_data <- process_wave(file_paths$wave_three, 3)
wave_four_data <- process_wave(file_paths$wave_four, 4)
wave_five_data <- process_wave(file_paths$wave_five, 5)

# Merge all datasets
merged_data <- full_join(wave_one_data, wave_two_data, by = "NSID")
merged_data <- full_join(merged_data, wave_three_data, by = "NSID")
merged_data <- full_join(merged_data, wave_four_data, by = "NSID")
merged_data <- full_join(merged_data, wave_five_data, by = "NSID")

# Select only required variables
final_data <- merged_data %>%
  select(NSID, starts_with("nssec"))

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")

# Print confirmation
message("Data cleaning and harmonization complete. Output saved to data/output/cleaned_data.csv")
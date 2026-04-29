library(haven)
library(dplyr)
library(purrr)
library(readr)

# Define file paths and names
file_info <- list(
  wave_one = "data/input/wave_one_lsype_family_background_2020.tab",
  wave_two = "data/input/wave_two_lsype_family_background_2020.tab",
  wave_three = "data/input/wave_three_lsype_family_background_2020.tab",
  wave_four = "data/input/wave_four_lsype_family_background_2020.tab",
  wave_five = "data/input/wave_five_lsype_family_background_2020.tab"
)

# Load and process each wave
load_wave <- function(file_path, wave_name) {
  data <- read_delim(file_path, delim = "\t")

  # Select and rename variables based on wave
  if (wave_name == "wave_one") {
    data <- data %>%
      select(NSID, W1nsseccatmum, W1nsseccatdad) %>%
      rename(nssecma14 = W1nsseccatmum, nssecpa14 = W1nsseccatdad)
  } else if (wave_name == "wave_two") {
    data <- data %>%
      select(NSID, W2nsseccatmum, W2nsseccatdad) %>%
      rename(nssecma15 = W2nsseccatmum, nssecpa15 = W2nsseccatdad)
  } else if (wave_name == "wave_three") {
    data <- data %>%
      select(NSID, W3cnsseccatmum, W3cnsseccatdad) %>%
      rename(nssecma16 = W3cnsseccatmum, nssecpa16 = W3cnsseccatdad)
  } else if (wave_name == "wave_four") {
    data <- data %>%
      select(NSID, w4cnsseccatmum, w4cnsseccatdad) %>%
      rename(nssecma17 = w4cnsseccatmum, nssecpa17 = w4cnsseccatdad)
  } else if (wave_name == "wave_five") {
    data <- data %>%
      select(NSID, w5Cnsseccatmum, w5Cnsseccatdad) %>%
      rename(nssecma18 = w5Cnsseccatmum, nssecpa18 = w5Cnsseccatdad)
  }

  # Harmonize missing values
  data <- data %>%
    mutate(
      across(
        matches("nssec"), ~
          case_when(
            .x == -999 ~ -2,
            .x == -94 ~ -8,
            .x == -99 | .x == -98 | is.na(.x) ~ -3,
            TRUE ~ .x
          )
      )
    )

  # Harmonize NS-SEC categories
  data <- data %>%
    mutate(
      across(
        matches("nssec"), ~
          ifelse(
            .x >= 1 & .x <= 17,
            as.integer(as.character(.x)),
            NA_integer_
          )
      )
    )

  return(data)
}

# Load all waves using explicit names
wave_data <- list(
  wave_one = load_wave(file_info$wave_one, "wave_one"),
  wave_two = load_wave(file_info$wave_two, "wave_two"),
  wave_three = load_wave(file_info$wave_three, "wave_three"),
  wave_four = load_wave(file_info$wave_four, "wave_four"),
  wave_five = load_wave(file_info$wave_five, "wave_five")
)

# Combine all waves
combined_data <- reduce(wave_data, full_join, by = "NSID")

# Select only required variables
final_data <- combined_data %>%
  select(NSID, starts_with("nssec"))

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")

# Print confirmation
message("Data cleaning and harmonization complete. Output written to data/output/cleaned_data.csv")
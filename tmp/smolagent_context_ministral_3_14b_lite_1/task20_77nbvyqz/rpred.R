
# Load required libraries
library(readr)
library(dplyr)

# Define paths and filenames
files <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_two = "data/input/wave_two_lsype_young_person_2020.tab",
  wave_three = "data/input/wave_three_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "data/input/wave_seven_lsype_young_person_2020.tab",
  ns8 = "data/input/ns8_2015_self_completion.tab",
  ns9 = "data/input/ns9_2022_main_interview.tab"
)

# Load all files
wave_one_data <- read_delim(files$wave_one, delim = "\t")
wave_two_data <- read_delim(files$wave_two, delim = "\t")
wave_three_data <- read_delim(files$wave_three, delim = "\t")
wave_four_data <- read_delim(files$wave_four, delim = "\t")
wave_six_data <- read_delim(files$wave_six, delim = "\t")
wave_seven_data <- read_delim(files$wave_seven, delim = "\t")
ns8_data <- read_delim(files$ns8, delim = "\t")
ns9_data <- read_delim(files$ns9, delim = "\t")

# Merge all datasets by NSID
merged_data <- wave_one_data %>%
  full_join(wave_two_data, by = "NSID") %>%
  full_join(wave_three_data, by = "NSID") %>%
  full_join(wave_four_data, by = "NSID") %>%
  full_join(wave_six_data, by = "NSID") %>%
  full_join(wave_seven_data, by = "NSID") %>%
  full_join(ns8_data, by = "NSID") %>%
  full_join(ns9_data, by = "NSID")

# Define the age mapping
age_mapping <- tibble(
  wave = c("W1", "W2", "W3", "W4", "W6", "W7", "W8", "W9"),
  age = c(14, 15, 16, 17, 19, 20, 25, 32)
)

# Function to determine the earliest age of alcohol consumption
get_alcfst <- function(row) {
  for (i in seq_along(age_mapping$wave)) {
    wave <- age_mapping$wave[i]
    age <- age_mapping$age[i]

    # Check for W1-W7 (alceverYP)
    alcever_var <- paste0(wave, "alceverYP")
    if (alcever_var %in% names(row)) {
      response <- row[[alcever_var]]
      if (!is.na(response) && response == 1) {
        return(age)
      }
    }

    # Check for W8 and W9 (AUDIT1)
    if (wave %in% c("W8", "W9")) {
      audit_var <- paste0(wave, "AUDIT1")
      if (audit_var %in% names(row)) {
        response <- row[[audit_var]]
        if (!is.na(response) && response != 1) {  # Not 'Never'
          return(age)
        }
      }
    }
  }
  # If no 'Yes' found, return 99 (never drank alcohol)
  return(99)
}

# Apply get_alcfst to each row using a loop
merged_data$alcfst <- numeric(nrow(merged_data))
for (i in 1:nrow(merged_data)) {
  merged_data$alcfst[i] <- get_alcfst(merged_data[i, ])
}

# Select only NSID and alcfst for the output
output_data <- merged_data %>% select(NSID, alcfst)

# Write output to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

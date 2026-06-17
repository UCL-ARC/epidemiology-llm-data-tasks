library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define value labels for income bands
value_labels <- c(
  "1.0" = "less than 25",
  "2.0" = "25 to 50",
  "3.0" = "50 to 90",
  "4.0" = "90 to 140",
  "5.0" = "140 to 240",
  "6.0" = "240 to 300",
  "7.0" = "300 to 350",
  "8.0" = "350 to 400",
  "9.0" = "400 to 500",
  "10.0" = "500 to 600",
  "11.0" = "600 to 700",
  "12.0" = "700 to 800",
  "13.0" = "800 to 900",
  "14.0" = "900 to 1200",
  "15.0" = "1200 to 1400",
  "16.0" = "more than 1400"
)

# Function to harmonize missing values
harmonize_missing <- function(var) {
  case_when(
    var == -1.0 ~ -1,
    is.na(var) ~ -3,
    TRUE ~ var
  )
}

# Derive inc25 from W8DINCB
inc25 <- merged_data %>%
  mutate(
    inc25 = harmonize_missing(W8DINCB)
  ) %>%
  select(NSID, inc25)

# Derive inc32 from W9DINCB
inc32 <- merged_data %>%
  mutate(
    inc32 = harmonize_missing(W9DINCB)
  ) %>%
  select(NSID, inc32)

# Combine the derived variables
cleaned_data <- inc25 %>%
  full_join(inc32, by = "NSID")

# Convert inc25 and inc32 to labelled factors
cleaned_data$inc25 <- factor(cleaned_data$inc25, levels = c(1:16, -1, -3), labels = c(value_labels, "Not applicable", "Not interviewed"))
cleaned_data$inc32 <- factor(cleaned_data$inc32, levels = c(1:16, -1, -3), labels = c(value_labels, "Not applicable", "Not interviewed"))

# Write the cleaned data to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")
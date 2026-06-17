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

# Merge datasets using NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define the value labels for income bands
income_labels <- c(
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

# Map missing values for W8DINCB and W9DINCB
# For W8DINCB and W9DINCB, -1.0 is "Not applicable"
merged_data <- merged_data %>%
  mutate(
    inc25 = case_when(
      W8DINCB == -1.0 ~ -1,  # Not applicable
      W8DINCB %in% as.numeric(names(income_labels)) ~ W8DINCB,
      is.na(W8DINCB) ~ -3,  # Not interviewed
      TRUE ~ -3  # Default to not interviewed
    ),
    inc32 = case_when(
      W9DINCB == -1.0 ~ -1,  # Not applicable
      W9DINCB %in% as.numeric(names(income_labels)) ~ W9DINCB,
      is.na(W9DINCB) ~ -3,  # Not interviewed
      TRUE ~ -3  # Default to not interviewed
    )
  )

# Convert inc25 and inc32 to labelled factors
inc25_labels <- c(
  "-1" = "Not applicable",
  income_labels
)
inc32_labels <- c(
  "-1" = "Not applicable",
  income_labels
)

merged_data$inc25 <- factor(merged_data$inc25, levels = c(-1, as.numeric(names(income_labels))), labels = inc25_labels)
merged_data$inc32 <- factor(merged_data$inc32, levels = c(-1, as.numeric(names(income_labels))), labels = inc32_labels)

# Select only NSID and derived variables
output_data <- merged_data %>%
  select(NSID, inc25, inc32)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"
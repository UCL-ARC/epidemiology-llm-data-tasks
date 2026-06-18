library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load each file explicitly by name
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Define the value labels for the income bands
income_labels <- c(
  "less than 25",
  "25 to 50",
  "50 to 90",
  "90 to 140",
  "140 to 240",
  "240 to 300",
  "300 to 350",
  "350 to 400",
  "400 to 500",
  "500 to 600",
  "600 to 700",
  "700 to 800",
  "800 to 900",
  "900 to 1200",
  "1200 to 1400",
  "more than 1400"
)

# Create the inc25 variable from W8DINCB
inc25 <- merged_data$W8DINCB
inc25[inc25 == -1] <- -1  # Not applicable
inc25 <- labelled::to_factor(inc25, labels = c("-1" = "Not applicable", income_labels))

# Create the inc32 variable from W9DINCB
inc32 <- merged_data$W9DINCB
inc32[inc32 == -1] <- -1  # Not applicable
inc32 <- labelled::to_factor(inc32, labels = c("-1" = "Not applicable", income_labels))

# Combine the ID variable and the derived variables into the final dataset
final_data <- data.frame(
  NSID = merged_data$NSID,
  inc25 = inc25,
  inc32 = inc32
)

# Ensure the output directory exists
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Write the final dataset to CSV
write_csv(final_data, "data/output/cleaned_data.csv")

# Print confirmation
print("File written successfully.")
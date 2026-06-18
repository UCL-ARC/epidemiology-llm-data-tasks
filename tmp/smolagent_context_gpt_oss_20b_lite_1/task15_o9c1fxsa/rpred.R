# Load required packages
library(readr)
library(dplyr)
library(labelled)

# Define file paths
base_path <- "data/input/"

# Read files
wave1 <- read_delim(paste0(base_path, "wave_one_lsype_young_person_2020.tab"), delim = "\t", col_types = cols())
wave4 <- read_delim(paste0(base_path, "wave_four_lsype_young_person_2020.tab"), delim = "\t", col_types = cols())
ns8  <- read_delim(paste0(base_path, "ns8_2015_derived.tab"), delim = "\t", col_types = cols())
ns9  <- read_delim(paste0(base_path, "ns9_2022_derived_variables.tab"), delim = "\t", col_types = cols())

# Merge all datasets by NSID
merged <- full_join(wave1, wave4, by = "NSID") %>%
          full_join(ns8, by = "NSID") %>%
          full_join(ns9, by = "NSID")

# Function to harmonise income bands and assign labels
harmonise_income <- function(raw){
  # Replace source Not applicable code -1.0 with -1
  raw_clean <- raw
  raw_clean[raw_clean == -1.0] <- -1
  # Replace NA with -3 (Not asked)
  raw_clean[is.na(raw_clean)] <- -3
  # Labels vector: names are labels, values are codes
  labels_vec <- c(
    `Not applicable` = -1,
    `Not asked` = -3,
    `less than 25` = 1,
    `25 to 50` = 2,
    `50 to 90` = 3,
    `90 to 140` = 4,
    `140 to 240` = 5,
    `240 to 300` = 6,
    `300 to 350` = 7,
    `350 to 400` = 8,
    `400 to 500` = 9,
    `500 to 600` = 10,
    `600 to 700` = 11,
    `700 to 800` = 12,
    `800 to 900` = 13,
    `900 to 1200` = 14,
    `1200 to 1400` = 15,
    `more than 1400` = 16
  )
  # Assign labels using labelled()
  labelled(raw_clean, labels_vec)
}

# Create inc25 and inc32 using harmonise_income
merged <- merged %>%
  mutate(
    inc25 = harmonise_income(W8DINCB),
    inc32 = harmonise_income(W9DINCB)
  )

# Select only the ID and final variables
final_df <- merged %>% select(NSID, inc25, inc32)

# Write output CSV
write_csv(final_df, "data/output/cleaned_data.csv")

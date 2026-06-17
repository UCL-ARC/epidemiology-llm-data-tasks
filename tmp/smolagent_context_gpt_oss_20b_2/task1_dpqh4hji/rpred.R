# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Define paths
input_path <- "data/input/"
output_path <- "data/output/"

# List of metadata file names
file_names <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab",
  "wave_three_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_five_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_main_interview.tab",
  "ns9_2022_main_interview.tab"
)

# Function to load a single file
load_file <- function(fname) {
  path <- file.path(input_path, fname)
  read_delim(path, delim = "\t", col_types = cols())
}

# Load all datasets into a list
data_list <- map(file_names, load_file)
names(data_list) <- file_names

# Merge all datasets by NSID using full_join
full_df <- reduce(data_list, full_join, by = "NSID")

# Helper to clean sex columns: replace negative values with NA
clean_sex_col <- function(vec) {
  ifelse(vec < 0, NA_real_, vec)
}

# Clean each sex variable
full_df <- full_df %>%
  mutate(
    W1sexYP_clean = clean_sex_col(W1sexYP),
    W2SexYP_clean = clean_sex_col(W2SexYP),
    W3sexYP_clean = clean_sex_col(W3sexYP),
    W4SexYP_clean = clean_sex_col(W4SexYP),
    W5SexYP_clean = clean_sex_col(W5SexYP),
    W6Sex_clean = clean_sex_col(W6Sex),
    W7Sex_clean = clean_sex_col(W7Sex),
    W8CMSEX_clean = clean_sex_col(W8CMSEX),
    W9DSEX_clean = clean_sex_col(W9DSEX)
  )

# Derive consolidated sex variable following rule: most recent valid first, then earlier sweeps from earliest to most recent
full_df <- full_df %>%
  mutate(
    sex = coalesce(
      W9DSEX_clean,
      W8CMSEX_clean,
      W7Sex_clean,
      W6Sex_clean,
      W5SexYP_clean,
      W4SexYP_clean,
      W3sexYP_clean,
      W2SexYP_clean,
      W1sexYP_clean
    )
  )

# Replace NAs with standard missing code -3
full_df <- full_df %>%
  mutate(sex = ifelse(is.na(sex), -3, sex))

# Keep only NSID and final sex variable
output_df <- full_df %>% select(NSID, sex)

# Write to CSV
write_csv(output_df, file = file.path(output_path, "cleaned_data.csv"))

# Print a message
cat("Cleaning complete. Output written to", file.path(output_path, "cleaned_data.csv"), "\n")

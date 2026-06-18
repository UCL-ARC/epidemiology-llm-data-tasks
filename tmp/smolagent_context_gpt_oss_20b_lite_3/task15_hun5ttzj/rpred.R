library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# Helper to read a file and ensure NSID column is named exactly 'NSID'
read_and_rename <- function(file) {
  df <- read_delim(file, delim = "\t", col_names = TRUE, col_types = cols(.default = col_character()), progress = FALSE)
  # Find column that contains 'NSID' (case-insensitive)
  nsid_col <- grep("NSID", names(df), ignore.case = TRUE)
  if(length(nsid_col) == 0) {
    stop("NSID column not found in ", file)
  }
  # If not already 'NSID', rename
  if(names(df)[nsid_col] != "NSID") {
    df <- df %>% rename(NSID = !!sym(names(df)[nsid_col]))
  }
  # Ensure NSID is character
  df <- df %>% mutate(NSID = as.character(NSID))
  df
}

# Read all files
wave_one <- read_and_rename("data/input/wave_one_lsype_young_person_2020.tab")
wave_four <- read_and_rename("data/input/wave_four_lsype_young_person_2020.tab")
ns8 <- read_and_rename("data/input/ns8_2015_derived.tab")
ns9 <- read_and_rename("data/input/ns9_2022_derived_variables.tab")

# Merge on NSID
merged_df <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Helper to harmonise income band missing codes
clean_income <- function(x) {
  x_clean <- ifelse(is.na(x), -3, x)
  # Convert Not applicable (-1.0 or -1) to -1
  x_clean <- ifelse(x_clean == -1.0 | x_clean == -1, -1, x_clean)
  as.integer(x_clean)
}

# Extract income variables, coerce to numeric, clean, and keep only final columns
cleaned_df <- merged_df %>%
  select(NSID, W8DINCB, W9DINCB) %>%
  mutate(
    inc25 = clean_income(as.numeric(W8DINCB)),
    inc32 = clean_income(as.numeric(W9DINCB))
  ) %>%
  select(NSID, inc25, inc32)

# Ensure output directory exists
output_dir <- "data/output"
if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# Write to CSV
write_csv(cleaned_df, file.path(output_dir, "cleaned_data.csv"))

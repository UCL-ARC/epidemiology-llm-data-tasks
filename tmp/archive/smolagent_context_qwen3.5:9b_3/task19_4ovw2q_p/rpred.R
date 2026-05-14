library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Set the paths
input_dir <- "data/input/"
output_dir <- "data/output/"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Remove existing output file if it exists
if (file.exists(paste0(output_dir, "cleaned_data.csv"))) {
  file.remove(paste0(output_dir, "cleaned_data.csv"))
}

# Load the files - specify column types properly
wave1 <- read_delim(paste0(input_dir, "wave_one_lsype_young_person_2020.tab"), 
                    delim = "\t", 
                    col_types = cols(NSID = col_character()))
wave4 <- read_delim(paste0(input_dir, "wave_four_lsype_young_person_2020.tab"), 
                    delim = "\t", 
                    col_types = cols(NSID = col_character()))
ns8 <- read_delim(paste0(input_dir, "ns8_2015_derived.tab"), 
                  delim = "\t", 
                  col_types = cols(NSID = col_character(), W8DBMI = col_double()))
ns9 <- read_delim(paste0(input_dir, "ns9_2022_derived_variables.tab"), 
                  delim = "\t", 
                  col_types = cols(NSID = col_character(), W9DBMI = col_double()))

# Merge the files by NSID
combined <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Recode missing values to standard codes
combined <- combined %>%
  mutate(
    bmi25 = case_when(
      is.na(W8DBMI) ~ -3,
      W8DBMI == -9 ~ -9,
      W8DBMI == -8 ~ -8,
      W8DBMI == -1 ~ -1,
      TRUE ~ W8DBMI
    ),
    bmi32 = case_when(
      is.na(W9DBMI) ~ -3,
      W9DBMI == -9 ~ -9,
      W9DBMI == -8 ~ -8,
      W9DBMI == -1 ~ -1,
      TRUE ~ W9DBMI
    )
  )

# Output only NSID, bmi25, bmi32 to CSV
output_data <- combined %>% select(NSID, bmi25, bmi32)
write_delim(output_data, paste0(output_dir, "cleaned_data.csv"), delim = ",")

cat("Script completed successfully!\n")
cat("Output written to", output_dir, "cleaned_data.csv\n")
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Directory paths
input_dir <- "data/input/"
output_dir <- "data/output/"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# List all tab files in input directory
tab_files <- list.files(input_dir, pattern = "\\.tab$", full.names = TRUE)
cat("Found files:", paste(tab_files, collapse = ", "), "\n")

# Define wave-specific missing value mappings
# Format: standard_code = list(wave_code -> standard_code)
missing_value_codes <- list(
  wave_one = list(
    "Refused" = -92,
    "Don't know" = -91,
    "Not applicable" = -91,
    "YP not interviewed" = -99,
    "Not asked" = -999
  ),
  wave_two = list(
    "Refused" = -92,
    "Don't know" = -1,
    "Not applicable" = -91,
    "YP not interviewed" = -99,
    "Not asked" = -999,
    "Interviewer missed question" = -998,
    "Script error" = -997
  ),
  wave_three = list(
    "Refused" = -92,
    "Don't know" = -91,
    "Not applicable" = -91,
    "YP not interviewed" = -99
  ),
  wave_four = list(
    "Refused" = -92,
    "Don't know" = -1,
    "Not applicable" = -91,
    "YP not interviewed" = -99
  ),
  wave_five = list(
    "Refused" = -9,
    "Don't know" = -1,
    "Not applicable" = -1
  ),
  wave_six = list(
    "Refused" = -92,
    "Not applicable" = -91
  ),
  wave_seven = list(
    "Not applicable" = -91
  ),
  wave_eight = list(
    "Refused" = -9,
    "Don't know" = -8,
    "Not applicable" = -1
  ),
  wave_nine = list(
    "Not applicable" = -99
  )
)

# Load all data files
users <- list()
for (f in tab_files) {
  f_name <- basename(f)
  cat("Loading", f_name, "...\n")
  users[[f_name]] <- read_delim(f, delim = "\t", col_types = cols(.default = "c"))
}

# Merge all data by NSID
all_data <- users[[1]]
for (i in 2:length(users)) {
  all_data <- full_join(all_data, users[[i]], by = "NSID")
}

cat("All data loaded and merged.\n")

cat("Done!\n")

# Output the cleaned data
write_csv(all_data, "data/output/cleaned_data.csv")
cat("Output saved to data/output/cleaned_data.csv\n")
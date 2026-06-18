# Load required packages
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(haven)
library(labelled)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE)

# Define file paths
files <- list(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_derived_variables.tab"
)

# Load all files
file_list <- purrr::map(files, ~ read_delim(paste0("data/input/", .x), delim = "\t", show_col_types = FALSE))
names(file_list) <- files

# Merge all files by NSID
df <- file_list[[1]]
for (i in 2:length(file_list)) {
  df <- full_join(df, file_list[[i]], by = "NSID")
}

cat("Loaded data dimensions:", dim(df), "\n")

# --- Derive harmonised marital status variables ---

# Function to map W6MarStatYP to harmonised categories
map_w6 <- function(x) {
  result <- rep(-3, length(x))  # default: Not asked
  
  # Map valid codes
  valid_idx <- !is.na(x) & x >= 1 & x <= 5
  result[valid_idx] <- x[valid_idx]
  
  # Map missing codes
  result[x == -997] <- -2       # Script error
  result[x == -97] <- -8        # Respondent declined self completion
  result[x == -92] <- -9        # Refused
  result[x == -91] <- -1        # Not applicable
  result[x == -1] <- -8         # Don't know
  result[x == -999] <- -2       # Schedule not applicable
  
  result
}

# Function to map W8DMARSTAT to harmonised categories
map_w8 <- function(x) {
  result <- rep(-3, length(x))  # default: Not asked
  
  # Map to standard missing codes
  result[x == -9] <- -9         # Refused
  result[x == -8] <- -8         # Insufficient information
  result[x == -1] <- -1         # Not applicable
  
  # Map substantive categories to harmonised scheme
  valid <- !is.na(x) & x >= 1 & x <= 5
  result[valid] <- x[valid]
  
  result[x == 6] <- 2  # Civil Partner -> Married
  result[x == 7] <- 3  # Separated in CP -> Separated
  result[x == 8] <- 4  # Former CP -> Divorced
  result[x == 9] <- 5  # Surviving CP -> Widowed
  
  result
}

# Function to map W9DMARSTAT to harmonised categories
map_w9 <- function(x) {
  result <- rep(-3, length(x))  # default: Not asked
  
  # Map to standard missing codes
  result[x == -9] <- -9         # Refused
  result[x == -8] <- -8         # Insufficient information
  
  # Map substantive categories to harmonised scheme
  valid <- !is.na(x) & x >= 1 & x <= 5
  result[valid] <- x[valid]
  
  result[x == 6] <- 2  # Civil Partner -> Married
  result[x == 7] <- 4  # Former CP -> Divorced
  result[x == 8] <- 5  # Surviving CP -> Widowed
  
  result
}

# Apply mappings
df$partnr19 <- map_w6(df$W6MarStatYP)
df$partnr25 <- map_w8(df$W8DMARSTAT)
df$partnr32 <- map_w9(df$W9DMARSTAT)

# --- Derive detailed adult partnership variables ---

# partnradu25: Keep detailed categories from W8DMARSTAT
df$partnradu25 <- df$W8DMARSTAT
# Convert NA to -3 (Not asked/Not interviewed)
df$partnradu25[is.na(df$partnradu25)] <- -3

# partnradu32: Keep detailed categories from W9DMARSTAT
df$partnradu32 <- df$W9DMARSTAT
# Convert NA to -3 (Not asked/Not interviewed)
df$partnradu32[is.na(df$partnradu32)] <- -3

# --- Select only ID and final derived variables ---
out_df <- df %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# --- Write output ---
write_csv(out_df, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Output dimensions:", dim(out_df), "\n")
cat("Variables:", names(out_df), "\n")

# Show summary of output
print(summary(out_df))
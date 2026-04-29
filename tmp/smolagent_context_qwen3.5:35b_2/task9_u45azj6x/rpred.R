# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "wave_one_lsype_family_background_2020.tab",
  "wave_two_lsype_family_background_2020.tab",
  "wave_four_lsype_family_background_2020.tab"
)

# Load all files
data_list <- map(files, ~ read_delim(paste0("data/input/", .x), delim = "\t", show_col_types = FALSE))

# Assign names to data frames
names(data_list) <- files

# Merge all data using full_join by NSID
merged_data <- reduce(data_list, full_join, by = "NSID")

# Check column names
cat("Column names in merged_data:\n")
cat(names(merged_data), "\n")

# Define detailed education value labels
detailed_labels <- c(
  "1" = "Higher Degree",
  "2" = "First Degree",
  "3" = "HE Diploma",
  "4" = "HNC/HND/NVQ4",
  "5" = "Teaching qualification, non-degree",
  "6" = "Nursing qualification, non-degree",
  "7" = "A Levels",
  "8" = "OND/ONC",
  "9" = "City and guilds part III, NVQ3",
  "10" = "CSYS",
  "11" = "Scottish Higher Grade",
  "12" = "AS Level",
  "13" = "Trade apprenticeship",
  "14" = "City and guilds part II, NVQ2",
  "15" = "GCSE grade A-C and equivalent",
  "16" = "GCSE grade D-E and equivalent",
  "17" = "City and guilds part I, NVQ1",
  "18" = "Youth training, skill seekers",
  "19" = "Qualification, level unspecified",
  "20" = "No qualification mentioned",
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-3" = "Not asked at fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-1" = "Item not applicable"
)

# Define collapsed education mapping
collapsed_map <- c(
  "1" = "0",
  "2" = "0",
  "3" = "0",
  "4" = "0",
  "5" = "1",
  "6" = "1",
  "7" = "1",
  "8" = "1",
  "9" = "1",
  "10" = "1",
  "11" = "1",
  "12" = "1",
  "13" = "1",
  "14" = "1",
  "15" = "1",
  "16" = "1",
  "17" = "1",
  "18" = "2",
  "19" = "3",
  "20" = "4"
)

# Define collapsed labels
collapsed_labels <- c(
  "0" = "NVQ 4-5: degree-level qualifications and above",
  "1" = "NVQ 1-3: sub-degree qualifications",
  "2" = "None/entry: training programmes below NVQ level",
  "3" = "Other: unspecified or undetermined qualifications",
  "4" = "No qualifications mentioned",
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-3" = "Not asked at fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-1" = "Item not applicable"
)

# Function to consolidate education variables
consolidate_education <- function(w1_val, w2_val, w4_val) {
  result <- numeric(length(w1_val))
  
  for (i in 1:length(w1_val)) {
    if (!is.na(w1_val[i]) && w1_val[i] >= 1 && w1_val[i] <= 20) {
      result[i] <- w1_val[i]
    } else if (!is.na(w2_val[i]) && w2_val[i] >= 1 && w2_val[i] <= 20) {
      result[i] <- w2_val[i]
    } else if (!is.na(w4_val[i]) && w4_val[i] >= 1 && w4_val[i] <= 20) {
      result[i] <- w4_val[i]
    } else if (!is.na(w1_val[i])) {
      result[i] <- w1_val[i]
    } else if (!is.na(w2_val[i])) {
      result[i] <- w2_val[i]
    } else if (!is.na(w4_val[i])) {
      result[i] <- w4_val[i]
    } else {
      result[i] <- -3
    }
  }
  
  return(result)
}

# Consolidate maternal education
merged_data$educdtlma <- consolidate_education(
  merged_data$W1hiqualmum, merged_data$W2hiqualmum, merged_data$w4hiqualmum
)

# Consolidate paternal education
merged_data$educdtlpa <- consolidate_education(
  merged_data$W1hiqualdad, merged_data$W2hiqualdad, merged_data$w4hiqualdad
)

# Create collapsed variables from consolidated detailed variables
collapse_education <- function(x, collapsed_map) {
  collapsed <- ifelse(
    x >= 1 & x <= 20,
    as.numeric(collapsed_map[as.character(x)]),
    x
  )
  
  return(collapsed)
}

# Create collapsed maternal education
merged_data$educma <- collapse_education(
  merged_data$educdtlma,
  collapsed_map
)

# Create collapsed paternal education
merged_data$educpa <- collapse_education(
  merged_data$educdtlpa,
  collapsed_map
)

# Check if columns exist
cat("Columns after consolidation:\n")
cat(names(merged_data), "\n")

# Select only the required variables
output_data <- merged_data %>%
  select(NSID, educma, educpa, educdtlma, educdtlpa)

# Ensure output directory exists
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Write to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Print summary
cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(output_data), "\n")
cat("Variables:", names(output_data), "\n")
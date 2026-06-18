# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Set working directory if needed (optional)
# setwd("data/input")

# Step 1: Load the datasets using base R functions
cat("Loading wave1 data...")
wave1 <- read.table("data/input/wave_one_lsype_family_background_2020.tab", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
cat("Loading wave2 data...")
wave2 <- read.table("data/input/wave_two_lsype_family_background_2020.tab", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
cat("Loading wave4 data...")
wave4 <- read.table("data/input/wave_four_lsype_family_background_2020.tab", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Convert NSID to character explicitly
wave1$NSID <- as.character(wave1$NSID)
wave2$NSID <- as.character(wave2$NSID)
wave4$NSID <- as.character(wave4$NSID)

# Step 2: Merge datasets by NSID
cat("Merging datasets...")
merged_data <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Check if required variables exist
required_vars <- c("W1hiqualmum", "W2hiqualmum", "w4hiqualmum", "W1hiqualdad", "W2hiqualdad", "w4hiqualdad")
missing_vars <- setdiff(required_vars, names(merged_data))
if (length(missing_vars) > 0) {
  stop(paste("Missing required variables:", paste(missing_vars, collapse = ", ")))
}

# Step 3: Define missing value mapping function
harmonize_missing <- function(x) {
  x <- ifelse(is.na(x), -3, x)

  # Map wave-specific missing values to standard codes
  x <- case_when(
    x == -999 ~ -2,
    x == -99  ~ -2,
    x == -98  ~ -2,
    x == -94  ~ -8,
    x == -92  ~ -9,
    x == -91  ~ -1,
    x == -1   ~ -8,
    TRUE      ~ x
  )
  return(x)
}

# Step 4: Harmonize missing values for mother and father education variables
mother_vars <- c("W1hiqualmum", "W2hiqualmum", "w4hiqualmum")
father_vars <- c("W1hiqualdad", "W2hiqualdad", "w4hiqualdad")

cat("Harmonizing missing values...")
for (var in mother_vars) {
  merged_data[[var]] <- harmonize_missing(merged_data[[var]])
}

for (var in father_vars) {
  merged_data[[var]] <- harmonize_missing(merged_data[[var]])
}

# Step 5: Consolidate education variables
consolidate_education <- function(data, vars) {
  consolidated <- rep(-3, nrow(data))
  for (wave_var in vars) {
    consolidated <- ifelse(is.na(consolidated) & !is.na(data[[wave_var]]),
                          data[[wave_var]], consolidated)
  }
  return(consolidated)
}

# Consolidate mother and father education variables
cat("Consolidating education variables...")
merged_data$educdtlma <- consolidate_education(merged_data, mother_vars)
merged_data$educdtlpa <- consolidate_education(merged_data, father_vars)

# Step 6: Define NVQ mapping for collapsed variables
nvq_mapping <- function(detailed_educ) {
  case_when(
    detailed_educ %in% c(1, 2, 3, 4) ~ 0,
    detailed_educ %in% c(5:17) ~ 1,
    detailed_educ == 18 ~ 2,
    detailed_educ == 19 ~ 3,
    detailed_educ == 20 ~ 4,
    TRUE ~ -3
  )
}

# Step 7: Create collapsed NVQ variables
cat("Creating collapsed NVQ variables...")
merged_data$educma <- nvq_mapping(merged_data$educdtlma)
merged_data$educpa <- nvq_mapping(merged_data$educdtlpa)

# Step 8: Define value labels
value_labels_detailed <- structure(
  c("Refusal", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Item not applicable",
    "Higher Degree", "First Degree", "HE Diploma", "HNC/HND/NVQ4", "Teaching qualification", "Nursing qualification",
    "A Levels", "OND/ONC", "City and Guilds Part III", "CSYS", "Scottish Higher Grade", "AS Level",
    "Trade apprenticeship", "City and Guilds Part II", "GCSE A-C", "GCSE D-E", "City and Guilds Part I",
    "Youth training", "Qualification unspecified", "No qualification"),
  names = c(-9, -8, -7, -3, -2, -1, 1:20)
)

value_labels_collapsed <- structure(
  c("Not asked", "NVQ 4-5", "NVQ 1-3", "Youth training", "Qualification unspecified", "No qualification"),
  names = c(-3, 0, 1, 2, 3, 4)
)

# Step 9: Apply labels to variables
cat("Applying value labels...")
merged_data$educdtlma <- factor(merged_data$educdtlma,
                               levels = names(value_labels_detailed),
                               labels = value_labels_detailed)
merged_data$educdtlpa <- factor(merged_data$educdtlpa,
                               levels = names(value_labels_detailed),
                               labels = value_labels_detailed)
merged_data$educma <- factor(merged_data$educma,
                            levels = names(value_labels_collapsed),
                            labels = value_labels_collapsed)
merged_data$educpa <- factor(merged_data$educpa,
                            levels = names(value_labels_collapsed),
                            labels = value_labels_collapsed)

# Step 10: Select only the ID and final derived variables
cat("Selecting final variables...")
output_data <- merged_data %>% select(NSID, educdtlma, educdtlpa, educma, educpa)

# Step 11: Write the output to CSV
cat("Writing output file...")
output_path <- "data/output/cleaned_data.csv"
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write.csv(output_data, output_path, row.names = FALSE)

# Verify output
cat("Output file created at:", output_path)
cat("Number of rows in output:", nrow(output_data))
cat("Number of columns in output:", ncol(output_data))
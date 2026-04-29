library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Set paths
input_dir <- "data/input/"
output_file <- "data/output/cleaned_data.csv"

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load the 5 files
wave_one <- read_delim(paste(input_dir, "wave_one_lsype_young_person_2020.tab", sep = ""), delim = "\t")
wave_two <- read_delim(paste(input_dir, "wave_two_lsype_family_background_2020.tab", sep = ""), delim = "\t")
wave_three <- read_delim(paste(input_dir, "wave_three_lsype_family_background_2020.tab", sep = ""), delim = "\t")
wave_four <- read_delim(paste(input_dir, "wave_four_lsype_young_person_2020.tab", sep = ""), delim = "\t")
ns9 <- read_delim(paste(input_dir, "ns9_2022_derived_variables.tab", sep = ""), delim = "\t")

cat("Loaded all files successfully\n")
cat("wave_two dimensions:", nrow(wave_two), ncol(wave_two), "\n")
cat("wave_three dimensions:", nrow(wave_three), ncol(wave_three), "\n")
cat("ns9 dimensions:", nrow(ns9), ncol(ns9), "\n")

# Extract IMD variables
# - wave_two (Age 15): IMDRSCORE -> imd15
# - wave_three (Age 16): IMDRSCORE -> imd16
# - ns9 (Age 32): W9DIMDD -> imd32

wave_two_imd <- wave_two %>% select(NSID, IMDRSCORE) %>% rename(timdr15 = IMDRSCORE)
wave_three_imd <- wave_three %>% select(NSID, IMDRSCORE) %>% rename(timdr16 = IMDRSCORE)
ns9_imd <- ns9 %>% select(NSID, W9DIMDD) %>% rename(timdr32 = W9DIMDD)

cat("Extracted IMD variables\n")
cat("wave_two_imd columns:", names(wave_two_imd), "\n")
cat("wave_three_imd columns:", names(wave_three_imd), "\n")
cat("ns9_imd columns:", names(ns9_imd), "\n")

# Check unique values before recoding
cat("\nBefore recoding - wave_two_imd timdr15 values:\n")
print(table(wave_two_imd$timdr15))
cat("\nBefore recoding - wave_three_imd timdr16 values:\n")
print(table(wave_three_imd$timdr16))
cat("\nBefore recoding - ns9_imd timdr32 values:\n")
print(table(ns9_imd$timdr32))

# Recode missing values to standard codes
# Standard codes: -9=Refusal, -8=Don't know/insufficient information, -3=Not asked, -2=Script error, -1=Not applicable
# Specifically: -94 -> -8, NA -> -3

# Recode missing values in wave_two_imd
wave_two_imd$timdr15[wave_two_imd$timdr15 == -94] <- -8
wave_two_imd$timdr15[is.na(wave_two_imd$timdr15)] <- -3

# Recode missing values in wave_three_imd
wave_three_imd$timdr16[wave_three_imd$timdr16 == -94] <- -8
wave_three_imd$timdr16[is.na(wave_three_imd$timdr16)] <- -3

# Recode missing values in ns9_imd
ns9_imd$timdr32[ns9_imd$timdr32 == -8] <- -8
ns9_imd$timdr32[is.na(ns9_imd$timdr32)] <- -3

cat("\nAfter recoding - wave_two_imd timdr15 values:\n")
print(table(wave_two_imd$timdr15))
cat("\nAfter recoding - wave_three_imd timdr16 values:\n")
print(table(wave_three_imd$timdr16))
cat("\nAfter recoding - ns9_imd timdr32 values:\n")
print(table(ns9_imd$timdr32))

# Merge all datasets by NSID
cleaned_data <- full_join(wave_two_imd, wave_three_imd, by = "NSID")
cleaned_data <- full_join(cleaned_data, ns9_imd, by = "NSID")

cat("\nMerged datasets, dimensions:", nrow(cleaned_data), ncol(cleaned_data), "\n")

# Rename variables to match requirements
cleaned_data <- cleaned_data %>%
  rename(
    imd15 = timdr15,
    imd16 = timdr16,
    imd32 = timdr32
  )

# Keep only the required variables
cleaned_data <- cleaned_data %>% select(NSID, imd15, imd16, imd32)

cat("\nFinal columns:", names(cleaned_data), "\n")

# Apply labels to variables
# Use haven::labelled to create labelled variables
cleaned_data$imd15 <- haven::labelled(cleaned_data$imd15, label = "IMD score at age 15")
cleaned_data$imd16 <- haven::labelled(cleaned_data$imd16, label = "IMD score at age 16")
cleaned_data$imd32 <- haven::labelled(cleaned_data$imd32, label = "IMD score at age 32")

# Set value labels for missing codes
value_labels <- data.frame(
  value = c("-9", "-8", "-3", "-2", "-1"),
  label = c("Refusal", "Insufficient Information", "Not asked", "Script error", "Not applicable")
)

# Convert values to numeric for setting labels
labels_imd <- data.frame(
  value = c(-9, -8, -3, -2, -1),
  label = c("Refusal", "Insufficient Information", "Not asked", "Script error", "Not applicable")
)

# Store the value labels in the attributes
attr(cleaned_data$imd15, "label") <- "IMD score at age 15"
attr(cleaned_data$imd16, "label") <- "IMD score at age 16"
attr(cleaned_data$imd32, "label") <- "IMD score at age 32"

# Convert to plain data frame for CSV output
cleaned_data <- as.data.frame(cleaned_data)
write_csv(cleaned_data, output_file)

cat("\nOutput written to", output_file, "\n")
cat("Rows:", nrow(cleaned_data), "\n")
cat("Columns:", ncol(cleaned_data), "\n")
cat("Variable Summary:\n")
print(summary(cleaned_data))
cat("\nMissing value check:\n")
for (var in c("imd15", "imd16", "imd32")) {
  cat(var, ": ", table(cleaned_data[[var]]), "\n", sep = "")
}

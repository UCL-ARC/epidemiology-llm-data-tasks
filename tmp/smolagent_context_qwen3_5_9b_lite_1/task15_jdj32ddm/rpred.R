library(haven)
library(dplyr)
library(readr)

# Load all four files from data/input/
print("Loading wave_one_lsype_young_person_2020.tab...")
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
print(paste("Loaded wave1:", nrow(wave1), "rows"))
print(paste("wave1 columns:", paste(head(names(wave1)), collapse = ", ")))

print("Loading wave_four_lsype_young_person_2020.tab...")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
print(paste("Loaded wave4:", nrow(wave4), "rows"))

print("Loading ns8_2015_derived.tab...")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
print(paste("Loaded wave8:", nrow(wave8), "rows"))

print("Loading ns9_2022_derived_variables.tab...")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)
print(paste("Loaded wave9:", nrow(wave9), "rows"))

# Check the structure of W8DINCB
print("Checking W8DINCB unique values...")
print(unique(wave8$W8DINCB))

# Check the structure of W9DINCB
print("Checking W9DINCB unique values...")
print(unique(wave9$W9DINCB))

# Merge all datasets by NSID
print("Merging datasets...")
cohort <- full_join(wave1, wave4, by = "NSID")
cohort <- full_join(cohort, wave8, by = "NSID")
cohort <- full_join(cohort, wave9, by = "NSID")

print("Checking cohort dimensions:")
print(dim(cohort))

# Create inc25 from W8DINCB (wave 8 = age 25)
print("Creating inc25 variable...")
cohort$inc25 <- cohort$W8DINCB

# Convert -1.0 to -1 (Item not applicable)
cohort$inc25[cohort$inc25 == -1.0] <- -1

# Convert R NA values to -3 (Not asked at fieldwork stage)
cohort$inc25[is.na(cohort$inc25)] <- -3

# Ensure integer type
cohort$inc25 <- as.integer(cohort$inc25)

print("Checking inc25 values:")
print(table(cohort$inc25))

# Create inc32 from W9DINCB (wave 9 = age 32)
print("Creating inc32 variable...")
cohort$inc32 <- cohort$W9DINCB

# Convert -1.0 to -1 (Item not applicable)
cohort$inc32[cohort$inc32 == -1.0] <- -1

# Convert R NA values to -3 (Not asked at fieldwork stage)
cohort$inc32[is.na(cohort$inc32)] <- -3

# Ensure integer type
cohort$inc32 <- as.integer(cohort$inc32)

print("Checking inc32 values:")
print(table(cohort$inc32))

# Remove the raw source variables - keep only NSID, inc25, inc32
print("Selecting final variables:")
cohort <- cohort %>%
  select(NSID, inc25, inc32)

print("Final columns:")
print(names(cohort))

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Write to CSV
print("Writing to data/output/cleaned_data.csv...")
write_csv(cohort, "data/output/cleaned_data.csv")

print("Done!")
print(paste("Output file written with", nrow(cohort), "rows and", ncol(cohort), "columns"))
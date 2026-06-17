library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(labelled)
library(haven)

# Load all files from metadata
file_list <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_derived_variables.tab"
)

# Load each file
df1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
df9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Print structure of each dataset to understand what we have
cat("=== Wave 1 (Age 14) ===\n")
print(str(df1))
cat("\n=== Wave 4 (Age 17) ===\n")
print(str(df4))
cat("\n=== Wave 8 (Age 25) ===\n")
print(str(df8))
cat("\n=== Wave 9 (Age 32) ===\n")
print(str(df9))

# Check W8DBMI and W9DBMI values
cat("\n=== W8DBMI summary ===\n")
print(summary(df8$W8DBMI))
cat("\n=== W9DBMI summary ===\n")
print(summary(df9$W9DBMI))

# Merge all datasets by NSID using full_join
merged <- full_join(df1, df4, by = "NSID")
merged <- full_join(merged, df8, by = "NSID")
merged <- full_join(merged, df9, by = "NSID")

cat("\n=== Merged data dimensions ===\n")
cat(nrow(merged), "rows", ncol(merged), "columns\n")

# Check for NA values in BMI variables
cat("\n=== BMI variable checks ===\n")
cat("W8DBMI NAs:", sum(is.na(merged$W8DBMI)), "\n")
cat("W9DBMI NAs:", sum(is.na(merged$W9DBMI)), "\n")

# Derive BMI variables for age 25 and age 32
# W8DBMI -> bmi25 (Age 25)
# W9DBMI -> bmi32 (Age 32)

# These are continuous variables - keep as numeric
# Convert any remaining R NAs to -3 (not asked) for missing values
# But first check if there are any negative values that are not missing codes

cat("\n=== W8DBMI unique values (sorted) ===\n")
print(sort(unique(merged$W8DBMI), na.last = TRUE))
cat("\n=== W9DBMI unique values (sorted) ===\n")
print(sort(unique(merged$W9DBMI), na.last = TRUE))

# Create derived BMI variables
# For continuous variables, retain NA for missing (per rule 6 exception)
# But actually, the missing codes are already in the data as negative numbers
# So we just need to rename/copy them

merged <- merged %>%
  mutate(
    bmi25 = W8DBMI,
    bmi32 = W9DBMI
  )

# Check the results
cat("\n=== bmi25 summary ===\n")
print(summary(merged$bmi25))
cat("\n=== bmi32 summary ===\n")
print(summary(merged$bmi32))

# Keep only NSID and final derived variables
final <- merged %>%
  select(NSID, bmi25, bmi32)

cat("\n=== Final data dimensions ===\n")
cat(nrow(final), "rows", ncol(final), "columns\n")

# Write output
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(final, "data/output/cleaned_data.csv")

cat("\n=== Output written successfully ===\n")
print(head(final))
print(tail(final))

# Verify the output
cat("\n=== Verification ===\n")
out_check <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
cat("Rows:", nrow(out_check), "\n")
cat("Columns:", ncol(out_check), "\n")
print(head(out_check))

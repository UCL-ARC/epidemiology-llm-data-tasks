library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(haven)
library(labelled)

# Load all files from the metadata
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
n9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID using full_join
full_data <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w3, by = "NSID") %>%
  full_join(w4, by = "NSID") %>%
  full_join(n9, by = "NSID")

# Create the three IMD variables
# imd15 from Wave 2 (Age 15) - IMDRSCORE.x
# imd16 from Wave 3 (Age 16) - IMDRSCORE.y
# imd32 from NS9 (Age 32) - W9DIMDD

full_data <- full_data %>%
  mutate(
    imd15 = IMDRSCORE.x,
    imd16 = IMDRSCORE.y,
    imd32 = W9DIMDD
  )

# Handle missing values for imd15 and imd16
# -94 = Insufficient Information -> map to -8 (standard code)
full_data$imd15[!is.na(full_data$imd15) & full_data$imd15 == -94] <- -8
full_data$imd16[!is.na(full_data$imd16) & full_data$imd16 == -94] <- -8
# Convert remaining NA to -3 (Not asked / not interviewed)
full_data$imd15[is.na(full_data$imd15)] <- -3
full_data$imd16[is.na(full_data$imd16)] <- -3

# Handle missing values for imd32
# -8 = Insufficient information -> -8 (already matches standard code)
full_data$imd32[!is.na(full_data$imd32) & full_data$imd32 == -8] <- -8
# Convert remaining NA to -3 (Not asked / not interviewed)
full_data$imd32[is.na(full_data$imd32)] <- -3

# Keep only NSID and the three IMD variables
output <- full_data %>%
  select(NSID, imd15, imd16, imd32)

# Check the distributions
cat("\n=== imd15 distribution ===\n")
print(table(full_data$imd15, useNA = "ifany"))

cat("\n=== imd16 distribution ===\n")
print(table(full_data$imd16, useNA = "ifany"))

cat("\n=== imd32 distribution ===\n")
print(table(full_data$imd32, useNA = "ifany"))

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("\nOutput written to data/output/cleaned_data.csv\n")
cat("Output dimensions:", dim(output), "\n")
cat("First 10 rows:\n")
print(head(output, 10))

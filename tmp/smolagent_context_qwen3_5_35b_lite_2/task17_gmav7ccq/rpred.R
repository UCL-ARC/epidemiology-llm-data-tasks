library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from the metadata
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", 
                        delim = "\t", show_col_types = FALSE)
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", 
                         delim = "\t", show_col_types = FALSE)
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", 
                        delim = "\t", show_col_types = FALSE)
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", 
                         delim = "\t", show_col_types = FALSE)
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", 
                  delim = "\t", show_col_types = FALSE)

cat("wave_two IMDRSCORE range:", range(wave_two$IMDRSCORE, na.rm = TRUE), "\n")
cat("wave_three IMDRSCORE range:", range(wave_three$IMDRSCORE, na.rm = TRUE), "\n")
cat("ns9 W9DIMDD range:", range(ns9$W9DIMDD, na.rm = TRUE), "\n")

# Function to harmonise missing values for IMDRSCORE
# -999 to -1 are user missing values
# -94 = Insufficient Information -> -8
# Other codes (-999 to -1) typically indicate schedule not applicable -> -2
harmonise_imd <- function(x) {
  x <- as.numeric(x)
  # Convert -94 (Insufficient Information) to -8
  x[x == -94] <- -8
  # Convert other negative codes (-999 to -1, excluding -94) to -2 (Schedule not applicable)
  x[x >= -999 & x <= -1 & x != -94] <- -2
  return(x)
}

# Function to harmonise missing values for W9DIMDD
# -8 = Insufficient information -> -8
harmonise_w9dimdd <- function(x) {
  x <- as.numeric(x)
  # Convert -8 (Insufficient information) to -8 (same)
  # Valid values are 1-10 (deciles)
  return(x)
}

# Create separate data frames with NSID and IMD variables
imd15_df <- wave_two %>%
  select(NSID, IMDRSCORE) %>%
  mutate(imd15 = harmonise_imd(IMDRSCORE)) %>%
  select(NSID, imd15)

imd16_df <- wave_three %>%
  select(NSID, IMDRSCORE) %>%
  mutate(imd16 = harmonise_imd(IMDRSCORE)) %>%
  select(NSID, imd16)

imd32_df <- ns9 %>%
  select(NSID, W9DIMDD) %>%
  mutate(imd32 = harmonise_w9dimdd(W9DIMDD)) %>%
  select(NSID, imd32)

cat("imd15_df nrow:", nrow(imd15_df), "\n")
cat("imd16_df nrow:", nrow(imd16_df), "\n")
cat("imd32_df nrow:", nrow(imd32_df), "\n")

# Create full cohort frame from wave_one (all IDs)
full_cohort <- wave_one %>%
  select(NSID)

# Merge all IMD data frames by NSID
cleaned_data <- full_cohort %>%
  full_join(imd15_df, by = "NSID") %>%
  full_join(imd16_df, by = "NSID") %>%
  full_join(imd32_df, by = "NSID")

cat("\nAfter merge - cleaned_data nrow:", nrow(cleaned_data), "\n")

# Write output to CSV without labels (labels not needed for CSV output)
write_csv(cleaned_data, "data/output/cleaned_data.csv")

cat("\nOutput written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(cleaned_data), "\n")
cat("Number of columns:", ncol(cleaned_data), "\n")
cat("\nSummary of IMD variables:\n")
print(table(cleaned_data$imd15, useNA = "ifany"))
print(table(cleaned_data$imd16, useNA = "ifany"))
print(table(cleaned_data$imd32, useNA = "ifany"))

# Verify output file exists
if (file.exists("data/output/cleaned_data.csv")) {
  cat("\nOutput file exists: YES\n")
} else {
  cat("\nOutput file exists: NO\n")
}

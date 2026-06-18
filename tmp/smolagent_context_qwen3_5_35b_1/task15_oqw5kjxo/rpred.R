# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
wave1_file <- "data/input/wave_one_lsype_young_person_2020.tab"
wave4_file <- "data/input/wave_four_lsype_young_person_2020.tab"
wave8_file <- "data/input/ns8_2015_derived.tab"
wave9_file <- "data/input/ns9_2022_derived_variables.tab"

# Load all files using readr::read_delim with tab delimiter
wave1 <- read_delim(wave1_file, delim = "\t", show_col_types = FALSE)
wave4 <- read_delim(wave4_file, delim = "\t", show_col_types = FALSE)
wave8 <- read_delim(wave8_file, delim = "\t", show_col_types = FALSE)
wave9 <- read_delim(wave9_file, delim = "\t", show_col_types = FALSE)

# Merge all datasets using full_join by NSID
cleaned <- full_join(wave1, wave4, by = "NSID")
cleaned <- full_join(cleaned, wave8, by = "NSID")
cleaned <- full_join(cleaned, wave9, by = "NSID")

# Income band categories (16 substantive bands)
income_labels <- c("less than 25", "25 to 50", "50 to 90", "90 to 140", 
                   "140 to 240", "240 to 300", "300 to 350", "350 to 400",
                   "400 to 500", "500 to 600", "600 to 700", "700 to 800",
                   "800 to 900", "900 to 1200", "1200 to 1400", "more than 1400")

# Create inc25 from W8DINCB (Wave 8, Age 25)
# Convert -1 to NA for not applicable
inc25_raw <- cleaned$W8DINCB
inc25_converted <- ifelse(inc25_raw == -1, NA_real_, inc25_raw)

# Create factor with proper labels
inc25 <- factor(inc25_converted, 
                levels = 1:16,
                labels = income_labels)

# Set variable label
var_label(inc25) <- "DV: Banded weekly income"

# Create inc32 from W9DINCB (Wave 9, Age 32)
inc32_raw <- cleaned$W9DINCB
inc32_converted <- ifelse(inc32_raw == -1, NA_real_, inc32_raw)

# Create factor with proper labels
inc32 <- factor(inc32_converted,
                levels = 1:16,
                labels = income_labels)

# Set variable label
var_label(inc32) <- "DV: Banded weekly income of cohort member and partner"

# Add columns to dataframe using mutate
cleaned <- cleaned %>%
  mutate(inc25 = inc25,
         inc32 = inc32)

# Select only ID and final derived variables
output <- cleaned %>%
  select(NSID, inc25, inc32)

# Write output to CSV
write_csv(output, "data/output/cleaned_data.csv")

# Print summary
cat("Output created successfully.\n")
cat("Number of rows:", nrow(output), "\n")
cat("Number of columns:", ncol(output), "\n")
cat("\ninc25 distribution:\n")
print(table(output$inc25, useNA = "ifany"))
cat("\ninc32 distribution:\n")
print(table(output$inc32, useNA = "ifany"))

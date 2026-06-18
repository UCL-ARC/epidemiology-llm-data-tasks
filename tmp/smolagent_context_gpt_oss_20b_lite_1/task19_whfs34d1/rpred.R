# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(haven)

# Helper function to harmonise missing values for BMI variables
# Converts NA to -3 (Not asked), retains -9 (Refusal), -8 (Insufficient information), -1 (Not applicable)
# Assumes no valid negative BMI values.
clean_bmi <- function(x) {
  x_clean <- ifelse(is.na(x), -3, x)
  as.numeric(x_clean)
}

# 1. Load all files from data/input/
wave1_path <- "data/input/wave_one_lsype_young_person_2020.tab"
wave4_path <- "data/input/wave_four_lsype_young_person_2020.tab"
wave8_path <- "data/input/ns8_2015_derived.tab"
wave9_path <- "data/input/ns9_2022_derived_variables.tab"

# Read delimited files (tab separated)
wave1 <- read_delim(wave1_path, delim = "\t", col_types = cols(.default = col_guess()))
wave4 <- read_delim(wave4_path, delim = "\t", col_types = cols(.default = col_guess()))
wave8 <- read_delim(wave8_path, delim = "\t", col_types = cols(.default = col_guess()))
wave9 <- read_delim(wave9_path, delim = "\t", col_types = cols(.default = col_guess()))

# 2. Merge all datasets by NSID to preserve full cohort
merged12 <- full_join(wave1, wave4, by = "NSID")
merged123 <- full_join(merged12, wave8, by = "NSID")
merged1234 <- full_join(merged123, wave9, by = "NSID")

# 3. Derive BMI variables
merged1234 <- merged1234 %>%
  mutate(
    bmi25 = clean_bmi(W8DBMI),
    bmi32 = clean_bmi(W9DBMI)
  )

# 4. Keep only ID and BMI variables
final_df <- merged1234 %>% select(NSID, bmi25, bmi32)

# 5. Write output CSV
write_csv(final_df, "data/output/cleaned_data.csv")

cat("BMI variables derived and written to data/output/cleaned_data.csv\n")
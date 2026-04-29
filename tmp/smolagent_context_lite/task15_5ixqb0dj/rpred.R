library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Load all files mentioned in metadata
file1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
file2 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
file3 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
file4 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = readr::cols(.default = 'c'))

# Convert numeric columns for the target variables specifically
file3$W8DINCB <- as.numeric(file3$W8DINCB)
file4$W9DINCB <- as.numeric(file4$W9DINCB)

# Merge datasets using full_join by NSID
full_cohort <- file1 %>%
  full_join(file2, by = 'NSID') %>%
  full_join(file3, by = 'NSID') %>%
  full_join(file4, by = 'NSID')

# 2. Identify Target Variables and Harmonise
# Target: inc25 (Age 25) and inc32 (Age 32)
# Based on metadata, ns8_2015_derived (Wave 8) likely corresponds to the Age 25 period 
# and ns9_2022_derived_variables (Wave 9) corresponds to Age 32.

# Standard Missing-Value Codes:
# -1 = Item not applicable
# -3 = Not asked / NA

process_income <- function(var) {
  # Map categories based on metadata labels
  # -1.0 is 'Not applicable' -> map to -1
  # NA -> map to -3
  res <- case_when(
    is.na(var) ~ -3,
    var == -1.0 ~ -1,
    TRUE ~ var
  )
  return(res)
}

# Create derived variables
full_cohort <- full_cohort %>%
  mutate(
    inc25 = process_income(W8DINCB),
    inc32 = process_income(W9DINCB)
  )

# Define labels for income bands
income_labels <- c(
  "-1" = "Not applicable",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "1" = "less than 25",
  "2" = "25 to 50",
  "3" = "50 to 90",
  "4" = "90 to 140",
  "5" = "140 to 240",
  "6" = "240 to 300",
  "7" = "300 to 350",
  "8" = "350 to 400",
  "9" = "400 to 500",
  "10" = "500 to 600",
  "11" = "600 to 700",
  "12" = "700 to 800",
  "13" = "800 to 900",
  "14" = "900 to 1200",
  "15" = "1200 to 1400",
  "16" = "more than 1400"
)

# Convert to factors with labels
full_cohort <- full_cohort %>%
  mutate(
    inc25 = factor(inc25, levels = as.numeric(names(income_labels)), labels = income_labels),
    inc32 = factor(inc32, levels = as.numeric(names(income_labels)), labels = income_labels)
  )

# 3. Final Output
final_data <- full_cohort %>%
  select(NSID, inc25, inc32)

write_csv(final_data, 'data/output/cleaned_data.csv')
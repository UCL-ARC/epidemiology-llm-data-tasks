library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Load files based on metadata
file1 <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
file4 <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
file8 <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = readr::cols(NSID = "c", W8DBMI = "d"))
file9 <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols(NSID = "c", W9DBMI = "d"))

# Merge datasets using full_join by NSID
cohort_frame <- file1 %>%
  full_join(file4, by = "NSID") %>%
  full_join(file8, by = "NSID") %>%
  full_join(file9, by = "NSID")

# 2. Target Variables: BMI at age 25 and 32
# Based on metadata:
# Wave 8 (Derived Variables) contains W8DBMI (Age 25)
# Wave 9 (Derived Variables) contains W9DBMI (Age 32)

# Function to map missing values based on standard scheme
# -9 = Refusal
# -8 = Don't know / insufficient information
# -1 = Not applicable
# NA -> -3 (Not asked / not interviewed)
map_missing <- function(x) {
  x <- as.numeric(x)
  # The metadata labels for BMI are already mapped to -9, -8, -1
  # We just need to handle NA
  x[is.na(x)] <- -3
  return(x)
}

# Process bmi25 (from W8DBMI)
cohort_frame <- cohort_frame %>%
  mutate(bmi25 = map_missing(W8DBMI))

# Process bmi32 (from W9DBMI)
cohort_frame <- cohort_frame %>%
  mutate(bmi32 = map_missing(W9DBMI))

# 3. Output Final File
# Keep only NSID and the derived BMI variables
final_data <- cohort_frame %>%
  select(NSID, bmi25, bmi32)

readr::write_csv(final_data, "data/output/cleaned_data.csv")
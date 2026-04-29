library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load files
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols())
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols())
wave_eight <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = readr::cols())
wave_nine <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols())

# Merge datasets
full_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Processing BMI for Age 25 (Wave 8)
# Metadata: W8DBMI. Missing: -9 (Refused), -8 (Insufficient), -1 (Not applicable)
# Requirement: Preserve continuous values. 
# Standard Missing: -9 Refusal, -8 DK/Insuff, -1 Not applicable, -3 Not asked (NA)

bmi_25 <- full_data %>%
  mutate(
    bmi_25 = W8DBMI,
    # Convert NAs to -3 (Not asked/interviewed)
    bmi_25 = ifelse(is.na(bmi_25), -3, bmi_25)
  ) %>%
  select(NSID, bmi_25)

# Processing BMI for Age 32 (Wave 9)
# Metadata: W9DBMI. Missing: -1 to -9
bmi_32 <- full_data %>%
  mutate(
    bmi_32 = W9DBMI,
    # Convert NAs to -3
    bmi_32 = ifelse(is.na(bmi_32), -3, bmi_32)
  ) %>%
  select(NSID, bmi_32)

# Combine results
final_df <- bmi_25 %>%
  full_join(bmi_32, by = "NSID")

# Write to CSV
write_csv(final_df, "data/output/cleaned_data.csv")
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all files by NSID
merged <- full_join(wave1, wave4, by = "NSID")
merged <- full_join(merged, wave6, by = "NSID")
merged <- full_join(merged, wave8, by = "NSID")
merged <- full_join(merged, wave9, by = "NSID")

# Standard missing value codes
# -9 = Refusal
# -8 = Don't know / insufficient information
# -7 = Prefer not to say
# -3 = Not asked at the fieldwork stage / not interviewed
# -2 = Schedule not applicable / script error / information lost
# -1 = Item not applicable

# Process W6MarStatYP (Wave 6, Age 19)
# Map missing values:
# -997 (Script error) -> -2
# -97 (Respondent declined self completion) -> -2
# -92 (Refused) -> -9
# -91 (Not applicable) -> -1
# -1 (Don't know) -> -8

# Create partnr19 (collapsed harmonised)
merged <- merged %>%
  mutate(
    partnr19 = case_when(
      W6MarStatYP %in% c(-997, -97) ~ -2,
      W6MarStatYP == -92 ~ -9,
      W6MarStatYP == -91 ~ -1,
      W6MarStatYP == -1 ~ -8,
      TRUE ~ as.numeric(W6MarStatYP)
    )
  )

# Process W8DMARSTAT (Wave 8, Age 25)
# Map missing values:
# -9 (Refused) -> -9
# -8 (Insufficient information) -> -8
# -1 (Not applicable) -> -1

# Create detailed adult variable partnradu25 from W8DMARSTAT
merged <- merged %>%
  mutate(
    partnradu25 = case_when(
      W8DMARSTAT %in% c(-9, -8, -1) ~ as.numeric(W8DMARSTAT),
      TRUE ~ as.numeric(W8DMARSTAT)
    )
  )

# Collapse detailed adult to harmonised categories for partnr25
merged <- merged %>%
  mutate(
    partnr25 = case_when(
      partnradu25 %in% c(-9, -8, -1) ~ partnradu25,
      partnradu25 == 1 ~ 1,           # Single and never married or in a CP
      partnradu25 == 2 ~ 2,           # Married
      partnradu25 %in% c(3, 7) ~ 3,   # Separated (married or CP)
      partnradu25 == 4 ~ 4,           # Divorced
      partnradu25 == 5 ~ 5,           # Widowed
      partnradu25 == 6 ~ 1,           # Civil Partner -> Single
      partnradu25 == 8 ~ 1,           # Former Civil Partner -> Single
      partnradu25 == 9 ~ 5            # Surviving Civil Partner -> Widowed
    )
  )

# Process W9DMARSTAT (Wave 9, Age 32)
# Map missing values:
# -9 (Refused) -> -9
# -8 (Insufficient information) -> -8

# Create detailed adult variable partnradu32 from W9DMARSTAT
merged <- merged %>%
  mutate(
    partnradu32 = case_when(
      W9DMARSTAT %in% c(-9, -8) ~ as.numeric(W9DMARSTAT),
      TRUE ~ as.numeric(W9DMARSTAT)
    )
  )

# Collapse detailed adult to harmonised categories for partnr32
merged <- merged %>%
  mutate(
    partnr32 = case_when(
      partnradu32 %in% c(-9, -8) ~ partnradu32,
      partnradu32 == 1 ~ 1,           # Single that is never married or never in CP
      partnradu32 == 2 ~ 2,           # Married
      partnradu32 %in% c(3, 4) ~ 3,   # Divorced or Legally separated -> Separated
      partnradu32 == 5 ~ 5,           # Widowed
      partnradu32 == 6 ~ 1,           # Civil Partner -> Single
      partnradu32 == 7 ~ 1,           # Former Civil Partner -> Single
      partnradu32 == 8 ~ 5            # Surviving Civil Partner -> Widowed
    )
  )

# Keep only final derived variables and NSID
output <- merged %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Write output to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
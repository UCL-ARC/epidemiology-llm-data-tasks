library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define a function to map missing values based on metadata
map_missing <- function(var, metadata) {
  case_when(
    var %in% metadata$refusal_codes ~ -9,
    var %in% metadata$dont_know_codes ~ -8,
    var %in% metadata$prefer_not_to_say_codes ~ -7,
    var %in% metadata$not_asked_codes ~ -3,
    var %in% metadata$not_applicable_codes ~ -2,
    var %in% metadata$information_lost_codes ~ -2,
    TRUE ~ var
  )
}

# Define metadata for each wave
wave4_metadata <- list(
  refusal_codes = c(-92),
  dont_know_codes = c(-94),
  not_applicable_codes = c(-91),
  information_lost_codes = c(-999)
)

wave5_metadata <- list(
  dont_know_codes = c(-94),
  information_lost_codes = c(-999)
)

wave6_metadata <- list(
  not_applicable_codes = c(-91),
  information_lost_codes = c(-999)
)

wave7_metadata <- list(
  not_applicable_codes = c(-91),
  information_lost_codes = c(-999)
)

wave8_metadata <- list(
  refusal_codes = c(-9),
  dont_know_codes = c(-8),
  not_applicable_codes = c(-1)
)

wave9_metadata <- list(
  refusal_codes = c(-9),
  dont_know_codes = c(-8),
  not_applicable_codes = c(-1)
)

# Function to harmonize economic activity into 6 categories
harmonize_ecoact <- function(var, wave) {
  case_when(
    var %in% c(1, 2, 3, 10) ~ 1,  # Paid work
    var %in% c(4, 5, 6) ~ 2,     # Education
    var %in% c(7, 8) ~ 3,        # Unemployed
    var %in% c(9, 11) ~ 4,       # Other
    var %in% c(12, 13, 14) ~ 5,  # Other specific
    var %in% c(15) ~ 6,          # Not defined
    TRUE ~ var
  )
}

# Process each wave
merged_data <- merged_data %>%
  mutate(
    ecoact17 = map_missing(W4empsYP, wave4_metadata),
    ecoact18 = map_missing(W5mainactYP, wave5_metadata),
    ecoact19 = map_missing(W6TCurrentAct, wave6_metadata),
    ecoact20 = map_missing(W7TCurrentAct, wave7_metadata),
    ecoact25 = map_missing(W8DACTIVITYC, wave8_metadata),
    ecoact32 = map_missing(W9DACTIVITYC, wave9_metadata)
  )

# Harmonize economic activity variables
merged_data <- merged_data %>%
  mutate(
    ecoact17 = harmonize_ecoact(ecoact17, "wave4"),
    ecoact18 = harmonize_ecoact(ecoact18, "wave5"),
    ecoact19 = harmonize_ecoact(ecoact19, "wave6"),
    ecoact20 = harmonize_ecoact(ecoact20, "wave7"),
    ecoact25 = harmonize_ecoact(ecoact25, "wave8"),
    ecoact32 = harmonize_ecoact(ecoact32, "wave9")
  )

# Create detailed variables for waves 25 and 32
merged_data <- merged_data %>%
  mutate(
    ecoactadu25 = W8DACTIVITYC,
    ecoactadu32 = W9DACTIVITYC
  )

# Select only the required variables
cleaned_data <- merged_data %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Write the cleaned data to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return the path to the cleaned data
"data/output/cleaned_data.csv"
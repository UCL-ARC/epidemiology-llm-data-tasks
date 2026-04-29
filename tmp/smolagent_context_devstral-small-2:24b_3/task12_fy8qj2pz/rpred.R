library(dplyr)
library(purrr)
library(readr)

# Load each dataset
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
ns8_2015_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave_five <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns9_2022_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets using full_join
merged_data <- wave_one %>%
  full_join(ns8_2015_derived, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(wave_five, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns9_2022_main, by = "NSID")

# Rename variables
merged_data <- merged_data %>%
  rename(
    nssec17 = W8DNSSEC17,
    nssec18 = W5nsseccatYP,
    nssec19 = w6nsseccatYP,
    nssec20 = W7NSSECCat,
    nssec25 = W9NSSEC,
    nssec32 = W9NSSEC
  )

# Check which NS-SEC variables exist
nssec_vars <- c('nssec17', 'nssec18', 'nssec19', 'nssec20', 'nssec25', 'nssec32')
nssec_vars <- nssec_vars[nssec_vars %in% names(merged_data)]

if(length(nssec_vars) == 0) {
  stop("No NS-SEC variables found in the merged data.")
}

# Harmonize missing value codes
for(var in nssec_vars) {
  merged_data <- merged_data %>%
    mutate(
      !!var := case_when(
        !!sym(var) == -999 ~ -3,
        !!sym(var) == -91 ~ -1,
        !!sym(var) == -9 ~ -9,
        !!sym(var) == -8 ~ -8,
        !!sym(var) == -1 ~ -1,
        TRUE ~ !!sym(var)
      )
    )
}

# Collapse fractional codes to major categories
for(var in nssec_vars) {
  merged_data <- merged_data %>%
    mutate(
      !!var := ifelse(!is.na(!!sym(var)), floor(!!sym(var)), -3)
    )
}

# Ensure only valid categories (1-17) are retained
for(var in nssec_vars) {
  merged_data <- merged_data %>%
    mutate(
      !!var := ifelse(!!sym(var) >= 1 & !!sym(var) <= 17, !!sym(var), -3)
    )
}

# Derive full-time student category for age 25 if it exists
if('nssec25' %in% nssec_vars) {
  merged_data <- merged_data %>%
    mutate(
      nssec25 = case_when(
        nssec25 == -3 & W8DACTIVITYC == 5 ~ 15,
        TRUE ~ nssec25
      )
    )
}

# Select only the required variables
cleaned_data <- merged_data %>%
  select(NSID, all_of(nssec_vars))

# Write the cleaned data to a CSV file
write_csv(cleaned_data, "data/output/cleaned_data.csv")
cat("Successfully created cleaned_data.csv with variables:", nssec_vars, "\n")
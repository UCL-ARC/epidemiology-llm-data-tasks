library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Select relevant variables from each wave
w1 <- wave1 %>% select(NSID)
w4 <- wave4 %>% select(NSID, W4nsseccatYP)
w5 <- wave5 %>% select(NSID, W5nsseccatYP)
w6 <- wave6 %>% select(NSID, w6nsseccatYP)
w7 <- wave7 %>% select(NSID, W7NSSECCat)
w8 <- wave8 %>% select(NSID, W8DNSSEC17, W8DACTIVITYC)
w9 <- wave9 %>% select(NSID, W9NSSEC)

# Merge all datasets by NSID using full_join
data <- w1 %>%
  full_join(w4, by = "NSID") %>%
  full_join(w5, by = "NSID") %>%
  full_join(w6, by = "NSID") %>%
  full_join(w7, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9, by = "NSID")

# Function to harmonize missing codes and collapse fractional categories
harmonize_nssec <- function(x, wave) {
  # Map wave-specific missing codes to standard scheme
  if (wave == 4) {
    x <- case_when(
      x == -99 ~ -3,  # YP Not interviewed -> Not asked
      x == -91 ~ -1,  # Not applicable
      TRUE ~ as.numeric(x)
    )
  } else if (wave == 5) {
    x <- case_when(
      x == -91 ~ -1,  # Not applicable
      TRUE ~ as.numeric(x)
    )
  } else if (wave == 6) {
    x <- case_when(
      x == -91 ~ -1,  # Not applicable
      TRUE ~ as.numeric(x)
    )
  } else if (wave == 7) {
    x <- case_when(
      x == -91 ~ -1,  # Not applicable
      TRUE ~ as.numeric(x)
    )
  } else if (wave == 8) {
    x <- case_when(
      x == -9 ~ -9,   # Refused
      x == -8 ~ -8,   # Insufficient information
      x == -1 ~ -1,   # Not applicable
      TRUE ~ as.numeric(x)
    )
  } else if (wave == 9) {
    x <- case_when(
      x == -1 ~ -1,   # Not applicable
      TRUE ~ as.numeric(x)
    )
  }
  
  # Collapse fractional codes to integer (floor)
  x <- ifelse(x > 0, floor(x), x)
  
  return(x)
}

# Apply harmonization to each wave
data <- data %>%
  mutate(
    nssec17 = harmonize_nssec(W4nsseccatYP, 4),
    nssec18 = harmonize_nssec(W5nsseccatYP, 5),
    nssec19 = harmonize_nssec(w6nsseccatYP, 6),
    nssec20 = harmonize_nssec(W7NSSECCat, 7),
    nssec25 = harmonize_nssec(W8DNSSEC17, 8),
    nssec32 = harmonize_nssec(W9NSSEC, 9)
  )

# Special derivation for Age 25: derive full-time student category (15) from activity
# W8DACTIVITYC == 5 means "Education: School/college/university"
data <- data %>%
  mutate(
    nssec25 = case_when(
      W8DACTIVITYC == 5 & (is.na(nssec25) | nssec25 < 1 | nssec25 > 17) ~ 15,
      TRUE ~ nssec25
    )
  )

# Remove the temporary activity variable
data <- data %>% select(-W8DACTIVITYC)

# Convert all NS-SEC variables to factors with proper labels
nssec_levels <- c(-9, -8, -7, -3, -2, -1, 1:17)
nssec_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-1" = "Item not applicable",
  "1" = "Employers in large organisations",
  "2" = "Higher managerial occupations",
  "3" = "Higher professional occupations",
  "4" = "Lower professional and higher technical occupations",
  "5" = "Lower managerial and administrative occupations",
  "6" = "Higher supervisory occupations",
  "7" = "Intermediate occupations",
  "8" = "Employers in small establishments",
  "9" = "Own account workers",
  "10" = "Lower supervisory occupations",
  "11" = "Lower technical occupations",
  "12" = "Semi-routine occupations",
  "13" = "Routine occupations",
  "14" = "Never worked and Long-term unemployed",
  "15" = "Full-time students",
  "16" = "Occupations not stated or inadequately described",
  "17" = "Not classifiable for other reasons"
)

data <- data %>%
  mutate(
    nssec17 = factor(nssec17, levels = nssec_levels, labels = nssec_labels),
    nssec18 = factor(nssec18, levels = nssec_levels, labels = nssec_labels),
    nssec19 = factor(nssec19, levels = nssec_levels, labels = nssec_labels),
    nssec20 = factor(nssec20, levels = nssec_levels, labels = nssec_labels),
    nssec25 = factor(nssec25, levels = nssec_levels, labels = nssec_labels),
    nssec32 = factor(nssec32, levels = nssec_levels, labels = nssec_labels)
  )

# Select only the required output variables
output <- data %>% select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Write output CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")

# Attempt to load wave9, but handle if it is empty or missing
wave9 <- tryCatch({
  read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
}, error = function(e) {
  NULL
})

# Check if wave9 is valid and has NSID
if (!is.null(wave9) && nrow(wave9) > 0 && "NSID" %in% colnames(wave9)) {
  # Merge all datasets by NSID, including wave9
  merged_data <- wave1 %>%
    full_join(wave4, by = "NSID") %>%
    full_join(wave5, by = "NSID") %>%
    full_join(wave6, by = "NSID") %>%
    full_join(wave7, by = "NSID") %>%
    full_join(wave8, by = "NSID") %>%
    full_join(wave9, by = "NSID")
} else {
  # Merge all datasets by NSID, excluding wave9
  merged_data <- wave1 %>%
    full_join(wave4, by = "NSID") %>%
    full_join(wave5, by = "NSID") %>%
    full_join(wave6, by = "NSID") %>%
    full_join(wave7, by = "NSID") %>%
    full_join(wave8, by = "NSID")
}

# Function to collapse fractional NS-SEC codes to major categories
collapse_nssec <- function(var) {
  ifelse(is.na(var), NA, floor(var))
}

# Derive nssec17 from wave4
merged_data <- merged_data %>%
  mutate(nssec17 = collapse_nssec(W4nsseccatYP))

# Derive nssec18 from wave5
merged_data <- merged_data %>%
  mutate(nssec18 = collapse_nssec(W5nsseccatYP))

# Derive nssec19 from wave6
merged_data <- merged_data %>%
  mutate(nssec19 = collapse_nssec(w6nsseccatYP))

# Derive nssec20 from wave7
merged_data <- merged_data %>%
  mutate(nssec20 = collapse_nssec(W7NSSECCat))

# Derive nssec25 from wave8 with special handling for full-time education
merged_data <- merged_data %>%
  mutate(nssec25 = case_when(
    W8DACTIVITYC == 5 ~ 15,  # Full-time education
    TRUE ~ collapse_nssec(W8DNSSEC17)
  ))

# Derive nssec32 from wave9 if available
if (!is.null(wave9) && "W9NSSEC" %in% colnames(merged_data)) {
  merged_data <- merged_data %>%
    mutate(nssec32 = collapse_nssec(W9NSSEC))
} else {
  # If wave9 is not available, set nssec32 to NA
  merged_data <- merged_data %>%
    mutate(nssec32 = NA)
}

# Apply standard missing-value codes
apply_missing_codes <- function(var) {
  case_when(
    is.na(var) ~ -3,
    var == -9 ~ -9,
    var == -8 ~ -8,
    var == -7 ~ -7,
    var == -3 ~ -3,
    var == -2 ~ -2,
    var == -1 ~ -1,
    TRUE ~ var
  )
}

merged_data <- merged_data %>%
  mutate(across(c(nssec17, nssec18, nssec19, nssec20, nssec25, nssec32), apply_missing_codes))

# Create labelled factors with meaningful labels
nssec_labels <- c(
  "1" = "Employers in large organisations",
  "2" = "Higher managerial and administrative occupations",
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
  "17" = "Not classifiable for other reasons",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

merged_data <- merged_data %>%
  mutate(across(c(nssec17, nssec18, nssec19, nssec20, nssec25, nssec32), ~ factor(.x, levels = as.numeric(names(nssec_labels)), labels = nssec_labels)))

# Select only NSID and derived variables
output_data <- merged_data %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")

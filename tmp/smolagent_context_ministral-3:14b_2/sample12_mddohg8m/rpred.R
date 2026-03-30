
# Load required libraries
library(haven)
library(dplyr)
library(readr)

# Suppress messages
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# Define NS-SEC labels
nssec_labels <- c(
  `-9` = "Refusal",
  `-8` = "Don't know/insufficient information",
  `-7` = "Prefer not to say",
  `-3` = "Not asked at the fieldwork stage/participated/interviewed",
  `-2` = "Schedule not applicable/Script error/information lost",
  `-1` = "Not applicable",
  `1` = "Employers in large organisations",
  `2` = "Higher managerial and administrative occupations",
  `3` = "Higher professional occupations",
  `4` = "Lower professional and higher technical occupations",
  `5` = "Lower managerial and administrative occupations",
  `6` = "Higher supervisory occupations",
  `7` = "Intermediate occupations",
  `8` = "Employers in small establishments",
  `9` = "Own account workers",
  `10` = "Lower supervisory occupations",
  `11` = "Lower technical occupations",
  `12` = "Semi-routine occupations",
  `13` = "Routine occupations",
  `14` = "Never worked and Long-term unemployed",
  `15` = "Full-time students",
  `16` = "Occupations not stated or inadequately described",
  `17` = "Not classifiable for other reasons"
)

# Load datasets with explicit column types and suppress messages
wave1 <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE) %>%
  mutate(W4nsseccatYP = as.numeric(W4nsseccatYP))
wave5 <- readr::read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE) %>%
  mutate(W5nsseccatYP = as.numeric(W5nsseccatYP))
wave6 <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE) %>%
  mutate(w6nsseccatYP = as.numeric(w6nsseccatYP))
wave7 <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE) %>%
  mutate(W7NSSECCat = as.numeric(W7NSSECCat))
wave8 <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE) %>%
  mutate(W8DNSSEC17 = as.numeric(W8DNSSEC17),
         W8DACTIVITYC = as.numeric(W8DACTIVITYC))
wave9 <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE) %>%
  mutate(W9NSSEC = as.numeric(W9NSSEC))

# Merge datasets
merged_data <- wave1 %>%
  left_join(wave4, by = "NSID") %>%
  left_join(wave5, by = "NSID") %>%
  left_join(wave6, by = "NSID") %>%
  left_join(wave7, by = "NSID") %>%
  left_join(wave8, by = "NSID") %>%
  left_join(wave9, by = "NSID")

# Function to process NS-SEC variables
process_nssec <- function(data, var_name, age_suffix) {
  data <- data %>%
    mutate(!!var_name := ifelse(is.na(!!sym(var_name)), -3, !!sym(var_name))) %>%
    mutate(!!var_name := floor(!!sym(var_name))) %>%
    mutate(!!var_name := factor(!!sym(var_name), levels = names(nssec_labels), labels = nssec_labels)) %>%
    rename(!!sym(paste0("nssec", age_suffix)) := !!sym(var_name))

  return(data)
}

# Process each wave's NS-SEC variable
merged_data <- process_nssec(merged_data, "W4nsseccatYP", "17")
merged_data <- process_nssec(merged_data, "W5nsseccatYP", "18")
merged_data <- process_nssec(merged_data, "w6nsseccatYP", "19")
merged_data <- process_nssec(merged_data, "W7NSSECCat", "20")

# Special processing for Age 25 (Wave 8)
merged_data <- merged_data %>%
  mutate(
    nssec25 = ifelse(
      !is.na(W8DACTIVITYC) & W8DACTIVITYC == 5 & is.na(W8DNSSEC17),
      15,
      ifelse(
        is.na(W8DNSSEC17),
        -3,
        floor(W8DNSSEC17)
      )
    )
  ) %>%
  mutate(nssec25 = factor(nssec25, levels = names(nssec_labels), labels = nssec_labels))

# Process Wave 9 (Age 32)
merged_data <- process_nssec(merged_data, "W9NSSEC", "32")

# Select required variables
final_data <- merged_data %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write output
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)

# Print success message
message("Data processing completed successfully!")

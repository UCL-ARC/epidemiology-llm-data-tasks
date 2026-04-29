library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# File paths
path_prefix <- "data/input/"

# 1. Load files
files_to_load <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_five_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_main_interview.tab"
)

# Load each file explicitly into a list
data_list <- map(files_to_load, function(f) {
  read_delim(paste0(path_prefix, f), delim = "\t", col_types = cols(NSID = "character", .default = "double"))
})

# Merge datasets using full_join by NSID
full_df <- reduce(data_list, full_join, by = "NSID")

# Helper function for missing values mapping
process_nssec <- function(var_name, df) {
  if (!var_name %in% names(df)) {
    return(rep(NA, nrow(df)))
  }
  
  vec <- df[[var_name]]
  
  # Map missing values based on metadata labels
  # -91.0 -> Not applicable (-1)
  # -99.0 -> YP Not interviewed (-3)
  # -9.0 -> Refused (-9), -8.0 -> Insufficient info (-8), -1.0 -> Not applicable (-1)
  res <- case_when(
    is.na(vec) ~ -3,
    vec == -91.0 ~ -1,
    vec == -99.0 ~ -3,
    vec == -9.0 ~ -9,
    vec == -8.0 ~ -8,
    vec == -1.0 ~ -1,
    TRUE ~ vec
  )
  
  # Collapse detailed categories (e.g., 3.1, 3.2 -> 3)
  res <- case_when(
    res >= 1 ~ floor(res),
    TRUE ~ res
  )
  
  return(res)
}

# Define labels for the harmonised NS-SEC
nssec_labels <- c(
  "1" = "Employers in large organisations",
  "2" = "Higher managerial occupations",
  "3" = "Higher professional occupations",
  "4" = "Lower professional and higher technical occupations",
  "5" = "Lower managerial occupations",
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

# Derive the required variables
full_df <- full_df %>%
  mutate(
    nssec17 = process_nssec("W4nsseccatYP", .),
    nssec18 = process_nssec("W5nsseccatYP", .),
    nssec19 = process_nssec("w6nsseccatYP", .),
    nssec20 = process_nssec("W7NSSECCat", .),
    nssec25 = process_nssec("W8DNSSEC17", .),
    nssec32 = process_nssec("W9NSSEC", .)
  )

# Convert to factors with explicit labels
final_vars <- c("nssec17", "nssec18", "nssec19", "nssec20", "nssec25", "nssec32")
for (var in final_vars) {
  full_df[[var]] <- factor(full_df[[var]], levels = as.numeric(names(nssec_labels)), labels = nssec_labels)
}

# Select only ID and final derived variables
final_df <- full_df %>% select(NSID, all_of(final_vars))

# Write to CSV
write_csv(final_df, "data/output/cleaned_data.csv")
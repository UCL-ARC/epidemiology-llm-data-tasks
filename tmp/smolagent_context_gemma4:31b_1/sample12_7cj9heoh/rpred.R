library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define files and their variables mapping
files_info <- list(
  wave1 = list(file = "wave_one_lsype_young_person_2020.tab", var = NULL),
  age17 = list(file = "wave_four_lsype_young_person_2020.tab", var = "W4nsseccatYP"),
  age18 = list(file = "wave_five_lsype_young_person_2020.tab", var = "W5nsseccatYP"),
  age19 = list(file = "wave_six_lsype_young_person_2020.tab", var = "w6nsseccatYP"),
  age20 = list(file = "wave_seven_lsype_young_person_2020.tab", var = "W7NSSECCat"),
  age25 = list(file = "ns8_2015_derived.tab", var = "W8DNSSEC17", activity = "W8DACTIVITYC"),
  age32 = list(file = "ns9_2022_main_interview.tab", var = "W9NSSEC")
)

# Load and merge data
full_data <- NULL

for (name in names(files_info)) {
  file_path <- paste0("data/input/", files_info[[name]]$file)
  df <- read_delim(file_path, delim = "\t", col_types = readr::cols_only(NSID = col_character()))
  
  if (!is.null(files_info[[name]]$var)) {
    df <- read_delim(file_path, delim = "\t", col_types = readr::cols_only(
      NSID = col_character(), 
      !!files_info[[name]]$var := col_double()
    ))
  }
  
  if (!is.null(files_info[[name]]$activity)) {
    df <- read_delim(file_path, delim = "\t", col_types = readr::cols_only(
      NSID = col_character(), 
      !!files_info[[name]]$var := col_double(),
      !!files_info[[name]]$activity := col_double()
    ))
  }

  if (is.null(full_data)) {
    full_data <- df
  } else {
    full_data <- full_join(full_data, df, by = "NSID")
  }
}

# Harmonization function for missing values based on meaning
# Meaning mapping from metadata
# -9: Refusal, -8: Don't know/insufficient, -7: Prefer not to say, -3: Not asked, -2: Schedule error, -1: Not applicable

harmonise_missing <- function(x, var_name) {
  # Map based on metadata labels provided in the prompt
  # For the NS-SEC variables, we look at the specific value labels
  # Note: Since we are processing numeric values, we map the codes
  
  # General logic: 
  # -91.0 (found in waves 4-7) -> Not applicable -> -1
  # -99.0 (found in wave 4) -> YP Not interviewed -> -3
  # -9.0 -> Refusal -> -9
  # -8.0 -> Insufficient information -> -8
  # -1.0 -> Not applicable -> -1
  
  x <- x %>% 
    mutate(
      val = case_when(
        . == -91.0 ~ -1,
        . == -99.0 ~ -3,
        . == -9.0 ~ -9,
        . == -8.0 ~ -8,
        . == -1.0 ~ -1,
        TRUE ~ .
      )
    )
  return(x)
}

# Processing NS-SEC variables
process_nssec <- function(data, var_name, age_suffix) {
  var_raw <- data[[var_name]]
  
  # 1. Handle missing codes before floor (since floor(-1.1) is -2)
  # Based on metadata labels:
  # -91.0 -> -1 (Not applicable)
  # -99.0 -> -3 (Not interviewed)
  # -9.0 -> -9 (Refused)
  # -8.0 -> -8 (Insufficient)
  # -1.0 -> -1 (Not applicable)
  
  processed <- var_raw
  processed[processed == -91.0] <- -1
  processed[processed == -99.0] <- -3
  processed[processed == -9.0] <- -9
  processed[processed == -8.0] <- -8
  processed[processed == -1.0] <- -1
  
  # 2. Floor for fractional codes
  # Only floor values > 0 to avoid messing with negative missing codes
  processed <- ifelse(processed > 0, floor(processed), processed)
  
  # 3. Accept only 1-17
  # Values that are not in 1-17 and not the standard missing codes should be handled
  # But prompt says: "accept only integer categories 1-17 as valid"
  # We keep the standard missing codes: -9, -8, -7, -3, -2, -1
  valid_missing <- c(-9, -8, -7, -3, -2, -1)
  processed[!(processed %in% 1:17 | processed %in% valid_missing)] <- -3
  
  # 4. Recode NA to -3
  processed[is.na(processed)] <- -3
  
  return(as.numeric(processed))
}

# Special derivation for Age 25: Student (15)
# W8DACTIVITYC: 5.0 = Education: School/college/university
full_data <- full_data %>%
  mutate(nssec25 = process_nssec(., "W8DNSSEC17", "25"))

# Apply the student override for age 25
# If W8DACTIVITYC == 5, then nssec25 = 15
full_data$nssec25[full_data$W8DACTIVITYC == 5] <- 15

# Process other ages
full_data$nssec17 <- process_nssec(full_data, "W4nsseccatYP", "17")
full_data$nssec18 <- process_nssec(full_data, "W5nsseccatYP", "18")
full_data$nssec19 <- process_nssec(full_data, "w6nsseccatYP", "19")
full_data$nssec20 <- process_nssec(full_data, "W7NSSECCat", "20")
full_data$nssec32 <- process_nssec(full_data, "W9NSSEC", "32")

# Factor labels
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
  "-8" = "Don't know/insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-1" = "Item not applicable"
)

# Convert to factors
nssec_vars <- c("nssec17", "nssec18", "nssec19", "nssec20", "nssec25", "nssec32")
for (v in nssec_vars) {
  full_data[[v]] <- factor(full_data[[v]], levels = as.numeric(names(nssec_labels)), labels = nssec_labels)
}

# Final Selection
final_output <- full_data %>%
  select(NSID, all_of(nssec_vars))

write_csv(final_output, "data/output/cleaned_data.csv")

library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
path_prefix <- "data/input/"

# Load datasets
# Wave 1 (Base for NSIDs)
df1 <- readr::read_delim(paste0(path_prefix, "wave_one_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c")) %>% select(NSID)

# Age 17
df17 <- readr::read_delim(paste0(path_prefix, "wave_four_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(NSID = "c", W4nsseccatYP = "numeric")) %>% select(NSID, nssec17 = W4nsseccatYP)

# Age 18
df18 <- readr::read_delim(paste0(path_prefix, "wave_five_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(NSID = "c", W5nsseccatYP = "numeric")) %>% select(NSID, nssec18 = W5nsseccatYP)

# Age 19
df19 <- readr::read_delim(paste0(path_prefix, "wave_six_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(NSID = "c", w6nsseccatYP = "numeric")) %>% select(NSID, nssec19 = w6nsseccatYP)

# Age 20
df20 <- readr::read_delim(paste0(path_prefix, "wave_seven_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(NSID = "c", W7NSSECCat = "numeric")) %>% select(NSID, nssec20 = W7NSSECCat)

# Age 25
df25 <- readr::read_delim(paste0(path_prefix, "ns8_2015_derived.tab"), delim = "\t", col_types = readr::cols(NSID = "c", W8DNSSEC17 = "numeric", W8DACTIVITYC = "numeric")) %>% select(NSID, nssec25 = W8DNSSEC17, W8DACTIVITYC)

# Age 32
df32 <- readr::read_delim(paste0(path_prefix, "ns9_2022_main_interview.tab"), delim = "\t", col_types = readr::cols(NSID = "c", W9NSSEC = "numeric")) %>% select(NSID, nssec32 = W9NSSEC)

# Merge all
final_df <- df1 %>%
  full_join(df17, by = "NSID") %>%
  full_join(df18, by = "NSID") %>%
  full_join(df19, by = "NSID") %>%
  full_join(df20, by = "NSID") %>%
  full_join(df25, by = "NSID") %>%
  full_join(df32, by = "NSID")

# Standard Missing Mapping
# Meaning -> Code
# Refusal -> -9
# Don't know/insufficient information -> -8
# Prefer not to say -> -7
# Not asked/participated/interviewed -> -3
# Schedule not applicable/Script error/lost -> -2
# Not applicable -> -1

process_nssec <- function(x, wave_name) {
  # 1. Handle Nulls first (coded as -3)
  x[is.na(x)] <- -3
  
  # 2. Wave specific mapping based on metadata meanings
  # Age 17: -99 (YP Not interviewed -> -3), -91 (Not applicable -> -1)
  if(wave_name == "nssec17") {
    x[x == -99] <- -3
    x[x == -91] <- -1
  }
  # Age 18, 19, 20: -91 (Not applicable -> -1)
  if(wave_name %in% c("nssec18", "nssec19", "nssec20")) {
    x[x == -91] <- -1
  }
  # Age 25: -9 (Refused -> -9), -8 (Insufficient info -> -8), -1 (Not applicable -> -1)
  # Age 32: -1 (Not Applicable -> -1)
  
  # 3. Collapse fractional codes to major category using floor()
  # We only floor positive values (1-17)
  pos_idx <- x > 0
  x[pos_idx] <- floor(x[pos_idx])
  
  # 4. Accept only integer categories 1-17
  # Anything else that isn't a defined missing code is set to -3
  valid_codes <- c(1:17, -9, -8, -7, -3, -2, -1)
  x[!(x %in% valid_codes)] <- -3
  
  return(x)
}

# Apply processing to all nssec variables
nssec_vars <- c("nssec17", "nssec18", "nssec19", "nssec20", "nssec25", "nssec32")

for(var in nssec_vars) {
  final_df[[var]] <- process_nssec(final_df[[var]], var)
}

# Special Derivation for Age 25: Full-time student (15) using W8DACTIVITYC
# W8DACTIVITYC: 5.0 = Education: School/college/university
final_df <- final_df %>%
  mutate(nssec25 = ifelse(!is.na(W8DACTIVITYC) & W8DACTIVITYC == 5, 15, nssec25))

# Convert to labelled factors
labels_map <- c(
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

for(var in nssec_vars) {
  final_df[[var]] <- factor(final_df[[var]], levels = as.numeric(names(labels_map)), labels = labels_map)
}

# Final selection and output
final_output <- final_df %>% select(NSID, all_of(nssec_vars))
write.csv(final_output, "data/output/cleaned_data.csv", row.names = FALSE)

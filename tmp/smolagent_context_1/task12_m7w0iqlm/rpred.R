library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
path_prefix <- "data/input/"

# Metadata files
files_to_load <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_five_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_main_interview.tab'
)

# Load each file explicitly
data_list <- map(files_to_load, ~read_delim(paste0(path_prefix, .x), delim = "\t", col_types = cols(.default = "c")))

# Merge datasets using full_join by NSID
data_list <- map(data_list, ~mutate(.x, NSID = as.character(NSID)))
cohort_frame <- reduce(data_list, full_join, by = "NSID")

# Convert necessary columns to numeric for processing
vars_to_numeric <- c("W4nsseccatYP", "W5nsseccatYP", "w6nsseccatYP", "W7NSSECCat", "W8DNSSEC17", "W9NSSEC", "W8DACTIVITYC")
cohort_frame <- cohort_frame %>% 
  mutate(across(any_of(vars_to_numeric), as.numeric))

# 6 & 7. Missing Value Harmonisation Function
harmonise_missing <- function(x) {
  res <- x
  res[x == -91] <- -1
  res[x == -99] <- -3
  res[x == -92] <- -9
  res[x == -999] <- -2
  res[is.na(res)] <- -3
  return(res)
}

# 8. Response Category Harmonisation
collapse_nssec <- function(x) {
  res <- x
  valid_idx <- which(x >= 1)
  res[valid_idx] <- floor(x[valid_idx])
  return(res)
}

# Labels for NS-SEC 1-17 (Must be a named vector where names are the labels and values are the codes)
# The error occurred because set_value_labels expects a named vector where the NAMES are the labels 
# and the VALUES are the numeric codes, or vice versa depending on the version, 
# but specifically the values in the vector must be numeric to match the double vector x.
nssec_labels_vec <- c(
  "Employers in large organisations" = 1,
  "Higher managerial and administrative occupations" = 2,
  "Higher professional occupations" = 3,
  "Lower professional and higher technical occupations" = 4,
  "Lower managerial and administrative occupations" = 5,
  "Higher supervisory occupations" = 6,
  "Intermediate occupations" = 7,
  "Employers in small establishments" = 8,
  "Own account workers" = 9,
  "Lower supervisory occupations" = 10,
  "Lower technical occupations" = 11,
  "Semi-routine occupations" = 12,
  "Routine occupations" = 13,
  "Never worked and Long-term unemployed" = 14,
  "Full-time students" = 15,
  "Occupations not stated or inadequately described" = 16,
  "Not classifiable for other reasons" = 17,
  "Not applicable" = -1,
  "Schedule not applicable / script error / information lost" = -2,
  "Not asked at the fieldwork stage / not interviewed" = -3,
  "Prefer not to say" = -7,
  "Don't know / insufficient information" = -8,
  "Refusal" = -9
)

# Processing waves
cohort_frame <- cohort_frame %>% 
  mutate(
    nssec17 = collapse_nssec(harmonise_missing(W4nsseccatYP)),
    nssec18 = collapse_nssec(harmonise_missing(W5nsseccatYP)),
    nssec19 = collapse_nssec(harmonise_missing(w6nsseccatYP)),
    nssec20 = collapse_nssec(harmonise_missing(W7NSSECCat))
  )

# Wave 8 (Age 25) special logic
cohort_frame <- cohort_frame %>% 
  mutate(nssec25_raw = collapse_nssec(harmonise_missing(W8DNSSEC17)))

cohort_frame <- cohort_frame %>% 
  mutate(nssec25 = ifelse(!is.na(W8DACTIVITYC) & W8DACTIVITYC == 5, 15, nssec25_raw))

# Wave 9 (Age 32)
cohort_frame <- cohort_frame %>% 
  mutate(nssec32 = collapse_nssec(harmonise_missing(W9NSSEC)))

# Final selection and labeling
final_vars <- c("NSID", "nssec17", "nssec18", "nssec19", "nssec20", "nssec25", "nssec32")
final_df <- cohort_frame %>% select(all_of(final_vars))

for(v in c("nssec17", "nssec18", "nssec19", "nssec20", "nssec25", "nssec32")) {
  final_df[[v]] <- set_value_labels(final_df[[v]], nssec_labels_vec)
}

write_csv(final_df, "data/output/cleaned_data.csv")

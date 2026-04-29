library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define files from metadata
files_metadata <- list(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_five_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_main_interview.tab"
)

# Load files
load_data <- function(filename) {
  read_delim(paste0("data/input/", filename), delim = "\t", col_types = cols(.default = "c"))
}

data_list <- map(files_metadata, load_data)
names(data_list) <- files_metadata

# Merge datasets
full_frame <- data_list[[1]] %>% 
  select(NSID) %>% 
  full_join(data_list[[2]], by = "NSID") %>% 
  full_join(data_list[[3]], by = "NSID") %>% 
  full_join(data_list[[4]], by = "NSID") %>% 
  full_join(data_list[[5]], by = "NSID") %>% 
  full_join(data_list[[6]], by = "NSID") %>% 
  full_join(data_list[[7]], by = "NSID")

# Define labels for NS-SEC major categories
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
  "17" = "Not classifiable for other reasons"
)

# Helper to clean NS-SEC
clean_nssec <- function(var_vec, activity_vec = NULL) {
  # Convert to numeric first
  val <- as.numeric(var_vec)
  
  # Handle specific missing codes based on metadata/requirements
  # Note: we use the original character vector to check for specific string patterns
  val[var_vec == "-91.0" | var_vec == "-91"] <- -1
  val[var_vec == "-99.0" | var_vec == "-99"] <- -3
  
  # Collapse fractional codes to integer for substantive responses (val > 0)
  # Use which() or handle NAs to avoid "NAs are not allowed in subscripted assignments"
  substantive_idx <- which(val > 0)
  if(length(substantive_idx) > 0) {
    val[substantive_idx] <- floor(val[substantive_idx])
  }
  
  # Special rule for Wave 8 (nssec25)
  if (!is.null(activity_vec)) {
    # Activity 5 = Education: School/college/university
    is_student <- (activity_vec == "5.0" | activity_vec == "5")
    val[is_student] <- 15
  }
  
  # Final missing value cleanup: Convert NA/Null to -3
  val[is.na(val)] <- -3
  
  return(val)
}

# Apply cleaning for each wave
final_df <- full_frame %>% 
  mutate(
    nssec17 = clean_nssec(W4nsseccatYP),
    nssec18 = clean_nssec(W5nsseccatYP),
    nssec19 = clean_nssec(w6nsseccatYP),
    nssec20 = clean_nssec(W7NSSECCat),
    nssec25 = clean_nssec(W8DNSSEC17, W8DACTIVITYC),
    nssec32 = clean_nssec(W9NSSEC)
  )

# Create factors with labels
apply_nssec_labels <- function(x) {
  # Levels: -9, -8, -7, -3, -2, -1, 1:17
  factor(x, 
         levels = c(-9, -8, -7, -3, -2, -1, 1:17), 
         labels = c("Refusal", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Not applicable", nssec_labels))
}

# Apply labels to the 6 variables
final_df <- final_df %>% 
  mutate(across(starts_with("nssec"), apply_nssec_labels))

# Select only NSID and derived variables
final_df <- final_df %>% 
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

write_csv(final_df, "data/output/cleaned_data.csv")
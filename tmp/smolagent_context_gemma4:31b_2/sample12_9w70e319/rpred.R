library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define files and variables
files <- list(
  wave1 = 'data/input/wave_one_lsype_young_person_2020.tab',
  age17 = 'data/input/wave_four_lsype_young_person_2020.tab',
  age18 = 'data/input/wave_five_lsype_young_person_2020.tab',
  age19 = 'data/input/wave_six_lsype_young_person_2020.tab',
  age20 = 'data/input/wave_seven_lsype_young_person_2020.tab',
  age25 = 'data/input/ns8_2015_derived.tab',
  age32 = 'data/input/ns9_2022_main_interview.tab'
)

# Loading
load_tab <- function(path) read_delim(path, delim = "\t", col_types = cols(.default = "c"))

data_w1 <- load_tab(files$wave1) %>% select(NSID)
data_17 <- load_tab(files$age17) %>% select(NSID, W4nsseccatYP)
data_18 <- load_tab(files$age18) %>% select(NSID, W5nsseccatYP)
data_19 <- load_tab(files$age19) %>% select(NSID, w6nsseccatYP)
data_20 <- load_tab(files$age20) %>% select(NSID, W7NSSECCat)
data_25 <- load_tab(files$age25) %>% select(NSID, W8DNSSEC17, W8DACTIVITYC)
data_32 <- load_tab(files$age32) %>% select(NSID, W9NSSEC)

# Merge
final_df <- data_w1 %>%
  full_join(data_17, by = "NSID") %>%
  full_join(data_18, by = "NSID") %>%
  full_join(data_19, by = "NSID") %>%
  full_join(data_20, by = "NSID") %>%
  full_join(data_25, by = "NSID") %>%
  full_join(data_32, by = "NSID")

# Harmonization helper
# Mapping logic based on metadata meanings
harmonise_missing <- function(val, wave_name) {
  if (is.na(val)) return(-3)
  val_num <- as.numeric(val)
  if (is.na(val_num)) return(-3)
  
  # General mappings for the provided metadata
  # -9 = Refusal, -8 = DK, -7 = Prefer not to say, -3 = Not asked, -2 = Script error, -1 = Not applicable
  if (val_num == -9) return(-9)
  if (val_num == -8) return(-8)
  if (val_num == -7) return(-7)
  if (val_num == -99) return(-3) # YP Not interviewed
  if (val_num == -91) return(-1) # Not applicable
  if (val_num == -1) return(-1)  # Not applicable
  
  return(val_num)
}

process_nssec <- function(vec, wave_name) {
  # Convert to numeric first
  num_vec <- as.numeric(vec)
  
  # Harmonize missing codes based on the specific logic
  processed <- map_dbl(vec, ~harmonise_missing(.x, wave_name))
  
  # The floor() operation for fractional codes
  # We only floor the positive values (1-17)
  final_vec <- map_dbl(vec, function(x) {
    if (is.na(x)) return(-3)
    v <- as.numeric(x)
    if (is.na(v)) return(-3)
    if (v < 0) return(harmonise_missing(x, wave_name))
    return(floor(v))
  })
  
  # Accept only 1-17, others are missing/invalid
  final_vec <- ifelse(final_vec >= 1 & final_vec <= 17, final_vec, 
                     ifelse(final_vec < 0, final_vec, -3))
  
  return(final_vec)
}

# Apply processing
final_df <- final_df %>%
  mutate(
    nssec17 = process_nssec(W4nsseccatYP, "17"),
    nssec18 = process_nssec(W5nsseccatYP, "18"),
    nssec19 = process_nssec(w6nsseccatYP, "19"),
    nssec20 = process_nssec(W7NSSECCat, "20"),
    nssec25 = process_nssec(W8DNSSEC17, "25"),
    nssec32 = process_nssec(W9NSSEC, "32")
  )

# Special Derivation for Age 25: Full-time student (15)
# W8DACTIVITYC: 5.0 is Education: School/college/university
final_df <- final_df %>%
  mutate(nssec25 = ifelse(!is.na(W8DACTIVITYC) & W8DACTIVITYC == "5", 15, nssec25))

# Factor Labels
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

# Apply labels and convert to factor
final_df <- final_df %>%
  mutate(across(starts_with("nssec"), ~factor(.x, levels = as.numeric(names(nssec_labels)), labels = nssec_labels)))

# Final selection
final_output <- final_df %>% select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

write_csv(final_output, "data/output/cleaned_data.csv")
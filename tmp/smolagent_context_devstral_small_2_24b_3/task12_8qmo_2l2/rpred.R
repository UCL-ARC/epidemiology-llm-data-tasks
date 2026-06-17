library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID")

# Define a function to collapse fractional NS-SEC codes to major categories
harmonise_nssec <- function(nssec_var) {
  # Ensure the input is numeric
  nssec_var <- as.numeric(nssec_var)
  
  # Collapse fractional codes to integer part
  nssec_major <- floor(nssec_var)
  
  # Apply standard missing-value codes
  nssec_major <- ifelse(nssec_major < 0, -3, nssec_major)
  
  return(nssec_major)
}

# Derive nssec17 from wave4
nssec17 <- harmonise_nssec(merged_data$W4nsseccatYP)

# Derive nssec18 from wave5
nssec18 <- harmonise_nssec(merged_data$W5nsseccatYP)

# Derive nssec19 from wave6
nssec19 <- harmonise_nssec(merged_data$w6nsseccatYP)

# Derive nssec20 from wave7
nssec20 <- harmonise_nssec(merged_data$W7NSSECCat)

# Derive nssec25 from wave8 with special handling for full-time students
data_with_nssec25 <- merged_data %>%
  mutate(
    nssec25_raw = harmonise_nssec(W8DNSSEC17),
    nssec25 = ifelse(
      W8DACTIVITYC == 5, 15,  # Assign category 15 if in full-time education
      nssec25_raw
    )
  )
nssec25 <- data_with_nssec25$nssec25

# Create the final cleaned dataset
cleaned_data <- tibble(
  NSID = merged_data$NSID,
  nssec17 = nssec17,
  nssec18 = nssec18,
  nssec19 = nssec19,
  nssec20 = nssec20,
  nssec25 = nssec25
)

# Write the output CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")

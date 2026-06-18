library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# File paths
file1 <- "data/input/wave_one_lsype_family_background_2020.tab"
file2 <- "data/input/wave_two_lsype_family_background_2020.tab"
file3 <- "data/input/wave_three_lsype_family_background_2020.tab"
file4 <- "data/input/wave_four_lsype_family_background_2020.tab"

# Load datasets
data1 <- readr::read_delim(file1, delim = "\t", col_types = readr::cols(.default = "numeric", NSID = readr::col_character()))
data2 <- readr::read_delim(file2, delim = "\t", col_types = readr::cols(.default = "numeric", NSID = readr::col_character()))
data3 <- readr::read_delim(file3, delim = "\t", col_types = readr::cols(.default = "numeric", NSID = readr::col_character()))
data4 <- readr::read_delim(file4, delim = "\t", col_types = readr::cols(.default = "numeric", NSID = readr::col_character()))

# Merge datasets
full_data <- data1 %>%
  full_join(data2, by = "NSID") %>%
  full_join(data3, by = "NSID") %>%
  full_join(data4, by = "NSID")

# Function to clean economic activity variables
clean_ecoact <- function(var) {
  # Map substantive codes (1-9) directly
  # Map specific missing codes to -3 as per requirements: -99, -98, -996
  # Others like -999, -94 map according to general guidance or are converted to -3 if NA
  
  res <- case_when(
    var >= 1 & var <= 9 ~ var,
    var == -99 ~ -3,
    var == -98 ~ -3,
    var == -996 ~ -3,
    var == -92 ~ -9, # Refusal
    var == -94 ~ -8, # Insufficient info
    var == -999 ~ -2, # Lost/Information lost
    TRUE ~ -3 # Default NA or others to -3
  )
  return(res)
}

# Process waves
final_data <- full_data %>%
  mutate(
    ecoactma14 = clean_ecoact(W1empsmum),
    ecoactpa14 = clean_ecoact(W1empsdad),
    ecoactma15 = clean_ecoact(W2empsmum),
    ecoactpa15 = clean_ecoact(W2empsdad),
    ecoactma16 = clean_ecoact(W3empsmum),
    ecoactpa16 = clean_ecoact(W3empsdad),
    ecoactma17 = clean_ecoact(w4empsmum),
    ecoactpa17 = clean_ecoact(w4empsdad)
  ) %>%
  select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Define labels for the 9-category scheme
labels_ecoact <- c(
  "1" = "Doing paid work for 30 or more hours a week",
  "2" = "Doing paid work for fewer than 30 hours a week",
  "3" = "Unemployed/ Looking for a job",
  "4" = "On a training course or scheme",
  "5" = "In full-time education/ at school",
  "6" = "Looking after the family/ household",
  "7" = "Retired from work altogether",
  "8" = "Sick/ disabled",
  "9" = "Other",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

# Apply labels to all 8 variables
vars_to_label <- names(final_data)[-1]
for(v in vars_to_label) {
  final_data[[v]] <- factor(final_data[[v]], levels = as.numeric(names(labels_ecoact)), labels = labels_ecoact)
}

# Write output
readr::write_csv(final_data, "data/output/cleaned_data.csv")
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load each file explicitly
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define the standard missing-value codes
standard_missing_codes <- c(
  `-9` = "Refusal",
  `-8` = "Don't know / insufficient information",
  `-7` = "Prefer not to say",
  `-3` = "Not asked at the fieldwork stage / not interviewed",
  `-2` = "Schedule not applicable / script error / information lost",
  `-1` = "Item not applicable"
)

# Function to map wave-specific missing codes to standard codes
map_missing <- function(x, wave) {
  if (wave == "wave1") {
    x <- na_if(x, -999.0)  # Missing - household data lost
    x <- na_if(x, -94.0)   # Insufficient information
    x <- na_if(x, -92.0)   # Refused
    x <- na_if(x, -91.0)   # Not applicable
    x <- na_if(x, -1.0)    # Don't know
  } else if (wave == "wave2") {
    x <- na_if(x, -998.0)  # Interviewer missed question
    x <- na_if(x, -997.0)  # Script error
    x <- na_if(x, -995.0)  # Missing history section data - unexplained
    x <- na_if(x, -99.0)   # YP not interviewed
    x <- na_if(x, -92.0)   # Refused
    x <- na_if(x, -91.0)   # Not applicable
    x <- na_if(x, -1.0)    # Don't Know
  } else if (wave == "wave4") {
    x <- na_if(x, -94.0)   # Insufficient information
    x <- na_if(x, -1.0)    # Don't know
  } else if (wave == "wave8") {
    x <- na_if(x, -9.0)    # Refused
    x <- na_if(x, -8.0)    # Insufficient information
    x <- na_if(x, -1.0)    # Not applicable
  } else if (wave == "wave9") {
    x <- na_if(x, -8.0)    # Insufficient information
  }
  return(x)
}

# Apply missing value mapping to each ethnicity variable
merged_data$W1ethnic2YP <- map_missing(merged_data$W1ethnic2YP, "wave1")
merged_data$W2ethnicYP <- map_missing(merged_data$W2ethnicYP, "wave2")
merged_data$w4ethnic2YP <- map_missing(merged_data$w4ethnic2YP, "wave4")
merged_data$W8DETHN15 <- map_missing(merged_data$W8DETHN15, "wave8")
merged_data$W9DETHN15 <- map_missing(merged_data$W9DETHN15, "wave9")

# Derive the consolidated ethnicity variable 'eth'
# Use earliest valid positive response first
merged_data$eth <- coalesce(
  ifelse(merged_data$W1ethnic2YP > 0, merged_data$W1ethnic2YP, NA_real_),
  ifelse(merged_data$W2ethnicYP > 0, merged_data$W2ethnicYP, NA_real_),
  ifelse(merged_data$w4ethnic2YP > 0, merged_data$w4ethnic2YP, NA_real_),
  ifelse(merged_data$W8DETHN15 > 0, merged_data$W8DETHN15, NA_real_),
  ifelse(merged_data$W9DETHN15 > 0, merged_data$W9DETHN15, NA_real_)
)

# Convert NA to -3 (Not asked at the fieldwork stage / not interviewed)
merged_data$eth[is.na(merged_data$eth)] <- -3

# Create labelled factor for 'eth'
eth_labels <- c(
  "1" = "White - British",
  "2" = "White - Irish",
  "3" = "Any other White background",
  "4" = "Mixed - White and Black Caribbean",
  "5" = "Mixed - White and Black African",
  "6" = "Mixed - White and Asian",
  "7" = "Any other mixed background",
  "8" = "Indian",
  "9" = "Pakistani",
  "10" = "Bangladeshi",
  "11" = "Any other Asian background",
  "12" = "Black Caribbean",
  "13" = "Black African",
  "14" = "Any other Black background",
  "15" = "Chinese",
  "16" = "Any other ethnic background",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

merged_data$eth <- factor(merged_data$eth, levels = as.numeric(names(eth_labels)), labels = eth_labels)

# Select only the ID variable and the final derived variable
output_data <- merged_data %>%
  select(NSID, eth)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"
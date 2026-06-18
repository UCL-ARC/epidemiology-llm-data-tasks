library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(var, value_labels) {
  var <- as.numeric(var)
  var[is.na(var)] <- -3
  
  for (code in names(value_labels)) {
    if (code %in% c("-92.0", "-92")) {
      var[var == as.numeric(code)] <- -9
    } else if (code %in% c("-91.0", "-91")) {
      var[var == as.numeric(code)] <- -1
    } else if (code %in% c("-97.0", "-97")) {
      var[var == as.numeric(code)] <- -7
    } else if (code %in% c("-1.0", "-1")) {
      var[var == as.numeric(code)] <- -1
    } else if (code %in% c("-997.0", "-997")) {
      var[var == as.numeric(code)] <- -2
    } else if (code %in% c("-999.0", "-999")) {
      var[var == as.numeric(code)] <- -2
    } else if (code %in% c("-9.0", "-9")) {
      var[var == as.numeric(code)] <- -9
    } else if (code %in% c("-8.0", "-8")) {
      var[var == as.numeric(code)] <- -8
    }
  }
  return(var)
}

# Derive partnr19 from W6MarStatYP
partnr19 <- merged_data$W6MarStatYP
value_labels_w6 <- c(
  "-997.0" = "Script error",
  "-97.0" = "Respondent declined self completion",
  "-92.0" = "Refused",
  "-91.0" = "Not applicable",
  "-1.0" = "Don't know",
  "1.0" = "Single, that is never married",
  "2.0" = "Married",
  "3.0" = "Separated",
  "4.0" = "Divorced",
  "5.0" = "Widowed"
)
partnr19 <- map_missing(partnr19, value_labels_w6)

# Map W6MarStatYP to harmonised categories
partnr19 <- case_when(
  partnr19 == 1 ~ 1,  # Single
  partnr19 == 2 ~ 2,  # Married
  partnr19 == 3 ~ 3,  # Separated
  partnr19 == 4 ~ 4,  # Divorced
  partnr19 == 5 ~ 5,  # Widowed
  partnr19 == -9 ~ -9,  # Refused
  partnr19 == -8 ~ -8,  # Don't know
  partnr19 == -7 ~ -7,  # Prefer not to say
  partnr19 == -3 ~ -3,  # Not asked
  partnr19 == -2 ~ -2,  # Not applicable
  partnr19 == -1 ~ -1,  # Item not applicable
  TRUE ~ -3
)

# Derive partnradu25 from W8DMARSTAT
partnradu25 <- merged_data$W8DMARSTAT
value_labels_w8 <- c(
  "-9.0" = "Refused",
  "-8.0" = "Insufficient information",
  "-1.0" = "Not applicable",
  "1.0" = "Single and never married or in a CP",
  "2.0" = "Married",
  "3.0" = "Separated but still legally married",
  "4.0" = "Divorced",
  "5.0" = "Widowed",
  "6.0" = "A Civil Partner",
  "7.0" = "Separated but still legally in a CP",
  "8.0" = "A former Civil Partner",
  "9.0" = "A surviving Civil Partner"
)
partnradu25 <- map_missing(partnradu25, value_labels_w8)

# Derive partnradu32 from W9DMARSTAT
partnradu32 <- merged_data$W9DMARSTAT
value_labels_w9 <- c(
  "-9.0" = "Refused",
  "-8.0" = "Insufficient information",
  "1.0" = "Single that is never married or never in a Civil Partnership",
  "2.0" = "Married",
  "3.0" = "Divorced",
  "4.0" = "Legally separated",
  "5.0" = "Widowed",
  "6.0" = "A Civil Partner in a legally recognised Civil Partnership",
  "7.0" = "A former Civil Partner (where Civil Partnership legally dissolved)",
  "8.0" = "A surviving Civil Partner (where Civil Partner has died)"
)
partnradu32 <- map_missing(partnradu32, value_labels_w9)

# Collapse partnradu25 to partnr25
partnr25 <- case_when(
  partnradu25 %in% c(1, 6, 7, 8, 9) ~ 1,  # Single or Civil Partner
  partnradu25 == 2 ~ 2,  # Married
  partnradu25 == 3 ~ 3,  # Separated
  partnradu25 == 4 ~ 4,  # Divorced
  partnradu25 == 5 ~ 5,  # Widowed
  partnradu25 == -9 ~ -9,  # Refused
  partnradu25 == -8 ~ -8,  # Don't know
  partnradu25 == -7 ~ -7,  # Prefer not to say
  partnradu25 == -3 ~ -3,  # Not asked
  partnradu25 == -2 ~ -2,  # Not applicable
  partnradu25 == -1 ~ -1,  # Item not applicable
  TRUE ~ -3
)

# Collapse partnradu32 to partnr32
partnr32 <- case_when(
  partnradu32 %in% c(1, 6, 7, 8) ~ 1,  # Single or Civil Partner
  partnradu32 == 2 ~ 2,  # Married
  partnradu32 == 4 ~ 3,  # Separated
  partnradu32 == 3 ~ 4,  # Divorced
  partnradu32 == 5 ~ 5,  # Widowed
  partnradu32 == -9 ~ -9,  # Refused
  partnradu32 == -8 ~ -8,  # Don't know
  partnradu32 == -7 ~ -7,  # Prefer not to say
  partnradu32 == -3 ~ -3,  # Not asked
  partnradu32 == -2 ~ -2,  # Not applicable
  partnradu32 == -1 ~ -1,  # Item not applicable
  TRUE ~ -3
)

# Create the final dataset
cleaned_data <- data.frame(
  NSID = merged_data$NSID,
  partnr19 = partnr19,
  partnradu25 = partnradu25,
  partnr25 = partnr25,
  partnradu32 = partnradu32,
  partnr32 = partnr32
)

# Write the cleaned data to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return the path to the cleaned data
"data/output/cleaned_data.csv"
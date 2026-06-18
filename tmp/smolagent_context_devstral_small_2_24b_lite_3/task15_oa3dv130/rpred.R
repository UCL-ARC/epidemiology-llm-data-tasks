library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define the standard missing-value codes
standard_missing <- c(
  `-9` = "Refusal",
  `-8` = "Don't know / insufficient information",
  `-7` = "Prefer not to say",
  `-3` = "Not asked at the fieldwork stage / not interviewed",
  `-2` = "Schedule not applicable / script error / information lost",
  `-1` = "Item not applicable"
)

# Function to map missing values
map_missing <- function(var, metadata) {
  if (is.numeric(var)) {
    var <- ifelse(var == -1.0, -1, var)
    var <- ifelse(var == -92, -9, var)
    var <- ifelse(var == -91, -1, var)
    var <- ifelse(var == -99, -3, var)
    var <- ifelse(var == -100, -3, var)
    var <- ifelse(var == -97, -3, var)
    var <- ifelse(var == -94, -8, var)
    var <- ifelse(var == -999 | var == -998 | var == -997 | var == -995, -2, var)
    var <- ifelse(is.na(var), -3, var)
  }
  return(var)
}

# Process W8DINCB for inc25
inc25 <- merged_data$W8DINCB
inc25 <- map_missing(inc25, wave8$variables$W8DINCB)
inc25 <- labelled::to_factor(inc25)
levels(inc25) <- c("less than 25", "25 to 50", "50 to 90", "90 to 140", "140 to 240", "240 to 300", "300 to 350", "350 to 400", "400 to 500", "500 to 600", "600 to 700", "700 to 800", "800 to 900", "900 to 1200", "1200 to 1400", "more than 1400", "Not applicable", "Refusal", "Don't know / insufficient information", "Prefer not to say", "Not asked at the fieldwork stage / not interviewed", "Schedule not applicable / script error / information lost")

# Process W9DINCB for inc32
inc32 <- merged_data$W9DINCB
inc32 <- map_missing(inc32, wave9$variables$W9DINCB)
inc32 <- labelled::to_factor(inc32)
levels(inc32) <- c("less than 25", "25 to 50", "50 to 90", "90 to 140", "140 to 240", "240 to 300", "300 to 350", "350 to 400", "400 to 500", "500 to 600", "600 to 700", "700 to 800", "800 to 900", "900 to 1200", "1200 to 1400", "more than 1400", "Not applicable", "Refusal", "Don't know / insufficient information", "Prefer not to say", "Not asked at the fieldwork stage / not interviewed", "Schedule not applicable / script error / information lost")

# Create the final dataset
cleaned_data <- data.frame(
  NSID = merged_data$NSID,
  inc25 = inc25,
  inc32 = inc32
)

# Write the output
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return the path to the cleaned data
"data/output/cleaned_data.csv"
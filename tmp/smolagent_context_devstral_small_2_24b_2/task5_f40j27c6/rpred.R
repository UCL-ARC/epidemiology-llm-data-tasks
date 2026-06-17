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

# Merge datasets
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Function to map missing values based on metadata
map_missing <- function(var) {
  var <- na_if(var, -999)
  var <- na_if(var, -998)
  var <- na_if(var, -997)
  var <- na_if(var, -995)
  var <- na_if(var, -97)
  var <- na_if(var, -94)
  var <- na_if(var, -92)
  var <- na_if(var, -91)
  var <- na_if(var, -99)
  var <- na_if(var, -9)
  var <- na_if(var, -8)
  var <- na_if(var, -7)
  var <- na_if(var, -3)
  var <- na_if(var, -2)
  var <- na_if(var, -1)
  var <- na_if(var, -100)
  var
}

# Derive partnr19 from W6MarStatYP
partnr19 <- merged_data$W6MarStatYP
partnr19 <- map_missing(partnr19)
partnr19 <- labelled::to_factor(partnr19, levels = c(1, 2, 3, 4, 5, -9, -8, -7, -3, -2, -1), labels = c("Single, never married", "Married", "Separated", "Divorced", "Widowed", "Refusal", "Don't know / insufficient information", "Prefer not to say", "Not asked at the fieldwork stage / not interviewed", "Schedule not applicable / script error / information lost", "Item not applicable"))

# Derive detailed adult variables partnradu25 from W8DMARSTAT and partnradu32 from W9DMARSTAT
partnradu25 <- merged_data$W8DMARSTAT
partnradu25 <- map_missing(partnradu25)
partnradu25 <- labelled::to_factor(partnradu25, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, -9, -8, -7, -3, -2, -1), labels = c("Single and never married or in a CP", "Married", "Separated but still legally married", "Divorced", "Widowed", "A Civil Partner", "Separated but still legally in a CP", "A former Civil Partner", "A surviving Civil Partner", "Refusal", "Don't know / insufficient information", "Prefer not to say", "Not asked at the fieldwork stage / not interviewed", "Schedule not applicable / script error / information lost", "Item not applicable"))

partnradu32 <- merged_data$W9DMARSTAT
partnradu32 <- map_missing(partnradu32)
partnradu32 <- labelled::to_factor(partnradu32, levels = c(1, 2, 3, 4, 5, 6, 7, 8, -9, -8, -7, -3, -2, -1), labels = c("Single that is never married or never in a Civil Partnership", "Married", "Divorced", "Legally separated", "Widowed", "A Civil Partner in a legally recognised Civil Partnership", "A former Civil Partner (where Civil Partnership legally dissolved)", "A surviving Civil Partner (where Civil Partner has died)", "Refusal", "Don't know / insufficient information", "Prefer not to say", "Not asked at the fieldwork stage / not interviewed", "Schedule not applicable / script error / information lost", "Item not applicable"))

# Collapse detailed adult variables into comparable partnr25 and partnr32 categories
partnr25 <- partnradu25
partnr25 <- labelled::to_factor(partnr25, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, -9, -8, -7, -3, -2, -1), labels = c("Single, never married", "Married", "Separated", "Divorced", "Widowed", "A Civil Partner", "Separated but still legally in a CP", "A former Civil Partner", "A surviving Civil Partner", "Refusal", "Don't know / insufficient information", "Prefer not to say", "Not asked at the fieldwork stage / not interviewed", "Schedule not applicable / script error / information lost", "Item not applicable"))

partnr32 <- partnradu32
partnr32 <- labelled::to_factor(partnr32, levels = c(1, 2, 3, 4, 5, 6, 7, 8, -9, -8, -7, -3, -2, -1), labels = c("Single, never married", "Married", "Divorced", "Separated", "Widowed", "A Civil Partner", "A former Civil Partner", "A surviving Civil Partner", "Refusal", "Don't know / insufficient information", "Prefer not to say", "Not asked at the fieldwork stage / not interviewed", "Schedule not applicable / script error / information lost", "Item not applicable"))

# Create the final dataset with only the required variables
cleaned_data <- tibble(
  NSID = merged_data$NSID,
  partnr19 = partnr19,
  partnr25 = partnr25,
  partnr32 = partnr32,
  partnradu25 = partnradu25,
  partnradu32 = partnradu32
)

# Write the cleaned data to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return the path to the cleaned data
"data/output/cleaned_data.csv"
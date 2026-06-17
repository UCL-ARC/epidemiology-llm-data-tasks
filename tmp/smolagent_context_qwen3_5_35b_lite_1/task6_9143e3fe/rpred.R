library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "data/input/wave_one_lsype_young_person_2020.tab",
  "data/input/wave_four_lsype_young_person_2020.tab",
  "data/input/wave_two_lsype_family_background_2020.tab",
  "data/input/wave_three_lsype_family_background_2020.tab",
  "data/input/ns8_2015_derived.tab",
  "data/input/ns9_2022_derived_variables.tab",
  "data/input/ns9_2022_main_interview.tab"
)

# Load all files
wave1 <- read_delim(files[1], delim = "\t", col_types = cols())
wave4 <- read_delim(files[2], delim = "\t", col_types = cols())
wave2 <- read_delim(files[3], delim = "\t", col_types = cols())
wave3 <- read_delim(files[4], delim = "\t", col_types = cols())
ns8 <- read_delim(files[5], delim = "\t", col_types = cols())
ns9_derived <- read_delim(files[6], delim = "\t", col_types = cols())
ns9_main <- read_delim(files[7], delim = "\t", col_types = cols())

# Rename duplicate columns before merging
wave2 <- wave2 %>%
  rename(urbind15 = urbind,
         gor15 = gor)

wave3 <- wave3 %>%
  rename(urbind16 = urbind,
         gor16 = gor)

# Merge all datasets by NSID
cleaned <- full_join(wave1, wave4, by = "NSID")
cleaned <- full_join(cleaned, wave2, by = "NSID")
cleaned <- full_join(cleaned, wave3, by = "NSID")
cleaned <- full_join(cleaned, ns8, by = "NSID")
cleaned <- full_join(cleaned, ns9_derived, by = "NSID")
cleaned <- full_join(cleaned, ns9_main, by = "NSID")

# Derive urban/rural variables for age 15 and 16
# Age 15: urbind15 from wave2
cleaned$regub15 <- cleaned$urbind15
# Map missing values for urbind (-94 = Insufficient information -> -8)
cleaned$regub15 <- case_when(
  cleaned$regub15 == -94 ~ -8,
  is.na(cleaned$regub15) ~ -3,
  TRUE ~ cleaned$regub15
)

# Age 16: urbind16 from wave3
cleaned$regub16 <- cleaned$urbind16
# Map missing values for urbind (-94 = Insufficient information -> -8)
cleaned$regub16 <- case_when(
  cleaned$regub16 == -94 ~ -8,
  is.na(cleaned$regub16) ~ -3,
  TRUE ~ cleaned$regub16
)

# Derive UK region variables
cleaned$regov15 <- cleaned$gor15
# Map missing values for gor at age 15 (-94 = Insufficient information -> -8)
cleaned$regov15 <- case_when(
  cleaned$regov15 == -94 ~ -8,
  is.na(cleaned$regov15) ~ -3,
  TRUE ~ cleaned$regov15
)

cleaned$regov16 <- cleaned$gor16
# Map missing values for gor at age 16 (-94 = Insufficient information -> -8)
cleaned$regov16 <- case_when(
  cleaned$regov16 == -94 ~ -8,
  is.na(cleaned$regov16) ~ -3,
  TRUE ~ cleaned$regov16
)

# Age 25: W8DGOR from ns8
cleaned$regor25 <- cleaned$W8DGOR
# Map missing values for W8DGOR
# -9 = Refused, -8 = Insufficient information, -1 = Not applicable
cleaned$regor25 <- case_when(
  cleaned$regor25 == -9 ~ -9,
  cleaned$regor25 == -8 ~ -8,
  cleaned$regor25 == -1 ~ -1,
  is.na(cleaned$regor25) ~ -3,
  TRUE ~ cleaned$regor25
)

# Age 32: W9DRGN from ns9_derived
cleaned$regor32 <- cleaned$W9DRGN
# Map missing values for W9DRGN
# -9 = Refused, -8 = Insufficient information, -1 = Not applicable
cleaned$regor32 <- case_when(
  cleaned$regor32 == -9 ~ -9,
  cleaned$regor32 == -8 ~ -8,
  cleaned$regor32 == -1 ~ -1,
  is.na(cleaned$regor32) ~ -3,
  TRUE ~ cleaned$regor32
)

# Age 32: regint32 from W9NATIONRES
cleaned$regint32 <- cleaned$W9NATIONRES
# Map missing values for W9NATIONRES
# -9 = Refused, -8 = Don't know, -3 = Not asked at fieldwork stage, -1 = Not applicable
cleaned$regint32 <- case_when(
  cleaned$regint32 == -9 ~ -9,
  cleaned$regint32 == -8 ~ -8,
  cleaned$regint32 == -3 ~ -3,
  cleaned$regint32 == -1 ~ -1,
  is.na(cleaned$regint32) ~ -3,
  TRUE ~ cleaned$regint32
)

# Create labels for categorical variables
label_values_urb <- c(
  "1" = "Urban >= 10k - sparse",
  "2" = "Town & Fringe - sparse",
  "3" = "Village - sparse",
  "4" = "Hamlet and Isolated Dwelling - sparse",
  "5" = "Urban >= 10k - less sparse",
  "6" = "Town & Fringe - less sparse",
  "7" = "Village - less sparse",
  "8" = "Hamlet & Isolated Dwelling",
  "-8" = "Insufficient information",
  "-3" = "Not asked at fieldwork stage"
)

# regov15 and regov16 (UK region - England only based on source)
label_values_gor <- c(
  "1" = "North East",
  "2" = "North West",
  "3" = "Yorkshire and The Humber",
  "4" = "East Midlands",
  "5" = "West Midlands",
  "6" = "East of England",
  "7" = "London",
  "8" = "South East",
  "9" = "South West",
  "-8" = "Insufficient information",
  "-3" = "Not asked at fieldwork stage"
)

# regor25 and regor32 (UK region - all UK)
label_values_gor_uk <- c(
  "1" = "North East",
  "2" = "North West",
  "3" = "Yorkshire and the Humber",
  "4" = "East Midlands",
  "5" = "West Midlands",
  "6" = "East of England",
  "7" = "London",
  "8" = "South East",
  "9" = "South West",
  "10" = "Wales",
  "11" = "Scotland",
  "12" = "Northern Ireland",
  "13" = "Unknown due to faulty/missing postcode",
  "-9" = "Refused",
  "-8" = "Insufficient information",
  "-1" = "Not applicable",
  "-3" = "Not asked at fieldwork stage"
)

# regint32 (UK vs abroad)
label_values_nationres <- c(
  "1" = "England",
  "2" = "Scotland",
  "3" = "Wales",
  "4" = "Northern Ireland",
  "5" = "Outside of UK or unknown",
  "-9" = "Refused",
  "-8" = "Don't know",
  "-3" = "Not asked at fieldwork stage",
  "-1" = "Not applicable"
)

# Apply labels to variables
labelled::set_variable_labels(cleaned$regub15, "Urban/Rural Indicator (England) - Age 15")
labelled::set_variable_labels(cleaned$regub16, "Urban/Rural Indicator (England) - Age 16")
labelled::set_variable_labels(cleaned$regov15, "Government Office Region - Age 15")
labelled::set_variable_labels(cleaned$regov16, "Government Office Region - Age 16")
labelled::set_variable_labels(cleaned$regor25, "Government Office Region - Age 25")
labelled::set_variable_labels(cleaned$regor32, "Government Office Region - Age 32")
labelled::set_variable_labels(cleaned$regint32, "Nation of UK / Residence Status - Age 32")

# Keep only required variables
output <- cleaned %>%
  select(NSID, regub15, regub16, regov15, regov16, regor25, regor32, regint32)

# Write output
write_csv(output, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")

library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all files by NSID using full_join
cleaned <- full_join(wave1, wave4, by = "NSID")
cleaned <- full_join(cleaned, wave6, by = "NSID")
cleaned <- full_join(cleaned, ns8, by = "NSID")
cleaned <- full_join(cleaned, ns9, by = "NSID")

# Process W6MarStatYP (Age 19) - map missing value codes by label meaning
cleaned$W6MarStatYP_harm <- cleaned$W6MarStatYP
cleaned$W6MarStatYP_harm[cleaned$W6MarStatYP_harm == -997] <- -2  # Script error
cleaned$W6MarStatYP_harm[cleaned$W6MarStatYP_harm == -97] <- -3   # Respondent declined self completion
cleaned$W6MarStatYP_harm[cleaned$W6MarStatYP_harm == -92] <- -9   # Refused
cleaned$W6MarStatYP_harm[cleaned$W6MarStatYP_harm == -91] <- -1   # Not applicable
cleaned$W6MarStatYP_harm[cleaned$W6MarStatYP_harm == -1] <- -8    # Don't know

# Create partnr19 variable
cleaned$partnr19 <- cleaned$W6MarStatYP_harm
attr(cleaned$partnr19, "labels") <- c(
  "1" = "Single, that is never married",
  "2" = "Married",
  "3" = "Separated",
  "4" = "Divorced",
  "5" = "Widowed",
  "-9" = "Refused",
  "-8" = "Don't know",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

# Process W8DMARSTAT (Age 25) - map missing value codes by label meaning
cleaned$W8DMARSTAT_harm <- cleaned$W8DMARSTAT
cleaned$W8DMARSTAT_harm[cleaned$W8DMARSTAT_harm == -9] <- -9   # Refused
cleaned$W8DMARSTAT_harm[cleaned$W8DMARSTAT_harm == -8] <- -8   # Insufficient information
cleaned$W8DMARSTAT_harm[cleaned$W8DMARSTAT_harm == -1] <- -1   # Not applicable

# Create partnr25 variable
cleaned$partnr25 <- cleaned$W8DMARSTAT_harm
attr(cleaned$partnr25, "labels") <- c(
  "1" = "Single and never married or in a CP",
  "2" = "Married",
  "3" = "Separated but still legally married",
  "4" = "Divorced",
  "5" = "Widowed",
  "6" = "A Civil Partner",
  "7" = "Separated but still legally in a CP",
  "8" = "A former Civil Partner",
  "9" = "A surviving Civil Partner",
  "-9" = "Refused",
  "-8" = "Insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

# Create partnradu25 variable (collapsed adult partnership status)
cleaned$partnradu25 <- cleaned$W8DMARSTAT_harm
cleaned$partnradu25[cleaned$partnradu25 == 1] <- 1  # Single
cleaned$partnradu25[!cleaned$partnradu25 %in% c(1, -9, -8, -7, -3, -2, -1)] <- 2  # Partnered (married or civil partner)
attr(cleaned$partnradu25, "labels") <- c(
  "1" = "Single",
  "2" = "Partnered (married or civil partner)",
  "-9" = "Refused",
  "-8" = "Insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

# Process W9DMARSTAT (Age 32) - map missing value codes by label meaning
cleaned$W9DMARSTAT_harm <- cleaned$W9DMARSTAT
cleaned$W9DMARSTAT_harm[cleaned$W9DMARSTAT_harm == -9] <- -9   # Refused
cleaned$W9DMARSTAT_harm[cleaned$W9DMARSTAT_harm == -8] <- -8   # Insufficient information

# Create partnr32 variable
cleaned$partnr32 <- cleaned$W9DMARSTAT_harm
attr(cleaned$partnr32, "labels") <- c(
  "1" = "Single that is never married or never in a Civil Partnership",
  "2" = "Married",
  "3" = "Divorced",
  "4" = "Legally separated",
  "5" = "Widowed",
  "6" = "A Civil Partner in a legally recognised Civil Partnership",
  "7" = "A former Civil Partner (where Civil Partnership legally dissolved)",
  "8" = "A surviving Civil Partner (where Civil Partner has died)",
  "-9" = "Refused",
  "-8" = "Insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

# Create partnradu32 variable (collapsed adult partnership status)
cleaned$partnradu32 <- cleaned$W9DMARSTAT_harm
cleaned$partnradu32[cleaned$partnradu32 == 1] <- 1  # Single
cleaned$partnradu32[!cleaned$partnradu32 %in% c(1, -9, -8, -7, -3, -2, -1)] <- 2  # Partnered (married or civil partner)
attr(cleaned$partnradu32, "labels") <- c(
  "1" = "Single",
  "2" = "Partnered (married or civil partner)",
  "-9" = "Refused",
  "-8" = "Insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

# Remove intermediate working variables
rm(W6MarStatYP_harm, W8DMARSTAT_harm, W9DMARSTAT_harm)

# Keep only ID and final derived variables
cleaned <- cleaned %>% select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Write output CSV
write_csv(cleaned, "data/output/cleaned_data.csv")

cat("Script completed successfully!\n")
cat("Output file: data/output/cleaned_data.csv\n")
cat("Variables in output:\n")
print(names(cleaned))

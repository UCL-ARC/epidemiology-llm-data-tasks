library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from the metadata
# Wave 1 (Age 14) - Young Person Data File
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", 
                     delim = "\t", show_col_types = FALSE)

# Wave 2 (Age 15) - Family Background Data File
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", 
                     delim = "\t", show_col_types = FALSE)

# Wave 3 (Age 16) - Family Background Data File
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", 
                     delim = "\t", show_col_types = FALSE)

# Wave 4 (Age 17) - Young Person Data File
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", 
                     delim = "\t", show_col_types = FALSE)

# Wave 8 (Age 25) - Derived Data File
ns8 <- read_delim("data/input/ns8_2015_derived.tab", 
                   delim = "\t", show_col_types = FALSE)

# Wave 9 (Age 32) - Derived Variables Data File
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", 
                           delim = "\t", show_col_types = FALSE)

# Wave 9 (Age 32) - Main Interview Data File
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", 
                        delim = "\t", show_col_types = FALSE)

# Extract relevant columns and rename for merging
cleaned <- wave1 %>% select(NSID)

# Add Age 15 variables from wave2
cleaned <- cleaned %>%
  full_join(wave2 %>% select(NSID, urbind_15 = urbind, gor_15 = gor), by = "NSID")

# Add Age 16 variables from wave3
cleaned <- cleaned %>%
  full_join(wave3 %>% select(NSID, urbind_16 = urbind, gor_16 = gor), by = "NSID")

# Add Age 25 variables from ns8
cleaned <- cleaned %>%
  full_join(ns8 %>% select(NSID, W8DGOR_25 = W8DGOR), by = "NSID")

# Add Age 32 variables from ns9_derived and ns9_main
cleaned <- cleaned %>%
  full_join(ns9_derived %>% select(NSID, W9DRGN_32 = W9DRGN), by = "NSID") %>%
  full_join(ns9_main %>% select(NSID, W9NATIONRES_32 = W9NATIONRES), by = "NSID")

# Derive the final variables with correct missing value codes
cleaned <- cleaned %>%
  mutate(
    regub15 = urbind_15,
    regub16 = urbind_16,
    regov15 = gor_15,
    regov16 = gor_16,
    regor25 = W8DGOR_25,
    regor32 = W9DRGN_32,
    regint32 = W9NATIONRES_32
  )

# Fix missing value codes for regub15 and regub16: -94 = Insufficient information -> -8
cleaned$regub15[cleaned$regub15 == -94] <- -8
cleaned$regub16[cleaned$regub16 == -94] <- -8

# For regov15 and regov16: -94 = Insufficient information -> -8
cleaned$regov15[cleaned$regov15 == -94] <- -8
cleaned$regov16[cleaned$regov16 == -94] <- -8

# Convert all NA values to -3 (Not asked at fieldwork stage) for all derived variables
cleaned$regub15[is.na(cleaned$regub15)] <- -3
cleaned$regub16[is.na(cleaned$regub16)] <- -3
cleaned$regov15[is.na(cleaned$regov15)] <- -3
cleaned$regov16[is.na(cleaned$regov16)] <- -3

# For regor25: -9=Refused, -8=Insufficient info, -1=Not applicable
cleaned$regor25[cleaned$regor25 == -9] <- -9
cleaned$regor25[cleaned$regor25 == -8] <- -8
cleaned$regor25[cleaned$regor25 == -1] <- -1
cleaned$regor25[is.na(cleaned$regor25)] <- -3

# For regor32: -9=Refused, -8=Insufficient info, -1=Not applicable
cleaned$regor32[cleaned$regor32 == -9] <- -9
cleaned$regor32[cleaned$regor32 == -8] <- -8
cleaned$regor32[cleaned$regor32 == -1] <- -1
cleaned$regor32[is.na(cleaned$regor32)] <- -3

# For regint32: 1=England, 2=Scotland, 3=Wales, 4=Northern Ireland, 5=Outside UK/unknown
# Map to: 1=UK (England, Scotland, Wales, Northern Ireland), 2=Outside UK/unknown
cleaned$regint32[cleaned$regint32 == 1] <- 1
cleaned$regint32[cleaned$regint32 == 2] <- 1
cleaned$regint32[cleaned$regint32 == 3] <- 1
cleaned$regint32[cleaned$regint32 == 4] <- 1
cleaned$regint32[cleaned$regint32 == 5] <- 2
# Missing codes: -9=Refused, -8=Don\'t know, -3=Not asked, -1=Not applicable
cleaned$regint32[cleaned$regint32 == -9] <- -9
cleaned$regint32[cleaned$regint32 == -8] <- -8
cleaned$regint32[cleaned$regint32 == -3] <- -3
cleaned$regint32[cleaned$regint32 == -1] <- -1
cleaned$regint32[is.na(cleaned$regint32)] <- -3

# Select only NSID and the derived variables
cleaned <- cleaned %>%
  select(NSID, regub15, regub16, regov15, regov16, regor25, regor32, regint32)

# Add labels to variables
labelled::set_variable_labels(cleaned, 
  NSID = "NSID - cohort member identifier",
  regub15 = "Urban/Rural Indicator (Age 15)",
  regub16 = "Urban/Rural Indicator (Age 16)",
  regov15 = "Government Office Region (Age 15)",
  regov16 = "Government Office Region (Age 16)",
  regor25 = "Government Office Region (Age 25)",
  regor32 = "Government Office Region (Age 32)",
  regint32 = "UK/International Status (Age 32)"
)

# Write output CSV
write_csv(cleaned, "data/output/cleaned_data.csv")

cat("Done! Output written to data/output/cleaned_data.csv\n")

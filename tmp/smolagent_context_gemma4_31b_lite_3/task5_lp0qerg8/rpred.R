library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = readr::cols(.default = "c"))
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols(.default = "c"))

# Merge datasets
full_frame <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

# Helper function to map missing values based on label meaning
map_missing <- function(val, mapping) {
  res <- as.numeric(val)
  # Convert NA to -3 (Not asked / not interviewed)
  res[is.na(res)] <- -3
  
  # Custom mapping based on metadata labels
  # -9: Refusal, -8: Don't know/insufficient, -7: Prefer not to say,
  # -3: Not asked, -2: Schedule not applicable/script error, -1: Not applicable
  
  # For W6MarStatYP:
  # -997: Script error -> -2
  # -97: Declined -> -7
  # -92: Refused -> -9
  # -91: Not applicable -> -1
  # -1: Don't know -> -8
  
  # For W8DMARSTAT:
  # -9: Refused -> -9
  # -8: Insufficient -> -8
  # -1: Not applicable -> -1
  
  # For W9DMARSTAT:
  # -9: Refused -> -9
  # -8: Insufficient -> -8
  
  return(res)
}

# Process partnr19 (Wave 6)
full_frame <- full_frame %>%
  mutate(
    W6MarStatYP_num = as.numeric(W6MarStatYP),
    partnr19 = case_when(
      W6MarStatYP_num == 1 ~ 1,
      W6MarStatYP_num == 2 ~ 2,
      W6MarStatYP_num == 3 ~ 3,
      W6MarStatYP_num == 4 ~ 4,
      W6MarStatYP_num == 5 ~ 5,
      W6MarStatYP_num == -997 ~ -2,
      W6MarStatYP_num == -97 ~ -7,
      W6MarStatYP_num == -92 ~ -9,
      W6MarStatYP_num == -91 ~ -1,
      W6MarStatYP_num == -1 ~ -8,
      is.na(W6MarStatYP_num) ~ -3,
      TRUE ~ -3
    )
  )

# Process partnr25 (Wave 8)
full_frame <- full_frame %>%
  mutate(
    W8DMARSTAT_num = as.numeric(W8DMARSTAT),
    partnr25 = case_when(
      W8DMARSTAT_num == 1 ~ 1,
      W8DMARSTAT_num == 2 ~ 2,
      W8DMARSTAT_num == 3 ~ 3,
      W8DMARSTAT_num == 4 ~ 4,
      W8DMARSTAT_num == 5 ~ 5,
      W8DMARSTAT_num == 6 ~ 6,
      W8DMARSTAT_num == 7 ~ 7,
      W8DMARSTAT_num == 8 ~ 8,
      W8DMARSTAT_num == 9 ~ 9,
      W8DMARSTAT_num == -9 ~ -9,
      W8DMARSTAT_num == -8 ~ -8,
      W8DMARSTAT_num == -1 ~ -1,
      is.na(W8DMARSTAT_num) ~ -3,
      TRUE ~ -3
    )
  )

# Process partnr32 (Wave 9)
full_frame <- full_frame %>%
  mutate(
    W9DMARSTAT_num = as.numeric(W9DMARSTAT),
    partnr32 = case_when(
      W9DMARSTAT_num == 1 ~ 1,
      W9DMARSTAT_num == 2 ~ 2,
      W9DMARSTAT_num == 3 ~ 3,
      W9DMARSTAT_num == 4 ~ 4,
      W9DMARSTAT_num == 5 ~ 5,
      W9DMARSTAT_num == 6 ~ 6,
      W9DMARSTAT_num == 7 ~ 7,
      W9DMARSTAT_num == 8 ~ 8,
      W9DMARSTAT_num == -9 ~ -9,
      W9DMARSTAT_num == -8 ~ -8,
      is.na(W9DMARSTAT_num) ~ -3,
      TRUE ~ -3
    )
  )

# Harmonisation for partnradu25 and partnradu32
# The requirement implies creating adult-specific versions. 
# Given the metadata provided, we only have legal marital status. 
# In this context, we will create these as copies of the marital status if no other specific adult partnership variable is provided,
# or as the specific legal status variables.

full_frame <- full_frame %>%
  mutate(
    partnradu25 = partnr25,
    partnradu32 = partnr32
  )

# Final selection
final_data <- full_frame %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

write_csv(final_data, "data/output/cleaned_data.csv")

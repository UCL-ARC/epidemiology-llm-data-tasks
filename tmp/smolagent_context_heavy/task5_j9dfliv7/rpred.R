library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Prepend path to filenames
files <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_eight = "data/input/ns8_2015_derived.tab",
  wave_nine = "data/input/ns9_2022_derived_variables.tab"
)

# Load files explicitly
data1 <- read_delim(files$wave_one, delim = "\t", col_types = cols(.default = "c"))
data4 <- read_delim(files$wave_four, delim = "\t", col_types = cols(.default = "c"))
data6 <- read_delim(files$wave_six, delim = "\t", col_types = cols(.default = "c"))
data8 <- read_delim(files$wave_eight, delim = "\t", col_types = cols(.default = "c"))
data9 <- read_delim(files$wave_nine, delim = "\t", col_types = cols(.default = "c"))

# Merge datasets using full_join by NSID
full_df <- data1 %>%
  full_join(data4, by = "NSID") %>%
  full_join(data6, by = "NSID") %>%
  full_join(data8, by = "NSID") %>%
  full_join(data9, by = "NSID")

# Helper for missing value mapping based on provided rules
# -9 = Refusal, -8 = Don't know/insufficient, -7 = Prefer not to say, 
# -3 = Not asked, -2 = Schedule not applicable/script error, -1 = Not applicable

# --- Processing partnr19 (W6MarStatYP) ---
# W6MarStatYP Labels: -997: Script error (-2), -97: Declined (-7), -92: Refused (-9), -91: Not applicable (-1), -1: Don't know (-8)
full_df <- full_df %>%
  mutate(W6MarStatYP = as.numeric(W6MarStatYP)) %>%
  mutate(partnr19 = case_when(
    W6MarStatYP == 1 ~ 1, # Single
    W6MarStatYP == 2 ~ 2, # Married
    W6MarStatYP == 3 ~ 3, # Separated
    W6MarStatYP == 4 ~ 4, # Divorced
    W6MarStatYP == 5 ~ 5, # Widowed
    W6MarStatYP == -997 ~ -2,
    W6MarStatYP == -97 ~ -7,
    W6MarStatYP == -92 ~ -9,
    W6MarStatYP == -91 ~ -1,
    W6MarStatYP == -1 ~ -8,
    TRUE ~ -3
  ))

# --- Processing partnradu25 (W8DMARSTAT) ---
# W8DMARSTAT Labels: -9: Refused (-9), -8: Insufficient (-8), -1: Not applicable (-1)
full_df <- full_df %>%
  mutate(W8DMARSTAT = as.numeric(W8DMARSTAT)) %>%
  mutate(partnradu25 = case_when(
    W8DMARSTAT == 1 ~ 1, # Single/CP
    W8DMARSTAT == 2 ~ 2, # Married
    W8DMARSTAT == 3 ~ 3, # Separated married
    W8DMARSTAT == 4 ~ 4, # Divorced
    W8DMARSTAT == 5 ~ 5, # Widowed
    W8DMARSTAT == 6 ~ 6, # Civil Partner
    W8DMARSTAT == 7 ~ 7, # Separated CP
    W8DMARSTAT == 8 ~ 8, # Former CP
    W8DMARSTAT == 9 ~ 9, # Surviving CP
    W8DMARSTAT == -9 ~ -9,
    W8DMARSTAT == -8 ~ -8,
    W8DMARSTAT == -1 ~ -1,
    TRUE ~ -3
  ))

# --- Processing partnradu32 (W9DMARSTAT) ---
# W9DMARSTAT Labels: -9: Refused (-9), -8: Insufficient (-8)
full_df <- full_df %>%
  mutate(W9DMARSTAT = as.numeric(W9DMARSTAT)) %>%
  mutate(partnradu32 = case_when(
    W9DMARSTAT == 1 ~ 1, # Single
    W9DMARSTAT == 2 ~ 2, # Married
    W9DMARSTAT == 3 ~ 3, # Divorced
    W9DMARSTAT == 4 ~ 4, # Separated
    W9DMARSTAT == 5 ~ 5, # Widowed
    W9DMARSTAT == 6 ~ 6, # Civil Partner
    W9DMARSTAT == 7 ~ 7, # Former CP
    W9DMARSTAT == 8 ~ 8, # Surviving CP
    W9DMARSTAT == -9 ~ -9,
    W9DMARSTAT == -8 ~ -8,
    TRUE ~ -3
  ))

# --- Harmonisation for partnr25 and partnr32 ---
# Need to map the detailed adult variables to the collapsed scheme used in partnr19/general
# Scheme: 1=Single, 2=Married/CP, 3=Separated, 4=Divorced/Former CP, 5=Widowed/Surviving CP

full_df <- full_df %>%
  mutate(partnr25 = case_when(
    partnradu25 == 1 ~ 1,
    partnradu25 %in% c(2, 6) ~ 2,
    partnradu25 %in% c(3, 7) ~ 3,
    partnradu25 %in% c(4, 8) ~ 4,
    partnradu25 %in% c(5, 9) ~ 5,
    partnradu25 == -9 ~ -9,
    partnradu25 == -8 ~ -8,
    partnradu25 == -1 ~ -1,
    partnradu25 == -3 ~ -3,
    TRUE ~ -3
  )) %>%
  mutate(partnr32 = case_when(
    partnradu32 == 1 ~ 1,
    partnradu32 %in% c(2, 6) ~ 2,
    partnradu32 == 4 ~ 3, # Legally separated
    partnradu32 %in% c(3, 7) ~ 4, # Divorced / Former CP
    partnradu32 %in% c(5, 8) ~ 5, # Widowed / Surviving CP
    partnradu32 == -9 ~ -9,
    partnradu32 == -8 ~ -8,
    partnradu32 == -3 ~ -3,
    TRUE ~ -3
  ))

# Factor Labels
common_labels <- c("Single" = "1", "Married/Partnered" = "2", "Separated" = "3", "Divorced" = "4", "Widowed" = "5", "Refusal" = "-9", "Don't know" = "-8", "Prefer not to say" = "-7", "Not asked" = "-3", "Not applicable" = "-1", "Script error" = "-2")

# Final selection
final_df <- full_df %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Write output
write_csv(final_df, "data/output/cleaned_data.csv")

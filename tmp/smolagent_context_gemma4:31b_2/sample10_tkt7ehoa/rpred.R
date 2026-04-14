library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Define files from metadata
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_five_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

# Load datasets
load_data <- function(filename) {
  read_delim(paste0('data/input/', filename), delim = '\t', col_types = cols(.default = "c"))
}

# Read all and merge
data_list <- map(files, load_data)
full_df <- reduce(data_list, full_join, by = 'NSID')

# Convert target variables to numeric for processing
# Wave 4 (Age 17)
full_df$W4empsYP <- as.numeric(full_df$W4empsYP)
# Wave 5 (Age 18)
full_df$W5mainactYP <- as.numeric(full_df$W5mainactYP)
# Wave 6 (Age 19)
full_df$W6TCurrentAct <- as.numeric(full_df$W6TCurrentAct)
# Wave 7 (Age 20)
full_df$W7TCurrentAct <- as.numeric(full_df$W7TCurrentAct)
# Wave 8 (Age 25)
full_df$W8DACTIVITYC <- as.numeric(full_df$W8DACTIVITYC)
# Wave 9 (Age 32)
full_df$W9DACTIVITYC <- as.numeric(full_df$W9DACTIVITYC)

# Standard Missing Value Logic
# -9 Refusal, -8 Don't know, -1 Not applicable, -3 Not asked/NA, -2 Script error/Lost
harmonize_missing <- function(x) {
  x <- as.numeric(x)
  case_when(
    is.na(x) ~ -3,
    x == -999 ~ -2,
    x == -998 ~ -2,
    x == -997 ~ -2,
    x == -995 ~ -2,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -99 ~ -3,
    TRUE ~ x
  )
}

# Define common categories for ecoact (collapsed)
# 1: In paid work
# 2: Education/Training
# 3: Unemployed
# 4: Looking after home/family
# 5: Sick/Disabled
# 6: Other/Not defined

# --- Processing each wave ---

# Age 17 (Wave 4)
full_df <- full_df %>%
  mutate(
    ecoact17 = case_when(
      W4empsYP == 1 ~ 1,
      W4empsYP == 2 ~ 1,
      W4empsYP == 3 ~ 3,
      W4empsYP == 4 ~ 2,
      W4empsYP == 5 ~ 2,
      W4empsYP == 6 ~ 4,
      W4empsYP == 7 ~ 6,
      W4empsYP == 8 ~ 5,
      W4empsYP == 9 ~ 6,
      W4empsYP == -92 ~ -9,
      W4empsYP == -94 ~ -8,
      W4empsYP == -91 ~ -1,
      W4empsYP <= -99 ~ -2,
      TRUE ~ -3
    )
  )

# Age 18 (Wave 5)
full_df <- full_df %>%
  mutate(
    ecoactadu18 = case_when(
      W5mainactYP == 1 ~ 1,
      W5mainactYP == 2 ~ 2,
      W5mainactYP == 3 ~ 3,
      W5mainactYP == 4 ~ 4,
      W5mainactYP == 5 ~ 5,
      W5mainactYP == 6 ~ 6,
      W5mainactYP == 7 ~ 7,
      W5mainactYP == 8 ~ 8,
      W5mainactYP == 9 ~ 9,
      W5mainactYP == 10 ~ 10,
      W5mainactYP == 11 ~ 11,
      W5mainactYP == -94 ~ -8,
      W5mainactYP <= -99 ~ -2,
      TRUE ~ -3
    ),
    ecoact18 = case_when(
      W5mainactYP == 3 ~ 1,
      W5mainactYP %in% c(1, 2, 4, 5, 6) ~ 2,
      W5mainactYP == 7 ~ 3,
      W5mainactYP == 8 ~ 4,
      W5mainactYP %in% c(9, 10, 11) ~ 6,
      W5mainactYP == -94 ~ -8,
      W5mainactYP <= -99 ~ -2,
      TRUE ~ -3
    )
  )

# Age 19 (Wave 6)
full_df <- full_df %>%
  mutate(
    ecoact19 = case_when(
      W6TCurrentAct == 3 ~ 1,
      W6TCurrentAct %in% c(1, 2, 4, 5, 10) ~ 2,
      W6TCurrentAct == 8 ~ 3,
      W6TCurrentAct == 7 ~ 4,
      W6TCurrentAct %in% c(6, 9, 11) ~ 6,
      W6TCurrentAct == -91 ~ -1,
      W6TCurrentAct <= -99 ~ -2,
      TRUE ~ -3
    )
  )

# Age 20 (Wave 7)
full_df <- full_df %>%
  mutate(
    ecoact20 = case_when(
      W7TCurrentAct %in% c(3, 9) ~ 1,
      W7TCurrentAct %in% c(1, 2, 4, 5, 11, 13) ~ 2,
      W7TCurrentAct == 8 ~ 3,
      W7TCurrentAct == 7 ~ 4,
      W7TCurrentAct == 14 ~ 5,
      W7TCurrentAct %in% c(6, 10, 12, 15) ~ 6,
      W7TCurrentAct == -91 ~ -1,
      W7TCurrentAct <= -99 ~ -2,
      TRUE ~ -3
    )
  )

# Age 25 (Wave 8)
full_df <- full_df %>%
  mutate(
    ecoactadu25 = case_when(
      W8DACTIVITYC == 1 ~ 1,
      W8DACTIVITYC == 2 ~ 2,
      W8DACTIVITYC == 3 ~ 3,
      W8DACTIVITYC == 4 ~ 4,
      W8DACTIVITYC == 5 ~ 5,
      W8DACTIVITYC == 6 ~ 6,
      W8DACTIVITYC == 7 ~ 7,
      W8DACTIVITYC == 8 ~ 8,
      W8DACTIVITYC == 9 ~ 9,
      W8DACTIVITYC == 10 ~ 10,
      W8DACTIVITYC == -9 ~ -9,
      W8DACTIVITYC == -8 ~ -8,
      W8DACTIVITYC == -1 ~ -1,
      TRUE ~ -3
    ),
    ecoact25 = case_when(
      W8DACTIVITYC %in% c(1, 2) ~ 1,
      W8DACTIVITYC %in% c(5, 6, 7) ~ 2,
      W8DACTIVITYC == 4 ~ 3,
      W8DACTIVITYC == 9 ~ 4,
      W8DACTIVITYC == 8 ~ 5,
      W8DACTIVITYC %in% c(3, 10) ~ 6,
      W8DACTIVITYC == -9 ~ -9,
      W8DACTIVITYC == -8 ~ -8,
      W8DACTIVITYC == -1 ~ -1,
      TRUE ~ -3
    )
  )

# Age 32 (Wave 9)
full_df <- full_df %>%
  mutate(
    ecoactadu32 = case_when(
      W9DACTIVITYC == 1 ~ 1,
      W9DACTIVITYC == 2 ~ 2,
      W9DACTIVITYC == 3 ~ 3,
      W9DACTIVITYC == 4 ~ 4,
      W9DACTIVITYC == 5 ~ 5,
      W9DACTIVITYC == 6 ~ 6,
      W9DACTIVITYC == 7 ~ 7,
      W9DACTIVITYC == 8 ~ 8,
      W9DACTIVITYC == 9 ~ 9,
      W9DACTIVITYC == 10 ~ 10,
      W9DACTIVITYC == -9 ~ -9,
      W9DACTIVITYC == -8 ~ -8,
      W9DACTIVITYC == -1 ~ -1,
      TRUE ~ -3
    ),
    ecoact32 = case_when(
      W9DACTIVITYC %in% c(1, 2) ~ 1,
      W9DACTIVITYC %in% c(5, 6, 7) ~ 2,
      W9DACTIVITYC == 4 ~ 3,
      W9DACTIVITYC == 9 ~ 4,
      W9DACTIVITYC == 8 ~ 5,
      W9DACTIVITYC %in% c(3, 10) ~ 6,
      W9DACTIVITYC == -9 ~ -9,
      W9DACTIVITYC == -8 ~ -8,
      W9DACTIVITYC == -1 ~ -1,
      TRUE ~ -3
    )
  )

# Convert to factors
vars_to_factor <- c("ecoact17", "ecoact18", "ecoact19", "ecoact20", "ecoact25", "ecoact32", "ecoactadu18", "ecoactadu25", "ecoactadu32")
full_df <- full_df %>%
  mutate(across(all_of(vars_to_factor), as.factor))

# Final Selection
final_data <- full_df %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu18, ecoactadu25, ecoactadu32)

# Output
write_csv(final_data, 'data/output/cleaned_data.csv')

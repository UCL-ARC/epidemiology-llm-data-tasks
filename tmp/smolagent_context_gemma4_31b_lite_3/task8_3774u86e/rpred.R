library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'character'))
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'character'))
ns8_main <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t', col_types = readr::cols(.default = 'character'))
ns8_der <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = readr::cols(.default = 'character'))
ns9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', col_types = readr::cols(.default = 'character'))
ns9_der <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = readr::cols(.default = 'character'))

# Merge all
full_frame <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(ns8_main, by = 'NSID') %>%
  full_join(ns8_der, by = 'NSID') %>%
  full_join(ns9_main, by = 'NSID') %>%
  full_join(ns9_der, by = 'NSID')

# Conversion helper for binary Yes/No to numeric
bin_yes_no <- function(x) {
  x <- as.numeric(x)
  res <- rep(NA, length(x))
  res[x == 1] <- 1 # Yes
  res[x == 2] <- 0 # No
  return(res)
}

# --- AGE 25 (Wave 8) ---
# Source for educ25: W8DHANVQH (Derived academic) and W8VCQU series (Vocational)

# Detailed Vocational 25 (educvdtl25 - though not explicitly asked for, we need it for harmonisation)
# But requirements only ask for educ25, educ32, educadtl32, educvdtl32.

# Map W8DHANVQH (Academic NVQ Level)
# Labels: -9 Refused, -8 Insufficient, -1 Not applicable, 1:L1, 2:L2, 3:L3, 4:L4, 5:L5, 95:Other, 96:None
# Standard Missing: -9 Refusal, -8 DK, -7 Prefer not, -3 Not asked, -2 Not applicable, -1 Item not applicable

# Function to harmonise NVQ 5-level scheme
# 1: NVQ 1, 2: NVQ 2, 3: NVQ 3, 4: NVQ 4, 5: NVQ 5, 0: No qual/None

map_nvq_5level <- function(val) {
  val <- as.numeric(val)
  res <- rep(-3, length(val))
  res[is.na(val)] <- -3
  
  # Valid substantive
  res[val == 1] <- 1
  res[val == 2] <- 2
  res[val == 3] <- 3
  res[val == 4] <- 4
  res[val == 5] <- 5
  res[val == 96] <- 0
  res[val == 95] <- 0 # Other academic mapped to 0 or lowest? Usually non-NVQ is 0.
  
  # Missing mapping
  res[val == -9] <- -9
  res[val == -8] <- -8
  res[val == -1] <- -1
  
  return(res)
}

# For Age 25, we have derived academic W8DHANVQH. 
# We also need to check vocational. 
# W8VCQU0J is NVQ L3-5, W8VCQU0I is NVQ L1-2.

# Detailed Vocational 25 (derived for internal use to calculate educ25)
# The task asks for educ25 (5-level NVQ). 
# Let's use the derived academic and the binary vocational variables to find the max NVQ.

full_frame <- full_frame %>%
  mutate(
    # Academic NVQ at 25
    ac_nvq25 = map_nvq_5level(W8DHANVQH),
    # Vocational NVQ at 25
    # W8VCQU0J (L3-5) -> 3, W8VCQU0I (L1-2) -> 1 (approx)
    # This is tricky. Let's use the provided derived variable W8DHANVQH as primary if available,
    # but for educ25 we need the highest overall.
    # Since only W8DHANVQH is provided as a derived NVQ for Wave 8, we'll use it as the basis for academic.
    # For vocational, we check the binary flags.
    voc_nvq25 = case_when(
      as.numeric(W8VCQU0J) == 1 ~ 3, # L3-5
      as.numeric(W8VCQU0I) == 1 ~ 1, # L1-2
      as.numeric(W8VCQU0P) == 1 ~ 0, # None
      TRUE ~ -3
    ),
    educ25 = pmax(ac_nvq25, voc_nvq25, na.rm = TRUE)
  )

# --- AGE 32 (Wave 9) ---
# Detailed Academic 32 (educadtl32)
# Doctorate (W9ACQU0A) -> 1, Masters (W9ACQU0B) -> 2... 
# Let's create a binary set for educadtl32 and educvdtl32 as per "detailed" request
# Usually "detailed" means preserving the raw breakdown or a specific sequence.
# Since the requirements say "detailed academic qualifications", let's use a string or a set of flags.
# However, output must be a CSV. I will create a combined string or a representative value.
# Actually, let's provide the highest qualification attained for the detailed versions.

full_frame <- full_frame %>%
  mutate(
    # Detailed Academic 32
    educadtl32 = case_when(
      as.numeric(W9ACQU0A) == 1 ~ "Doctorate",
      as.numeric(W9ACQU0B) == 1 ~ "Masters",
      as.numeric(W9ACQU0C) == 1 ~ "Undergraduate",
      as.numeric(W9ACQU0D) == 1 ~ "PG Diploma/Cert",
      as.numeric(W9ACQU0E) == 1 ~ "HE Diploma",
      as.numeric(W9ACQU0F) == 1 ~ "Teaching",
      as.numeric(W9ACQU0G) == 1 ~ "A-Level",
      as.numeric(W9ACQU0H) == 1 ~ "GCSE A-C",
      as.numeric(W9ACQU0I) == 1 ~ "GCSE D-G",
      as.numeric(W9ACQU0S) == 1 ~ "None",
      TRUE ~ NA_character_
    ),
    # Detailed Vocational 32
    educvdtl32 = case_when(
      as.numeric(W9VCQU0A) == 1 ~ "Professional Degree",
      as.numeric(W9VCQU0C) == 1 ~ "L4/5",
      as.numeric(W9VCQU0D) == 1 ~ "L3",
      as.numeric(W9VCQU0E) == 1 ~ "L2",
      as.numeric(W9VCQU0F) == 1 ~ "L1",
      as.numeric(W9VCQUAG) == 1 ~ "None",
      TRUE ~ NA_character_
    ),
    # Harmonised 5-level NVQ for Age 32
    # Use derived W9DANVQH (Academic) and W9DVNVQH (Vocational)
    ac_nvq32 = map_nvq_5level(W9DANVQH),
    voc_nvq32 = map_nvq_5level(W9DVNVQH),
    educ32 = pmax(ac_nvq32, voc_nvq32, na.rm = TRUE)
  )

# Final cleaning of missing values for the 5-level vars
# Ensure -3 is used for NAs
full_frame <- full_frame %>%
  mutate(
    educ25 = coalesce(educ25, -3),
    educ32 = coalesce(educ32, -3),
    educadtl32 = coalesce(educadtl32, "Not asked/NA"),
    educvdtl32 = coalesce(educvdtl32, "Not asked/NA")
  )

# Final selection
output <- full_frame %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

write_csv(output, 'data/output/cleaned_data.csv')

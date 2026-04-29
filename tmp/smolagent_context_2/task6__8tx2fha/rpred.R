library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define paths
input_path <- "data/input/"
output_path <- "data/output/cleaned_data.csv"

# Load files
# Wave 1
w1 <- read_delim(paste0(input_path, "wave_one_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(.default = "character")) %>% select(NSID)

# Wave 2
w2 <- read_delim(paste0(input_path, "wave_two_lsype_family_background_2020.tab"), delim = "\t", col_types = readr::cols(NSID = "character", .default = "numeric")) %>% 
  select(NSID, regub15 = urbind, regov15 = gor)

# Wave 3
w3 <- read_delim(paste0(input_path, "wave_three_lsype_family_background_2020.tab"), delim = "\t", col_types = readr::cols(NSID = "character", .default = "numeric")) %>% 
  select(NSID, regub16 = urbind, regov16 = gor)

# Wave 4
w4 <- read_delim(paste0(input_path, "wave_four_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(.default = "character")) %>% select(NSID)

# Wave 8
w8 <- read_delim(paste0(input_path, "ns8_2015_derived.tab"), delim = "\t", col_types = readr::cols(NSID = "character", .default = "numeric")) %>% 
  select(NSID, regor25 = W8DGOR)

# Wave 9 Derived
w9_der <- read_delim(paste0(input_path, "ns9_2022_derived_variables.tab"), delim = "\t", col_types = readr::cols(NSID = "character", .default = "numeric")) %>% 
  select(NSID, regor32 = W9DRGN)

# Wave 9 Main
w9_main <- read_delim(paste0(input_path, "ns9_2022_main_interview.tab"), delim = "\t", col_types = readr::cols(NSID = "character", .default = "numeric")) %>% 
  select(NSID, regint32_raw = W9NATIONRES)

# Merge all
full_data <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w3, by = "NSID") %>%
  full_join(w4, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9_der, by = "NSID") %>%
  full_join(w9_main, by = "NSID")

# Processing function for missing values mapping based on labels
# Standard: -9 Refusal, -8 DK, -7 Prefer not, -3 Not asked, -2 Script/Lost, -1 NA

# Processing regub15
full_data <- full_data %>%
  mutate(regub15 = case_when(
    regub15 >= 1 ~ regub15,
    regub15 == -94 ~ -2, # Special requirement: -94 -> -2 script error
    regub15 >= -999 & regub15 <= -1 ~ -2, # General map for these waves' missing range to -2
    TRUE ~ -3
  ))

# Processing regub16
full_data <- full_data %>%
  mutate(regub16 = case_when(
    regub16 >= 1 ~ regub16,
    regub16 == -94 ~ -2, # Special requirement: -94 -> -2 script error
    regub16 >= -999 & regub16 <= -1 ~ -2,
    TRUE ~ -3
  ))

# Processing regov15
full_data <- full_data %>%
  mutate(regov15 = case_when(
    regov15 >= 1 ~ regov15,
    regov15 == -94 ~ -2, # Special requirement: -94 -> -2 script error
    regov15 >= -999 & regov15 <= -1 ~ -2,
    TRUE ~ -3
  ))

# Processing regov16
full_data <- full_data %>%
  mutate(regov16 = case_when(
    regov16 >= 1 ~ regov16,
    regov16 == -94 ~ -2, # Special requirement: -94 -> -2 script error
    regov16 >= -999 & regov16 <= -1 ~ -2,
    TRUE ~ -3
  ))

# Processing regor25
full_data <- full_data %>%
  mutate(regor25 = case_when(
    regor25 >= 1 ~ regor25,
    regor25 == -9 ~ -9, # Refused
    regor25 == -8 ~ -8, # Insufficient info
    regor25 == -1 ~ -1, # Not applicable
    TRUE ~ -3
  ))

# Processing regor32
full_data <- full_data %>%
  mutate(regor32 = case_when(
    regor32 >= 1 ~ regor32,
    regor32 == -9 ~ -9, # Refused
    regor32 == -8 ~ -8, # Insufficient info
    regor32 == -1 ~ -1, # Not applicable
    TRUE ~ -3
  ))

# Processing regint32
full_data <- full_data %>%
  mutate(regint32 = case_when(
    regint32_raw %in% c(1, 2, 3, 4) ~ 1, # England/Scotland/Wales/NI
    regint32_raw == 5 ~ 2,               # Outside UK
    regint32_raw == -9 ~ -9,            # Refused
    regint32_raw == -8 ~ -8,            # DK
    regint32_raw == -3 ~ -3,            # Not asked
    regint32_raw == -1 ~ -1,            # NA
    TRUE ~ -3
  ))

# Final selection
final_df <- full_data %>%
  select(NSID, regub15, regub16, regov15, regov16, regor25, regor32, regint32)

write_csv(final_df, output_path)

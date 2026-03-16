# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(readr)

# Step 1: Load datasets
wave14 <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave17 <- readr::read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave18 <- readr::read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t')
wave19 <- readr::read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave20 <- readr::read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave25 <- readr::read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
wave32 <- readr::read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Step 2: Merge datasets by NSID
merged_data <- wave14
for (wave in list(wave17, wave18, wave19, wave20, wave25, wave32)) {
  merged_data <- merged_data %>% full_join(wave, by = 'NSID')
}

# Step 3: Define harmonization logic for each wave
# Wave 17 (Age 17)
merged_data <- merged_data %>% 
  mutate(ecoact17 = case_when(
    W4empsYP %in% c(-999, -998, -997, -995) ~ -2,
    W4empsYP == -94 ~ -8,
    W4empsYP == -92 ~ -9,
    W4empsYP == -91 ~ -1,
    W4empsYP %in% c(1, 2) ~ 1,
    W4empsYP == 3 ~ 4,
    W4empsYP == 4 ~ 5,
    W4empsYP == 5 ~ 2,
    W4empsYP == 6 ~ 9,
    W4empsYP == 7 ~ 8,
    W4empsYP == 8 ~ 8,
    W4empsYP == 9 ~ 10,
    TRUE ~ -3
  ))

# Wave 18 (Age 18)
merged_data <- merged_data %>% 
  mutate(ecoact18 = case_when(
    W5mainactYP %in% c(-999, -998, -997, -995) ~ -2,
    W5mainactYP == -94 ~ -8,
    W5mainactYP == -92 ~ -9,
    W5mainactYP == -91 ~ -1,
    W5mainactYP %in% c(1, 3) ~ 1,
    W5mainactYP == 2 ~ 9,
    W5mainactYP == 4 ~ 2,
    W5mainactYP == 5 ~ 5,
    W5mainactYP == 6 ~ 7,
    W5mainactYP == 7 ~ 4,
    W5mainactYP == 8 ~ 9,
    W5mainactYP %in% c(9, 10, 11) ~ 10,
    TRUE ~ -3
  ))

# Wave 19 (Age 19)
merged_data <- merged_data %>% 
  mutate(ecoact19 = case_when(
    W6TCurrentAct %in% c(-999, -998, -997, -995) ~ -2,
    W6TCurrentAct == -94 ~ -8,
    W6TCurrentAct == -92 ~ -9,
    W6TCurrentAct == -91 ~ -1,
    W6TCurrentAct == 1 ~ 2,
    W6TCurrentAct == 2 ~ 2,
    W6TCurrentAct == 3 ~ 1,
    W6TCurrentAct == 4 ~ 5,
    W6TCurrentAct == 5 ~ 6,
    W6TCurrentAct %in% c(6, 9) ~ 10,
    W6TCurrentAct == 7 ~ 9,
    W6TCurrentAct == 8 ~ 4,
    W6TCurrentAct == 10 ~ 9,
    W6TCurrentAct == 11 ~ 3,
    TRUE ~ -3
  ))

# Wave 20 (Age 20)
merged_data <- merged_data %>% 
  mutate(ecoact20 = case_when(
    W7TCurrentAct %in% c(-999, -998, -997, -995) ~ -2,
    W7TCurrentAct == -94 ~ -8,
    W7TCurrentAct == -92 ~ -9,
    W7TCurrentAct == -91 ~ -1,
    W7TCurrentAct %in% c(1, 2) ~ 2,
    W7TCurrentAct == 3 ~ 1,
    W7TCurrentAct == 4 ~ 5,
    W7TCurrentAct == 5 ~ 6,
    W7TCurrentAct %in% c(6, 13) ~ 10,
    W7TCurrentAct == 7 ~ 9,
    W7TCurrentAct == 8 ~ 4,
    W7TCurrentAct == 9 ~ 9,
    W7TCurrentAct == 10 ~ 3,
    W7TCurrentAct %in% c(11, 12) ~ 7,
    W7TCurrentAct == 14 ~ 8,
    W7TCurrentAct == 15 ~ 10,
    TRUE ~ -3
  ))

# Wave 25 (Age 25)
merged_data <- merged_data %>% 
  mutate(ecoact25 = case_when(
    W8DACTIVITYC %in% c(-9, -8, -1) ~ -2,
    W8DACTIVITYC %in% c(1, 2) ~ 1,
    W8DACTIVITYC == 3 ~ 3,
    W8DACTIVITYC == 4 ~ 4,
    W8DACTIVITYC == 5 ~ 2,
    W8DACTIVITYC == 6 ~ 6,
    W8DACTIVITYC == 7 ~ 7,
    W8DACTIVITYC == 8 ~ 8,
    W8DACTIVITYC == 9 ~ 9,
    W8DACTIVITYC == 10 ~ 10,
    TRUE ~ -3
  ))

# Wave 32 (Age 32)
merged_data <- merged_data %>% 
  mutate(ecoact32 = case_when(
    W9DACTIVITYC %in% c(-9, -8, -1) ~ -2,
    W9DACTIVITYC %in% c(1, 2) ~ 1,
    W9DACTIVITYC == 3 ~ 3,
    W9DACTIVITYC == 4 ~ 4,
    W9DACTIVITYC == 5 ~ 2,
    W9DACTIVITYC == 6 ~ 6,
    W9DACTIVITYC == 7 ~ 7,
    W9DACTIVITYC == 8 ~ 8,
    W9DACTIVITYC == 9 ~ 9,
    W9DACTIVITYC == 10 ~ 10,
    TRUE ~ -3
  ))

# Step 4: Define detailed adult variables
merged_data <- merged_data %>% 
  mutate(
    ecoactadu25 = ecoact25,
    ecoactadu32 = ecoact32
  )

# Step 5: Define factor levels and labels
create_factor <- function(var) {
  factor(var, 
         levels = c(-9, -8, -2, -1, -3, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
         labels = c("Refusal", "Don't know/insufficient information", 
                    "Schedule not applicable/script error/information lost", "Item not applicable", 
                    "Not asked at the fieldwork stage/participated/interviewed", 
                    "In paid work", "In education", "In unpaid/voluntary work", 
                    "Unemployed", "On training course/scheme", "Apprenticeship", 
                    "On government employment training scheme", "Sick/disabled", 
                    "Looking after home/family", "Other"))
}

# Convert variables to factors
factor_vars <- c('ecoact17', 'ecoact18', 'ecoact19', 'ecoact20', 'ecoact25', 'ecoact32', 'ecoactadu25', 'ecoactadu32')

for (var in factor_vars) {
  merged_data[[var]] <- create_factor(merged_data[[var]])
}

# Step 6: Select required variables
final_data <- merged_data %>% 
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Step 7: Write output to file
write_csv(final_data, 'data/output/cleaned_data.csv')

# Print confirmation
cat('Data cleaning and preprocessing complete. Output saved to data/output/cleaned_data.csv')
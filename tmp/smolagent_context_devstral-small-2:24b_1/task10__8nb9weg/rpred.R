library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load each dataset
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_five <- readr::read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave_six <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave_nine <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_five, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Standard missing value codes
standard_missing_codes <- function(df) {
  df <- df %>%
    mutate(across(where(is.numeric), ~ case_when(
      .x %in% c(-999, -998, -997, -995) ~ -2,
      .x == -94 ~ -8,
      .x == -92 ~ -9,
      .x == -91 ~ -1,
      .x == -99 ~ -3,
      is.na(.x) ~ -3,
      TRUE ~ .x
    )))
  return(df)
}

merged_data <- standard_missing_codes(merged_data)

# Harmonize economic activity variables
# Wave 4 (Age 17)
merged_data <- merged_data %>%
  mutate(ecoact17 = case_when(
    W4empsYP == 1 ~ 1,
    W4empsYP == 2 ~ 2,
    W4empsYP == 3 ~ 3,
    W4empsYP == 4 ~ 4,
    W4empsYP == 5 ~ 5,
    W4empsYP == 6 ~ 6,
    W4empsYP == 7 ~ 7,
    W4empsYP == 8 ~ 8,
    W4empsYP == 9 ~ 9,
    TRUE ~ as.numeric(W4empsYP)
  ))

# Wave 5 (Age 18)
merged_data <- merged_data %>%
  mutate(ecoact18 = case_when(
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
    TRUE ~ as.numeric(W5mainactYP)
  ))

# Wave 6 (Age 19)
merged_data <- merged_data %>%
  mutate(ecoact19 = case_when(
    W6TCurrentAct == 1 ~ 1,
    W6TCurrentAct == 2 ~ 2,
    W6TCurrentAct == 3 ~ 3,
    W6TCurrentAct == 4 ~ 4,
    W6TCurrentAct == 5 ~ 5,
    W6TCurrentAct == 6 ~ 6,
    W6TCurrentAct == 7 ~ 7,
    W6TCurrentAct == 8 ~ 8,
    W6TCurrentAct == 9 ~ 9,
    W6TCurrentAct == 10 ~ 10,
    W6TCurrentAct == 11 ~ 11,
    TRUE ~ as.numeric(W6TCurrentAct)
  ))

# Wave 7 (Age 20)
merged_data <- merged_data %>%
  mutate(ecoact20 = case_when(
    W7TCurrentAct == 1 ~ 1,
    W7TCurrentAct == 2 ~ 2,
    W7TCurrentAct == 3 ~ 3,
    W7TCurrentAct == 4 ~ 4,
    W7TCurrentAct == 5 ~ 5,
    W7TCurrentAct == 6 ~ 6,
    W7TCurrentAct == 7 ~ 7,
    W7TCurrentAct == 8 ~ 8,
    W7TCurrentAct == 9 ~ 9,
    W7TCurrentAct == 10 ~ 10,
    W7TCurrentAct == 11 ~ 11,
    W7TCurrentAct == 12 ~ 12,
    W7TCurrentAct == 13 ~ 13,
    W7TCurrentAct == 14 ~ 14,
    W7TCurrentAct == 15 ~ 15,
    TRUE ~ as.numeric(W7TCurrentAct)
  ))

# Wave 8 (Age 25)
merged_data <- merged_data %>%
  mutate(ecoact25 = case_when(
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
    TRUE ~ as.numeric(W8DACTIVITYC)
  ))

# Wave 9 (Age 32)
merged_data <- merged_data %>%
  mutate(ecoact32 = case_when(
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
    TRUE ~ as.numeric(W9DACTIVITYC)
  ))

# Detailed adult versions for waves 8 and 9
merged_data <- merged_data %>%
  mutate(ecoactadu25 = W8DACTIVITYC,
         ecoactadu32 = W9DACTIVITYC)

# Convert harmonized variables to factors
merged_data <- merged_data %>%
  mutate(
    ecoact17 = factor(ecoact17, levels = 1:9, labels = c(
      "Doing paid work for 30 or more hours a week",
      "Doing paid work for fewer than 30 hours a week",
      "Unemployed/ Looking for a job",
      "On a training course or scheme",
      "In full-time education/ at school",
      "Looking after the family/ household",
      "Retired from work altogether",
      "Sick/ disabled",
      "Other"
    )),
    ecoact18 = factor(ecoact18, levels = 1:11, labels = c(
      "Apprenticeship",
      "Part of week with employer, part of week at college",
      "In paid work",
      "In education",
      "On a training course/scheme",
      "On the Entry to Employment scheme",
      "Unemployed and looking for work",
      "Looking after the family and home",
      "Waiting for a course or job to start",
      "Waiting for exam results",
      "Waiting for the result of a job application"
    )),
    ecoact19 = factor(ecoact19, levels = 1:11, labels = c(
      "Doing a course at a university",
      "In education",
      "In paid work",
      "On a training course or scheme",
      "Doing an Apprenticeship",
      "Waiting for a course or job to start",
      "Looking after the family and home",
      "Unemployed and looking for work",
      "Waiting for exam results or result of job application",
      "Spending part of the week with an employer and part of the week at college",
      "Doing voluntary work"
    )),
    ecoact20 = factor(ecoact20, levels = 1:15, labels = c(
      "University",
      "School/college education",
      "Paid work",
      "Training course/scheme",
      "Apprenticeship",
      "Waiting for a course or job to start",
      "Looking after home/family",
      "Unemployed and looking for work",
      "Part time job and part time college",
      "Voluntary work",
      "Government employment programme",
      "Travelling",
      "Break from work/college",
      "Ill or disabled",
      "Not defined"
    )),
    ecoact25 = factor(ecoact25, levels = 1:10, labels = c(
      "Employee - in paid work",
      "Self employed",
      "In unpaid/voluntary work",
      "Unemployed",
      "Education: School/college/university",
      "Apprenticeship",
      "On gov't scheme for employment training",
      "Sick or disabled",
      "Looking after home or family",
      "Something else"
    )),
    ecoact32 = factor(ecoact32, levels = 1:10, labels = c(
      "Employee - in paid work",
      "Self employed",
      "In unpaid/voluntary work",
      "Unemployed",
      "Education: School/college/university",
      "Apprenticeship",
      "On gov't scheme for employment training",
      "Sick or disabled",
      "Looking after home or family",
      "Something else"
    ))
  )

# Convert detailed adult variables to factors
merged_data <- merged_data %>%
  mutate(
    ecoactadu25 = factor(ecoactadu25, levels = 1:10, labels = c(
      "Employee - in paid work",
      "Self employed",
      "In unpaid/voluntary work",
      "Unemployed",
      "Education: School/college/university",
      "Apprenticeship",
      "On gov't scheme for employment training",
      "Sick or disabled",
      "Looking after home or family",
      "Something else"
    )),
    ecoactadu32 = factor(ecoactadu32, levels = 1:10, labels = c(
      "Employee - in paid work",
      "Self employed",
      "In unpaid/voluntary work",
      "Unemployed",
      "Education: School/college/university",
      "Apprenticeship",
      "On gov't scheme for employment training",
      "Sick or disabled",
      "Looking after home or family",
      "Something else"
    ))
  )

# Select final variables
final_data <- merged_data %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Save to CSV
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)
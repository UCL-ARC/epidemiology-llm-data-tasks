# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_five = "data/input/wave_five_lsype_young_person_2020.tab",
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave_eight = "data/input/ns8_2015_derived.tab",
  wave_nine = "data/input/ns9_2022_derived_variables.tab"
)

# Load each dataset
wave_one <- readr::read_delim(files$wave_one, delim = "\t")
wave_four <- readr::read_delim(files$wave_four, delim = "\t")
wave_five <- readr::read_delim(files$wave_five, delim = "\t")
wave_six <- readr::read_delim(files$wave_six, delim = "\t")
wave_seven <- readr::read_delim(files$wave_seven, delim = "\t")
wave_eight <- readr::read_delim(files$wave_eight, delim = "\t")
wave_nine <- readr::read_delim(files$wave_nine, delim = "\t")

# Standard missing value codes
standard_missing <- c(
  "-999.0" = -2,
  "-998.0" = -2,
  "-997.0" = -2,
  "-995.0" = -2,
  "-94.0" = -8,
  "-92.0" = -9,
  "-91.0" = -8,
  "-9.0" = -9,
  "-8.0" = -8,
  "-1.0" = -1
)

# Function to harmonize missing values
harmonize_missing <- function(df, var) {
  if (var %in% colnames(df)) {
    df[[var]] <- case_when(
      df[[var]] %in% as.numeric(names(standard_missing)) ~ standard_missing[as.character(df[[var]])],
      is.na(df[[var]]) ~ -3,
      TRUE ~ df[[var]]
    )
  }
  return(df)
}

# Harmonize missing values for each wave
wave_four <- harmonize_missing(wave_four, "W4empsYP")
wave_five <- harmonize_missing(wave_five, "W5mainactYP")
wave_six <- harmonize_missing(wave_six, "W6TCurrentAct")
wave_seven <- harmonize_missing(wave_seven, "W7TCurrentAct")
wave_eight <- harmonize_missing(wave_eight, "W8DACTIVITYC")
wave_nine <- harmonize_missing(wave_nine, "W9DACTIVITYC")

# Create harmonized variables for each wave
wave_four <- wave_four %>% 
  mutate(ecoact17 = case_when(
    W4empsYP == 1.0 ~ 1,
    W4empsYP == 2.0 ~ 2,
    W4empsYP == 3.0 ~ 3,
    W4empsYP == 4.0 ~ 4,
    W4empsYP == 5.0 ~ 5,
    W4empsYP == 6.0 ~ 6,
    W4empsYP == 7.0 ~ 7,
    W4empsYP == 8.0 ~ 8,
    W4empsYP == 9.0 ~ 9,
    TRUE ~ W4empsYP
  ))

wave_five <- wave_five %>% 
  mutate(ecoact18 = case_when(
    W5mainactYP == 1.0 ~ 1,
    W5mainactYP == 2.0 ~ 2,
    W5mainactYP == 3.0 ~ 3,
    W5mainactYP == 4.0 ~ 4,
    W5mainactYP == 5.0 ~ 5,
    W5mainactYP == 6.0 ~ 6,
    W5mainactYP == 7.0 ~ 7,
    W5mainactYP == 8.0 ~ 8,
    W5mainactYP == 9.0 ~ 9,
    W5mainactYP == 10.0 ~ 10,
    W5mainactYP == 11.0 ~ 11,
    TRUE ~ W5mainactYP
  ))

wave_six <- wave_six %>% 
  mutate(ecoact19 = case_when(
    W6TCurrentAct == 1.0 ~ 1,
    W6TCurrentAct == 2.0 ~ 2,
    W6TCurrentAct == 3.0 ~ 3,
    W6TCurrentAct == 4.0 ~ 4,
    W6TCurrentAct == 5.0 ~ 5,
    W6TCurrentAct == 6.0 ~ 6,
    W6TCurrentAct == 7.0 ~ 7,
    W6TCurrentAct == 8.0 ~ 8,
    W6TCurrentAct == 9.0 ~ 9,
    W6TCurrentAct == 10.0 ~ 10,
    W6TCurrentAct == 11.0 ~ 11,
    TRUE ~ W6TCurrentAct
  ))

wave_seven <- wave_seven %>% 
  mutate(ecoact20 = case_when(
    W7TCurrentAct == 1.0 ~ 1,
    W7TCurrentAct == 2.0 ~ 2,
    W7TCurrentAct == 3.0 ~ 3,
    W7TCurrentAct == 4.0 ~ 4,
    W7TCurrentAct == 5.0 ~ 5,
    W7TCurrentAct == 6.0 ~ 6,
    W7TCurrentAct == 7.0 ~ 7,
    W7TCurrentAct == 8.0 ~ 8,
    W7TCurrentAct == 9.0 ~ 9,
    W7TCurrentAct == 10.0 ~ 10,
    W7TCurrentAct == 11.0 ~ 11,
    W7TCurrentAct == 12.0 ~ 12,
    W7TCurrentAct == 13.0 ~ 13,
    W7TCurrentAct == 14.0 ~ 14,
    W7TCurrentAct == 15.0 ~ 15,
    TRUE ~ W7TCurrentAct
  ))

wave_eight <- wave_eight %>% 
  mutate(ecoact25 = case_when(
    W8DACTIVITYC == 1.0 ~ 1,
    W8DACTIVITYC == 2.0 ~ 2,
    W8DACTIVITYC == 3.0 ~ 3,
    W8DACTIVITYC == 4.0 ~ 4,
    W8DACTIVITYC == 5.0 ~ 5,
    W8DACTIVITYC == 6.0 ~ 6,
    W8DACTIVITYC == 7.0 ~ 7,
    W8DACTIVITYC == 8.0 ~ 8,
    W8DACTIVITYC == 9.0 ~ 9,
    W8DACTIVITYC == 10.0 ~ 10,
    TRUE ~ W8DACTIVITYC
  ))

wave_nine <- wave_nine %>% 
  mutate(ecoact32 = case_when(
    W9DACTIVITYC == 1.0 ~ 1,
    W9DACTIVITYC == 2.0 ~ 2,
    W9DACTIVITYC == 3.0 ~ 3,
    W9DACTIVITYC == 4.0 ~ 4,
    W9DACTIVITYC == 5.0 ~ 5,
    W9DACTIVITYC == 6.0 ~ 6,
    W9DACTIVITYC == 7.0 ~ 7,
    W9DACTIVITYC == 8.0 ~ 8,
    W9DACTIVITYC == 9.0 ~ 9,
    W9DACTIVITYC == 10.0 ~ 10,
    TRUE ~ W9DACTIVITYC
  ))

# Merge all datasets
merged_data <- full_join(wave_one, wave_four, by = "NSID") %>% 
  full_join(wave_five, by = "NSID") %>% 
  full_join(wave_six, by = "NSID") %>% 
  full_join(wave_seven, by = "NSID") %>% 
  full_join(wave_eight, by = "NSID") %>% 
  full_join(wave_nine, by = "NSID")

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

# Select final variables
final_data <- merged_data %>% 
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32)

# Save the cleaned data
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)

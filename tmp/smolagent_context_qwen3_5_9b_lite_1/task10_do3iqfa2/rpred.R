library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Find the directory structure
print(getwd())

# Try to locate the input files
input_files <- list.files("data/input", pattern = "\\.tab$", full.names = TRUE)
print(input_files)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
}

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

print(paste("Wave1 cases:", nrow(wave1)))
print(paste("Wave4 cases:", nrow(wave4)))
print(paste("Wave5 cases:", nrow(wave5)))
print(paste("Wave6 cases:", nrow(wave6)))
print(paste("Wave7 cases:", nrow(wave7)))
print(paste("NS8 cases:", nrow(ns8)))
print(paste("NS9 cases:", nrow(ns9)))

# Standard missing value codes
# Convert NA to -3 for numeric variables
convert_na_to_missing <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- -3  # Not asked at fieldwork stage
  }
  return(x)
}

# Harmonize W4empsYP (Age 17) - create ecoact17
# Categories: 1=30+hrs, 2=<30hrs, 3=Unemployed, 4=Training, 5=Edu, 6=Family, 7=Retired, 8=Sick, 9=Other
# Collapse to 6 categories for harmonisation
# 1 = Paid work (30+ hrs)
# 2 = Paid work (<30 hrs)
# 3 = Unemployed
# 4 = Training course
# 5 = Education
# 6 = Other (family, other, retired, sick, not applicable)
ecoact17 <- wave4 %>%
  mutate(
    ecoact17 = case_when(
      W4empsYP %in% c(1, 2) ~ 1,  # Paid work (any hours)
      W4empsYP == 3 ~ 3,           # Unemployed
      W4empsYP == 4 ~ 4,           # Training course
      W4empsYP == 5 ~ 5,           # Education
      W4empsYP %in% c(6, 7, 8, 9) ~ 6,  # Other
      W4empsYP %in% c(-999, -94, -92, -91) ~ -9,  # Missing codes (refused, insufficient, not applicable)
      TRUE ~ -3
    )
  ) %>%
  mutate(ecoact17 = as.integer(ecoact17))

# Harmonize W5mainactYP (Age 18) - create ecoact18
# Categories: 1=Apprenticeship, 2=Part week, 3=Paid work, 4=Edu, 5=Training, 6=Entry scheme, 7=Unemployed, 8=Family, 9=Waiting course/job, 10=Waiting exam, 11=Waiting job app
ecoact18 <- wave5 %>%
  mutate(
    ecoact18 = case_when(
      W5mainactYP == 3 ~ 1,        # Paid work
      W5mainactYP %in% c(4) ~ 5,    # Education
      W5mainactYP %in% c(1, 5, 6) ~ 4,  # Apprenticeship, Training, Entry scheme - education/training
      W5mainactYP == 2 ~ 2,         # Part week
      W5mainactYP == 7 ~ 3,         # Unemployed
      W5mainactYP == 8 ~ 6,         # Family
      W5mainactYP %in% c(9, 10, 11) ~ -9,  # Waiting - missing
      W5mainactYP %in% c(-94) ~ -8,  # Insufficient info
      TRUE ~ -3
    )
  ) %>%
  mutate(ecoact18 = as.integer(ecoact18))

# Harmonize W6TCurrentAct (Age 19) - create ecoact19
# Categories: 1=University, 2=Edu, 3=Paid work, 4=Training, 5=Apprenticeship, 6=Waiting, 7=Family, 8=Unemployed, 9=Waiting exam/job, 10=Part week, 11=Voluntary
ecoact19 <- wave6 %>%
  mutate(
    ecoact19 = case_when(
      W6TCurrentAct == 3 ~ 1,      # Paid work
      W6TCurrentAct == 10 ~ 2,     # Part week (treat as education/training)
      W6TCurrentAct == 11 ~ 5,     # Voluntary work
      W6TCurrentAct %in% c(1, 2, 4, 5) ~ 4,  # Education, training, apprenticeship
      W6TCurrentAct %in% c(7) ~ 6,   # Family
      W6TCurrentAct == 8 ~ 3,       # Unemployed
      W6TCurrentAct %in% c(6, 9) ~ -9,  # Waiting
      W6TCurrentAct %in% c(-91) ~ -1, # Not applicable
      TRUE ~ -3
    )
  ) %>%
  mutate(ecoact19 = as.integer(ecoact19))

# Harmonize W7TCurrentAct (Age 20) - create ecoact20
# Categories: 1=University, 2=School/college, 3=Paid work, 4=Training, 5=Apprenticeship, 6=Waiting, 7=Family, 8=Unemployed, 9=Part time, 10=Voluntary, 11=Gov scheme, 12=Travelling, 13=Break, 14=Ill/disabled, 15=Not defined
ecoact20 <- wave7 %>%
  mutate(
    ecoact20 = case_when(
      W7TCurrentAct == 3 ~ 1,       # Paid work
      W7TCurrentAct == 11 ~ 1,      # Gov scheme for employment training -> Paid work
      W7TCurrentAct %in% c(1, 2, 5) ~ 4,  # Education, apprenticeship
      W7TCurrentAct == 4 ~ 4,        # Training
      W7TCurrentAct %in% c(7) ~ 6,    # Family
      W7TCurrentAct == 8 ~ 3,        # Unemployed
      W7TCurrentAct %in% c(6, 13) ~ -9,  # Waiting, Break
      W7TCurrentAct %in% c(9, 10) ~ 5,   # Part time, Voluntary
      W7TCurrentAct %in% c(14) ~ -2,    # Ill/disabled
      W7TCurrentAct == 15 ~ -3,        # Not defined
      TRUE ~ -3
    )
  ) %>%
  mutate(ecoact20 = as.integer(ecoact20))

# Harmonize W8DACTIVITYC (Age 25) - create ecoact25 and ecoactadu25 (detailed)
# Categories: 1=Employee, 2=Self employed, 3=Unpaid/voluntary, 4=Unemployed, 5=Education, 6=Apprenticeship, 7=Gov scheme, 8=Sick, 9=Family, 10=Something else
ecoact25 <- ns8 %>%
  mutate(
    ecoact25 = case_when(
      W8DACTIVITYC %in% c(1, 2) ~ 1,      # Paid work (employee or self-employed)
      W8DACTIVITYC == 3 ~ 5,               # Unpaid/voluntary work
      W8DACTIVITYC == 4 ~ 3,               # Unemployed
      W8DACTIVITYC %in% c(5, 6) ~ 4,       # Education or Apprenticeship
      W8DACTIVITYC == 7 ~ 4,               # Gov scheme for employment training
      W8DACTIVITYC == 8 ~ -2,              # Sick or disabled
      W8DACTIVITYC %in% c(9, 10) ~ 6,      # Family or Something else
      W8DACTIVITYC %in% c(-9, -8, -1) ~ -9,  # Missing
      TRUE ~ -3
    )
  ) %>%
  mutate(ecoact25 = as.integer(ecoact25))

# Detailed variable for age 25
ecoactadu25 <- ns8 %>%
  mutate(
    ecoactadu25 = W8DACTIVITYC
  ) %>%
  mutate(
    ecoactadu25 = case_when(
      W8DACTIVITYC %in% c(-9, -8) ~ -9,  # Refused, Insufficient info
      W8DACTIVITYC == -1 ~ -1,            # Not applicable
      TRUE ~ W8DACTIVITYC
    )
  )

# Harmonize W9DACTIVITYC (Age 32) - create ecoact32 and ecoactadu32 (detailed)
ecoact32 <- ns9 %>%
  mutate(
    ecoact32 = case_when(
      W9DACTIVITYC %in% c(1, 2) ~ 1,      # Paid work (employee or self-employed)
      W9DACTIVITYC == 3 ~ 5,               # Unpaid/voluntary work
      W9DACTIVITYC == 4 ~ 3,               # Unemployed
      W9DACTIVITYC %in% c(5, 6) ~ 4,       # Education or Apprenticeship
      W9DACTIVITYC == 7 ~ 4,               # Gov scheme for employment training
      W9DACTIVITYC == 8 ~ -2,              # Sick or disabled
      W9DACTIVITYC %in% c(9, 10) ~ 6,      # Family or Something else
      W9DACTIVITYC %in% c(-9, -8) ~ -9,    # Refused, Insufficient info
      W9DACTIVITYC == -1 ~ -1,             # Not applicable
      TRUE ~ -3
    )
  ) %>%
  mutate(ecoact32 = as.integer(ecoact32))

# Detailed variable for age 32
ecoactadu32 <- ns9 %>%
  mutate(
    ecoactadu32 = W9DACTIVITYC
  ) %>%
  mutate(
    ecoactadu32 = case_when(
      W9DACTIVITYC %in% c(-9, -8) ~ -9,  # Refused, Insufficient info
      W9DACTIVITYC == -1 ~ -1,            # Not applicable
      TRUE ~ W9DACTIVITYC
    )
  )

# Merge all datasets by NSID
demo <- wave1 %>%
  mutate(NSID = as.character(NSID))

demo <- full_join(demo, ecoact17, by = "NSID")
demo <- full_join(demo, ecoact18, by = "NSID")
demo <- full_join(demo, ecoact19, by = "NSID")
demo <- full_join(demo, ecoact20, by = "NSID")
demo <- full_join(demo, ecoact25, by = "NSID")
demo <- full_join(demo, ecoactadu25, by = "NSID")
demo <- full_join(demo, ecoact32, by = "NSID")
demo <- full_join(demo, ecoactadu32, by = "NSID")

# Ensure ecoact variables are integers
demo <- demo %>%
  mutate(across(starts_with("ecoact"), as.integer))

# View structure
cat("Variables in output:", paste(names(demo), collapse = ", "), "\n")
cat("Number of cases:", nrow(demo), "\n")

# Write output
write_csv(demo, "data/output/cleaned_data.csv")

cat("Successfully wrote cleaned_data.csv\n")

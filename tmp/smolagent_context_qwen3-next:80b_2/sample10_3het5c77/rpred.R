library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
df1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
df4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
df5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
df6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
df7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
df8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
df9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets by NSID
combined <- full_join(df1, df4, by = "NSID") %>%
  full_join(df5, by = "NSID") %>%
  full_join(df6, by = "NSID") %>%
  full_join(df7, by = "NSID") %>%
  full_join(df8, by = "NSID") %>%
  full_join(df9, by = "NSID")

# Process wave4 (age 17): W4empsYP
combined <- combined %>%
  mutate(
    W4empsYP = case_when(
      W4empsYP == -999 ~ -2,
      W4empsYP == -94 ~ -8,
      W4empsYP == -92 ~ -9,
      W4empsYP == -91 ~ -1,
      TRUE ~ W4empsYP
    ),
    ecoact17 = case_when(
      W4empsYP %in% c(1, 2) ~ 1,          # Paid work
      W4empsYP == 3 ~ 3,                  # Unemployed
      W4empsYP %in% c(4, 5) ~ 2,          # Education
      W4empsYP == 6 ~ 4,                  # Looking after home
      W4empsYP == 8 ~ 5,                  # Sick/disabled
      W4empsYP %in% c(7, 9) ~ 8,          # Other
      TRUE ~ W4empsYP
    )
  )

# Process wave5 (age 18): W5mainactYP
combined <- combined %>%
  mutate(
    W5mainactYP = case_when(
      W5mainactYP == -999 ~ -2,
      W5mainactYP == -94 ~ -8,
      W5mainactYP == -99 ~ -3,
      TRUE ~ W5mainactYP
    ),
    ecoact18 = case_when(
      W5mainactYP %in% c(1, 4, 5) ~ 2,    # Education (apprenticeship, education, training)
      W5mainactYP %in% c(2, 3) ~ 1,       # Paid work
      W5mainactYP == 7 ~ 3,               # Unemployed
      W5mainactYP == 8 ~ 4,               # Looking after home
      W5mainactYP %in% c(6, 9, 10, 11) ~ 8, # Other (government scheme, waiting)
      TRUE ~ W5mainactYP
    )
  )

# Process wave6 (age 19): W6TCurrentAct
combined <- combined %>%
  mutate(
    W6TCurrentAct = case_when(
      W6TCurrentAct == -91 ~ -8,          # Unable to classify → -8
      W6TCurrentAct == -999 ~ -2,
      TRUE ~ W6TCurrentAct
    ),
    ecoact19 = case_when(
      W6TCurrentAct %in% c(1, 2, 4, 5) ~ 2, # Education (university, school, training, apprenticeship)
      W6TCurrentAct %in% c(3, 10) ~ 1,    # Paid work
      W6TCurrentAct == 8 ~ 3,             # Unemployed
      W6TCurrentAct == 7 ~ 4,             # Looking after home
      W6TCurrentAct == 11 ~ 7,            # Voluntary work
      W6TCurrentAct %in% c(6, 9) ~ 8,     # Waiting → Other
      TRUE ~ W6TCurrentAct
    )
  )

# Process wave7 (age 20): W7TCurrentAct
combined <- combined %>%
  mutate(
    W7TCurrentAct = case_when(
      W7TCurrentAct == -91 ~ -1,          # Not applicable → -1
      W7TCurrentAct == -999 ~ -2,
      TRUE ~ W7TCurrentAct
    ),
    ecoact20 = case_when(
      W7TCurrentAct %in% c(1, 2, 4, 5) ~ 2, # Education
      W7TCurrentAct %in% c(3, 11) ~ 1,    # Paid work
      W7TCurrentAct == 8 ~ 3,             # Unemployed
      W7TCurrentAct == 7 ~ 4,             # Looking after home
      W7TCurrentAct == 14 ~ 5,            # Sick/disabled
      W7TCurrentAct == 10 ~ 7,            # Voluntary work
      W7TCurrentAct %in% c(6, 9, 12, 13, 15) ~ 8, # Other
      TRUE ~ W7TCurrentAct
    )
  )

# Process wave8 (age 25): W8DACTIVITYC
combined <- combined %>%
  mutate(
    W8DACTIVITYC = case_when(
      W8DACTIVITYC == -9 ~ -9,
      W8DACTIVITYC == -8 ~ -8,
      W8DACTIVITYC == -1 ~ -1,
      TRUE ~ W8DACTIVITYC
    ),
    ecoact25 = case_when(
      W8DACTIVITYC %in% c(1, 2) ~ 1,      # In paid work
      W8DACTIVITYC == 3 ~ 7,              # Voluntary work
      W8DACTIVITYC == 4 ~ 3,              # Unemployed
      W8DACTIVITYC %in% c(5, 6) ~ 2,      # Education
      W8DACTIVITYC == 7 ~ 6,              # Gov't scheme
      W8DACTIVITYC == 8 ~ 5,              # Sick/disabled
      W8DACTIVITYC == 9 ~ 4,              # Looking after home
      W8DACTIVITYC == 10 ~ 8,             # Other
      TRUE ~ W8DACTIVITYC
    ),
    ecoactadu25 = W8DACTIVITYC
  )

# Process wave9 (age 32): W9DACTIVITYC
combined <- combined %>%
  mutate(
    W9DACTIVITYC = case_when(
      W9DACTIVITYC == -9 ~ -9,
      W9DACTIVITYC == -8 ~ -8,
      W9DACTIVITYC == -1 ~ -1,
      TRUE ~ W9DACTIVITYC
    ),
    ecoact32 = case_when(
      W9DACTIVITYC %in% c(1, 2) ~ 1,      # In paid work
      W9DACTIVITYC == 3 ~ 7,              # Voluntary work
      W9DACTIVITYC == 4 ~ 3,              # Unemployed
      W9DACTIVITYC %in% c(5, 6) ~ 2,      # Education
      W9DACTIVITYC == 7 ~ 6,              # Gov't scheme
      W9DACTIVITYC == 8 ~ 5,              # Sick/disabled
      W9DACTIVITYC == 9 ~ 4,              # Looking after home
      W9DACTIVITYC == 10 ~ 8,             # Other
      TRUE ~ W9DACTIVITYC
    ),
    ecoactadu32 = W9DACTIVITYC
  )

# Convert all variables to factors with labels
combined <- combined %>%
  mutate(
    ecoact17 = factor(ecoact17, levels = c(1, 2, 3, 4, 5, 8), labels = c("In paid work", "In education", "Unemployed", "Looking after home", "Sick/disabled", "Other")),
    ecoact18 = factor(ecoact18, levels = c(1, 2, 3, 4, 7, 8), labels = c("In paid work", "In education", "Unemployed", "Looking after home", "Voluntary work", "Other")),
    ecoact19 = factor(ecoact19, levels = c(1, 2, 3, 4, 5, 7, 8), labels = c("In paid work", "In education", "Unemployed", "Looking after home", "Sick/disabled", "Voluntary work", "Other")),
    ecoact20 = factor(ecoact20, levels = c(1, 2, 3, 4, 5, 7, 8), labels = c("In paid work", "In education", "Unemployed", "Looking after home", "Sick/disabled", "Voluntary work", "Other")),
    ecoact25 = factor(ecoact25, levels = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c("In paid work", "In education", "Unemployed", "Looking after home", "Sick/disabled", "Gov't scheme", "Voluntary work", "Other")),
    ecoact32 = factor(ecoact32, levels = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c("In paid work", "In education", "Unemployed", "Looking after home", "Sick/disabled", "Gov't scheme", "Voluntary work", "Other")),
    ecoactadu25 = factor(ecoactadu25, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels = c("Employee", "Self-employed", "Unpaid/voluntary", "Unemployed", "Education", "Apprenticeship", "Gov't scheme", "Sick/disabled", "Looking after home", "Other")),
    ecoactadu32 = factor(ecoactadu32, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels = c("Employee", "Self-employed", "Unpaid/voluntary", "Unemployed", "Education", "Apprenticeship", "Gov't scheme", "Sick/disabled", "Looking after home", "Other"))
  )

# Select only required variables
final_data <- combined %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Save to CSV
write_csv(final_data, "data/output/cleaned_data.csv")
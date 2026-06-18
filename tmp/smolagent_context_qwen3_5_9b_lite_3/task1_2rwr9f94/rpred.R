# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Set working directory if needed
setwd("data")

# Load all files
W1 <- read_delim("input/wave_one_lsype_young_person_2020.tab", delim = "\t")
W2 <- read_delim("input/wave_two_lsype_young_person_2020.tab", delim = "\t")
W3 <- read_delim("input/wave_three_lsype_young_person_2020.tab", delim = "\t")
W4 <- read_delim("input/wave_four_lsype_young_person_2020.tab", delim = "\t")
W5 <- read_delim("input/wave_five_lsype_young_person_2020.tab", delim = "\t")
W6 <- read_delim("input/wave_six_lsype_young_person_2020.tab", delim = "\t")
W7 <- read_delim("input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
W8 <- read_delim("input/ns8_2015_main_interview.tab", delim = "\t")
W9 <- read_delim("input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets by NSID
data <- W1
data <- full_join(data, W2, by = "NSID")
data <- full_join(data, W3, by = "NSID")
data <- full_join(data, W4, by = "NSID")
data <- full_join(data, W5, by = "NSID")
data <- full_join(data, W6, by = "NSID")
data <- full_join(data, W7, by = "NSID")
data <- full_join(data, W8, by = "NSID")
data <- full_join(data, W9, by = "NSID")

# Remove sex variables from source data (keep only NSID)
data <- data %>%
  select(NSID, W1sexYP, W2SexYP, W3sexYP, W4SexYP, W5SexYP, W6Sex, W7Sex, W8CMSEX, W9DSEX)

# Define missing value codes for each wave
# Wave 1 (age 14): -99 (YP not interviewed), -92 (Refused), -91 (Not applicable)
# Wave 2 (age 15): -998 (Interviewer missed), -997 (Script error), -995 (Missing history), 
#                  -99 (YP not interviewed), -92 (Refused), -91 (Not applicable), -1 (Don't know)
# Wave 3 (age 16): -99 (YP not interviewed), -92 (Refused), -91 (Not applicable)
# Wave 4 (age 17): -99 (YP not interviewed), -92 (Refused), -91 (Not applicable), -1 (Don't know)
# Wave 5 (age 18): -1 (Don't know)
# Wave 6 (age 19): -92 (Refused), -91 (Not applicable)
# Wave 7 (age 20): -91 (Not applicable)
# Wave 8 (age 25): -9 (Refused), -8 (Don't know), -1 (Not applicable)
# Wave 9 (age 32): No user missing values (missing = NA)

# Standardize missing values based on label meanings
# Using standard codes: -9=Refusal, -8=Don't know, -7=Prefer not to say, -3=Not asked, -2=Not applicable, -1=Item not applicable

data <- data %>%
  mutate(
    # Wave 1 (age 14)
    W1sexYP = case_when(
      W1sexYP == -99 ~ -3,  # YP not interviewed -> Not asked
      W1sexYP == -92 ~ -9,  # Refused
      W1sexYP == -91 ~ -1,  # Not applicable
      TRUE ~ W1sexYP
    ),
    
    # Wave 2 (age 15)
    W2SexYP = case_when(
      W2SexYP %in% c(-998, -997, -995) ~ -2,  # Interviewer issues/missing section -> Not applicable
      W2SexYP == -99 ~ -3,  # YP not interviewed -> Not asked
      W2SexYP == -92 ~ -9,  # Refused
      W2SexYP == -91 ~ -1,  # Not applicable
      W2SexYP == -1 ~ -8,   # Don't know
      TRUE ~ W2SexYP
    ),
    
    # Wave 3 (age 16)
    W3sexYP = case_when(
      W3sexYP == -99 ~ -3,  # YP not interviewed -> Not asked
      W3sexYP == -92 ~ -9,  # Refused
      W3sexYP == -91 ~ -1,  # Not applicable
      TRUE ~ W3sexYP
    ),
    
    # Wave 4 (age 17)
    W4SexYP = case_when(
      W4SexYP == -99 ~ -3,  # YP not interviewed -> Not asked
      W4SexYP == -92 ~ -9,  # Refused
      W4SexYP == -91 ~ -1,  # Not applicable
      W4SexYP == -1 ~ -8,   # Don't know
      TRUE ~ W4SexYP
    ),
    
    # Wave 5 (age 18)
    W5SexYP = case_when(
      W5SexYP == -1 ~ -8,   # Don't know
      TRUE ~ W5SexYP
    ),
    
    # Wave 6 (age 19)
    W6Sex = case_when(
      W6Sex == -92 ~ -9,    # Refused
      W6Sex == -91 ~ -1,    # Not applicable
      TRUE ~ W6Sex
    ),
    
    # Wave 7 (age 20)
    W7Sex = case_when(
      W7Sex == -91 ~ -1,    # Not applicable
      TRUE ~ W7Sex
    ),
    
    # Wave 8 (age 25)
    W8CMSEX = case_when(
      W8CMSEX == -9 ~ -9,   # Refused
      W8CMSEX == -8 ~ -8,   # Don't know
      W8CMSEX == -1 ~ -1,   # Not applicable
      TRUE ~ W8CMSEX
    ),
    
    # Wave 9 (age 32) - no user missing, NA stays as NA
  )

# Create consolidated sex variable using earliest valid response
# Rule: earliest valid for stable characteristics like sex
consolidated_sex <- data %>%
  arrange(NSID, W1sexYP) %>%
  group_by(NSID) %>%
  mutate(
    sex = case_when(
      # Wave 1 (age 14) - earliest
      !is.na(W1sexYP) & W1sexYP %in% c(1, 2) ~ W1sexYP,
      # Wave 2 (age 15)
      !is.na(W2SexYP) & W2SexYP %in% c(1, 2) ~ W2SexYP,
      # Wave 3 (age 16)
      !is.na(W3sexYP) & W3sexYP %in% c(1, 2) ~ W3sexYP,
      # Wave 4 (age 17)
      !is.na(W4SexYP) & W4SexYP %in% c(1, 2) ~ W4SexYP,
      # Wave 5 (age 18)
      !is.na(W5SexYP) & W5SexYP %in% c(1, 2) ~ W5SexYP,
      # Wave 6 (age 19)
      !is.na(W6Sex) & W6Sex %in% c(1, 2) ~ W6Sex,
      # Wave 7 (age 20)
      !is.na(W7Sex) & W7Sex %in% c(1, 2) ~ W7Sex,
      # Wave 8 (age 25)
      !is.na(W8CMSEX) & W8CMSEX %in% c(1, 2) ~ W8CMSEX,
      # Wave 9 (age 32) - last wave, only if no earlier valid
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# Recode 1->Male, 2->Female, -9/-8/-1/-3 to labeled missing
consolidated_sex <- consolidated_sex %>%
  mutate(
    sex = case_when(
      sex == 1 ~ 1,
      sex == 2 ~ 2,
      sex == -9 ~ -9,
      sex == -8 ~ -8,
      sex == -1 ~ -1,
      sex == -3 ~ -3,
      TRUE ~ NA_real_
    )
  )

# Create labelled factor with explicit labels
consolidated_sex <- consolidated_sex %>%
  mutate(
    sex = factor(sex, 
      levels = c(1, 2, -1, -3, -8, -9),
      labels = c("Male", "Female", "Not applicable", "Not asked", "Don't know", "Refused")
    )
  )

# Select final variables
final_data <- consolidated_sex %>%
  select(NSID, sex)

# Write output
write_csv(final_data, "output/cleaned_data.csv")

# Confirm output
print("Script completed successfully")
print(paste("Rows in output:", nrow(final_data)))
print(summary(final_data))
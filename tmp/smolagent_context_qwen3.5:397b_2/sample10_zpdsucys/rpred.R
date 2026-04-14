library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Keep only NSID and relevant variables from each wave
wave1_clean <- wave1 %>% select(NSID)
wave4_clean <- wave4 %>% select(NSID, W4empsYP)
wave5_clean <- wave5 %>% select(NSID, W5mainactYP)
wave6_clean <- wave6 %>% select(NSID, W6TCurrentAct)
wave7_clean <- wave7 %>% select(NSID, W7TCurrentAct)
wave8_clean <- wave8 %>% select(NSID, W8DACTIVITYC)
wave9_clean <- wave9 %>% select(NSID, W9DACTIVITYC)

# Merge all datasets
merged <- wave1_clean %>%
  full_join(wave4_clean, by = "NSID") %>%
  full_join(wave5_clean, by = "NSID") %>%
  full_join(wave6_clean, by = "NSID") %>%
  full_join(wave7_clean, by = "NSID") %>%
  full_join(wave8_clean, by = "NSID") %>%
  full_join(wave9_clean, by = "NSID")

# Function to harmonize missing values for waves 4-7
harmonize_missing_early <- function(x) {
  case_when(
    x == -999 ~ -2,
    x == -998 ~ -2,
    x == -997 ~ -2,
    x == -995 ~ -2,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -99 ~ -3,
    is.na(x) ~ -3,
    TRUE ~ as.numeric(x)
  )
}

# Function to harmonize missing values for waves 8-9
harmonize_missing_late <- function(x) {
  case_when(
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    is.na(x) ~ -3,
    TRUE ~ as.numeric(x)
  )
}

# Create detailed adult variables for wave 8 (age 25)
merged <- merged %>%
  mutate(
    ecoactadu25_raw = harmonize_missing_late(W8DACTIVITYC),
    ecoactadu25 = case_when(
      ecoactadu25_raw == 1 ~ 1,
      ecoactadu25_raw == 2 ~ 2,
      ecoactadu25_raw == 3 ~ 3,
      ecoactadu25_raw == 4 ~ 4,
      ecoactadu25_raw == 5 ~ 5,
      ecoactadu25_raw == 6 ~ 6,
      ecoactadu25_raw == 7 ~ 7,
      ecoactadu25_raw == 8 ~ 8,
      ecoactadu25_raw == 9 ~ 9,
      ecoactadu25_raw == 10 ~ 10,
      ecoactadu25_raw %in% c(-1, -2, -3, -8, -9) ~ ecoactadu25_raw,
      TRUE ~ ecoactadu25_raw
    )
  )

# Create detailed adult variables for wave 9 (age 32)
merged <- merged %>%
  mutate(
    ecoactadu32_raw = harmonize_missing_late(W9DACTIVITYC),
    ecoactadu32 = case_when(
      ecoactadu32_raw == 1 ~ 1,
      ecoactadu32_raw == 2 ~ 2,
      ecoactadu32_raw == 3 ~ 3,
      ecoactadu32_raw == 4 ~ 4,
      ecoactadu32_raw == 5 ~ 5,
      ecoactadu32_raw == 6 ~ 6,
      ecoactadu32_raw == 7 ~ 7,
      ecoactadu32_raw == 8 ~ 8,
      ecoactadu32_raw == 9 ~ 9,
      ecoactadu32_raw == 10 ~ 10,
      ecoactadu32_raw %in% c(-1, -2, -3, -8, -9) ~ ecoactadu32_raw,
      TRUE ~ ecoactadu32_raw
    )
  )

# Create harmonized collapsed ecoact25 from ecoactadu25
# Collapsed: 1=In paid work, 2=In education/training, 3=Unemployed, 4=Looking after home/family, 5=Sick/disabled, 6=Other
merged <- merged %>%
  mutate(
    ecoact25 = case_when(
      ecoactadu25 %in% c(1, 2) ~ 1,  # Employee or Self employed -> In paid work
      ecoactadu25 == 5 ~ 2,           # Education -> In education/training
      ecoactadu25 %in% c(6, 7) ~ 2,   # Apprenticeship or Gov scheme -> In education/training
      ecoactadu25 == 4 ~ 3,           # Unemployed
      ecoactadu25 == 9 ~ 4,           # Looking after home/family
      ecoactadu25 == 8 ~ 5,           # Sick/disabled
      ecoactadu25 %in% c(3, 10) ~ 6,  # Unpaid/voluntary or Something else -> Other
      ecoactadu25 %in% c(-1, -2, -3, -8, -9) ~ ecoactadu25,
      TRUE ~ 6
    )
  )

# Create harmonized collapsed ecoact32 from ecoactadu32
merged <- merged %>%
  mutate(
    ecoact32 = case_when(
      ecoactadu32 %in% c(1, 2) ~ 1,  # Employee or Self employed -> In paid work
      ecoactadu32 == 5 ~ 2,           # Education -> In education/training
      ecoactadu32 %in% c(6, 7) ~ 2,   # Apprenticeship or Gov scheme -> In education/training
      ecoactadu32 == 4 ~ 3,           # Unemployed
      ecoactadu32 == 9 ~ 4,           # Looking after home/family
      ecoactadu32 == 8 ~ 5,           # Sick/disabled
      ecoactadu32 %in% c(3, 10) ~ 6,  # Unpaid/voluntary or Something else -> Other
      ecoactadu32 %in% c(-1, -2, -3, -8, -9) ~ ecoactadu32,
      TRUE ~ 6
    )
  )

# Create harmonized ecoact17 from W4empsYP (Age 17)
# Original: 1=30+ hrs, 2=<30 hrs, 3=Unemployed, 4=Training, 5=Education, 6=Looking after family, 7=Retired, 8=Sick/disabled, 9=Other
merged <- merged %>%
  mutate(
    W4empsYP_harm = harmonize_missing_early(W4empsYP),
    ecoact17 = case_when(
      W4empsYP_harm %in% c(1, 2) ~ 1,  # Paid work (any hours) -> In paid work
      W4empsYP_harm %in% c(4, 5) ~ 2,  # Training or Education -> In education/training
      W4empsYP_harm == 3 ~ 3,           # Unemployed
      W4empsYP_harm == 6 ~ 4,           # Looking after family
      W4empsYP_harm == 8 ~ 5,           # Sick/disabled
      W4empsYP_harm %in% c(7, 9) ~ 6,   # Retired or Other -> Other
      W4empsYP_harm %in% c(-1, -2, -3, -8, -9) ~ W4empsYP_harm,
      TRUE ~ 6
    )
  )

# Create harmonized ecoact18 from W5mainactYP (Age 18)
# Original: 1=Apprenticeship, 2=Part employer/part college, 3=Paid work, 4=Education, 5=Training, 6=Entry to Employment, 7=Unemployed, 8=Looking after family, 9=Waiting for course/job, 10=Waiting exam, 11=Waiting job application
merged <- merged %>%
  mutate(
    W5mainactYP_harm = harmonize_missing_early(W5mainactYP),
    ecoact18 = case_when(
      W5mainactYP_harm %in% c(2, 3) ~ 1,  # Part employer/part college or Paid work -> In paid work
      W5mainactYP_harm %in% c(1, 4, 5, 6) ~ 2,  # Apprenticeship, Education, Training, Entry to Employment -> In education/training
      W5mainactYP_harm == 7 ~ 3,           # Unemployed
      W5mainactYP_harm == 8 ~ 4,           # Looking after family
      W5mainactYP_harm %in% c(9, 10, 11) ~ 3,  # Waiting -> Unemployed
      W5mainactYP_harm %in% c(-1, -2, -3, -8, -9) ~ W5mainactYP_harm,
      TRUE ~ 6
    )
  )

# Create harmonized ecoact19 from W6TCurrentAct (Age 19)
# Original: 1=University, 2=Education, 3=Paid work, 4=Training, 5=Apprenticeship, 6=Waiting, 7=Looking after family, 8=Unemployed, 9=Waiting exam/job, 10=Part employer/part college, 11=Voluntary work
merged <- merged %>%
  mutate(
    W6TCurrentAct_harm = harmonize_missing_early(W6TCurrentAct),
    ecoact19 = case_when(
      W6TCurrentAct_harm %in% c(3, 10) ~ 1,  # Paid work or Part employer/part college -> In paid work
      W6TCurrentAct_harm %in% c(1, 2, 4, 5) ~ 2,  # University, Education, Training, Apprenticeship -> In education/training
      W6TCurrentAct_harm %in% c(8, 6, 9) ~ 3,  # Unemployed or Waiting -> Unemployed
      W6TCurrentAct_harm == 7 ~ 4,           # Looking after family
      W6TCurrentAct_harm == 11 ~ 6,          # Voluntary work -> Other
      W6TCurrentAct_harm %in% c(-1, -2, -3, -8, -9) ~ W6TCurrentAct_harm,
      TRUE ~ 6
    )
  )

# Create harmonized ecoact20 from W7TCurrentAct (Age 20)
# Original: 1=University, 2=School/college, 3=Paid work, 4=Training, 5=Apprenticeship, 6=Waiting, 7=Looking after home/family, 8=Unemployed, 9=Part time job/part time college, 10=Voluntary, 11=Gov employment programme, 12=Travelling, 13=Break, 14=Ill/disabled, 15=Not defined
merged <- merged %>%
  mutate(
    W7TCurrentAct_harm = harmonize_missing_early(W7TCurrentAct),
    ecoact20 = case_when(
      W7TCurrentAct_harm %in% c(3, 9) ~ 1,  # Paid work or Part time job/college -> In paid work
      W7TCurrentAct_harm %in% c(1, 2, 4, 5, 11) ~ 2,  # University, School/college, Training, Apprenticeship, Gov programme -> In education/training
      W7TCurrentAct_harm %in% c(8, 6) ~ 3,  # Unemployed or Waiting -> Unemployed
      W7TCurrentAct_harm == 7 ~ 4,          # Looking after home/family
      W7TCurrentAct_harm == 14 ~ 5,         # Ill/disabled
      W7TCurrentAct_harm %in% c(10, 12, 13, 15) ~ 6,  # Voluntary, Travelling, Break, Not defined -> Other
      W7TCurrentAct_harm %in% c(-1, -2, -3, -8, -9) ~ W7TCurrentAct_harm,
      TRUE ~ 6
    )
  )

# Convert harmonized variables to factors with labels
ecoact_labels <- c("1" = "In paid work", "2" = "In education/training", "3" = "Unemployed", 
                   "4" = "Looking after home/family", "5" = "Sick/disabled", "6" = "Other",
                   "-1" = "Not applicable", "-2" = "Schedule not applicable", "-3" = "Not asked",
                   "-8" = "Don't know", "-9" = "Refused")

ecoactadu_labels <- c("1" = "Employee", "2" = "Self employed", "3" = "Unpaid/voluntary work",
                      "4" = "Unemployed", "5" = "Education", "6" = "Apprenticeship",
                      "7" = "Gov employment scheme", "8" = "Sick/disabled", 
                      "9" = "Looking after home/family", "10" = "Something else",
                      "-1" = "Not applicable", "-2" = "Schedule not applicable", "-3" = "Not asked",
                      "-8" = "Don't know", "-9" = "Refused")

merged <- merged %>%
  mutate(
    ecoact17 = factor(ecoact17, levels = c(-9, -8, -3, -2, -1, 1:6), labels = names(ecoact_labels)),
    ecoact18 = factor(ecoact18, levels = c(-9, -8, -3, -2, -1, 1:6), labels = names(ecoact_labels)),
    ecoact19 = factor(ecoact19, levels = c(-9, -8, -3, -2, -1, 1:6), labels = names(ecoact_labels)),
    ecoact20 = factor(ecoact20, levels = c(-9, -8, -3, -2, -1, 1:6), labels = names(ecoact_labels)),
    ecoact25 = factor(ecoact25, levels = c(-9, -8, -3, -2, -1, 1:6), labels = names(ecoact_labels)),
    ecoact32 = factor(ecoact32, levels = c(-9, -8, -3, -2, -1, 1:6), labels = names(ecoact_labels)),
    ecoactadu25 = factor(ecoactadu25, levels = c(-9, -8, -3, -2, -1, 1:10), labels = names(ecoactadu_labels)),
    ecoactadu32 = factor(ecoactadu32, levels = c(-9, -8, -3, -2, -1, 1:10), labels = names(ecoactadu_labels))
  )

# Select final variables
final_data <- merged %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Write output
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(final_data), "\n")
cat("Number of columns:", ncol(final_data), "\n")
cat("Variables:", paste(names(final_data), collapse = ", "), "\n")
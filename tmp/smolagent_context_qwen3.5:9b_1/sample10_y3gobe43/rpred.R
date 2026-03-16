library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

wave4 <- wave4 %>%
  mutate(ecoact17 = case_when(
    W4empsYP %in% c(-999, -998, -997, -995) ~ -2,
    W4empsYP == -94 ~ -8,
    W4empsYP == -92 ~ -9,
    W4empsYP == -91 ~ -3,
    W4empsYP == -9 ~ -9,
    W4empsYP == -8 ~ -8,
    W4empsYP == -1 ~ -1,
    W4empsYP %in% c(1:9) ~ W4empsYP,
    TRUE ~ -3
  ))

wave5 <- wave5 %>%
  mutate(ecoact18 = case_when(
    W5mainactYP %in% c(-999, -998, -997, -995) ~ -2,
    W5mainactYP == -94 ~ -8,
    W5mainactYP == -9 ~ -9,
    W5mainactYP == -8 ~ -8,
    W5mainactYP == -1 ~ -1,
    W5mainactYP %in% c(1:11) ~ W5mainactYP,
    TRUE ~ -3
  ))

wave6 <- wave6 %>%
  mutate(ecoact19 = case_when(
    W6TCurrentAct %in% c(-999, -998, -997, -995) ~ -2,
    W6TCurrentAct == -94 ~ -8,
    W6TCurrentAct == -92 ~ -9,
    W6TCurrentAct == -91 ~ -3,
    W6TCurrentAct == -9 ~ -9,
    W6TCurrentAct == -8 ~ -8,
    W6TCurrentAct == -1 ~ -1,
    W6TCurrentAct %in% c(1:11) ~ W6TCurrentAct,
    TRUE ~ -3
  ))

wave7 <- wave7 %>%
  mutate(ecoact20 = case_when(
    W7TCurrentAct %in% c(-999, -998, -997, -995) ~ -2,
    W7TCurrentAct == -94 ~ -8,
    W7TCurrentAct == -92 ~ -9,
    W7TCurrentAct == -91 ~ -3,
    W7TCurrentAct == -9 ~ -9,
    W7TCurrentAct == -8 ~ -8,
    W7TCurrentAct == -1 ~ -1,
    W7TCurrentAct %in% c(1:15) ~ W7TCurrentAct,
    TRUE ~ -3
  ))

wave8 <- wave8 %>%
  mutate(ecoact25 = case_when(
    W8DACTIVITYC %in% c(-999, -998, -997, -995) ~ -2,
    W8DACTIVITYC == -94 ~ -8,
    W8DACTIVITYC == -9 ~ -9,
    W8DACTIVITYC == -8 ~ -8,
    W8DACTIVITYC == -1 ~ -1,
    W8DACTIVITYC %in% c(-3, -4, -5, -6, -7) ~ -3,
    W8DACTIVITYC %in% c(1:10) ~ W8DACTIVITYC,
    TRUE ~ -3
  ),
  ecoactadu25 = W8DACTIVITYC)

wave9 <- wave9 %>%
  mutate(ecoact32 = case_when(
    W9DACTIVITYC %in% c(-999, -998, -997, -995) ~ -2,
    W9DACTIVITYC == -94 ~ -8,
    W9DACTIVITYC == -9 ~ -9,
    W9DACTIVITYC == -8 ~ -8,
    W9DACTIVITYC == -1 ~ -1,
    W9DACTIVITYC %in% c(-3, -4, -5, -6, -7) ~ -3,
    W9DACTIVITYC %in% c(1:10) ~ W9DACTIVITYC,
    TRUE ~ -3
  ),
  ecoactadu32 = W9DACTIVITYC)

merged_data <- wave1 %>%
  full_join(., wave4, by = "NSID") %>%
  full_join(., wave5, by = "NSID") %>%
  full_join(., wave6, by = "NSID") %>%
  full_join(., wave7, by = "NSID") %>%
  full_join(., wave8, by = "NSID") %>%
  full_join(., wave9, by = "NSID")

merged_data <- merged_data %>%
  mutate(
    ecoact17 = factor(ecoact17, levels = c(-9, -8, -1, -3, -2, 1:9), 
      labels = c("Refused", "Don't know/insufficient", "Not applicable", "Not asked", "Schedule not applicable", "Paid work 30+ hrs", "Paid work <30 hrs", "Unemployed", "Training", "Education", "Looking after family", "Retired", "Sick/disabled", "Other")),
    ecoact18 = factor(ecoact18, levels = c(-9, -8, -1, -3, -2, 1:11),
      labels = c("Refused", "Don't know/insufficient", "Not applicable", "Not asked", "Schedule not applicable", "Apprenticeship", "Part-time work/college", "In paid work", "In education", "Training", "Entry to Employment", "Unemployed", "Looking after family", "Waiting for course/job", "Waiting for exams", "Waiting for job app")),
    ecoact19 = factor(ecoact19, levels = c(-9, -8, -1, -3, -2, 1:11),
      labels = c("Refused", "Don't know/insufficient", "Not applicable", "Not asked", "Schedule not applicable", "University course", "In education", "In paid work", "Training", "Apprenticeship", "Waiting for course/job", "Looking after family", "Unemployed", "Waiting for exam results/job", "Waiting for job app", "Voluntary work")),
    ecoact20 = factor(ecoact20, levels = c(-9, -8, -1, -3, -2, 1:15),
      labels = c("Refused", "Don't know/insufficient", "Not applicable", "Not asked", "Schedule not applicable", "University", "School/college", "Paid work", "Training", "Apprenticeship", "Waiting for course/job", "Looking after family", "Unemployed", "Part-time job/college", "Voluntary work", "Government employment", "Travelling", "Break from work", "Ill/disabled", "Not defined")),
    ecoact25 = factor(ecoact25, levels = c(-9, -8, -1, -3, -2, 1:10),
      labels = c("Refused", "Don't know/insufficient", "Not applicable", "Not asked", "Schedule not applicable", "Employee", "Self-employed", "Unpaid/voluntary", "Unemployed", "Education", "Apprenticeship", "Gov't scheme", "Sick/disabled", "Looking after home/family", "Something else")),
    ecoact32 = factor(ecoact32, levels = c(-9, -8, -1, -3, -2, 1:10),
      labels = c("Refused", "Don't know/insufficient", "Not applicable", "Not asked", "Schedule not applicable", "Employee", "Self-employed", "Unpaid/voluntary", "Unemployed", "Education", "Apprenticeship", "Gov't scheme", "Sick/disabled", "Looking after home/family", "Something else")),
    ecoactadu25 = factor(ecoactadu25, levels = c(-9, -8, -1, -3, -2, 1:10),
      labels = c("Refused", "Don't know/insufficient", "Not applicable", "Not asked", "Schedule not applicable", "Employee", "Self-employed", "Unpaid/voluntary", "Unemployed", "Education", "Apprenticeship", "Gov't scheme", "Sick/disabled", "Looking after home/family", "Something else")),
    ecoactadu32 = factor(ecoactadu32, levels = c(-9, -8, -1, -3, -2, 1:10),
      labels = c("Refused", "Don't know/insufficient", "Not applicable", "Not asked", "Schedule not applicable", "Employee", "Self-employed", "Unpaid/voluntary", "Unemployed", "Education", "Apprenticeship", "Gov't scheme", "Sick/disabled", "Looking after home/family", "Something else"))
  )

output_data <- merged_data %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

write_csv(output_data, "data/output/cleaned_data.csv")
cat("Done! Rows:", nrow(output_data), "Cols:", ncol(output_data), "\n")
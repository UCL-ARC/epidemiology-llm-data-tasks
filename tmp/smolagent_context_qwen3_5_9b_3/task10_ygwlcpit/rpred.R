library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files
df1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
df4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
df5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
df6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
df7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
df8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
df9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Combine all data
df <- df1 %>% full_join(df4, by = "NSID") %>% full_join(df5, by = "NSID") %>% full_join(df6, by = "NSID") %>% full_join(df7, by = "NSID") %>% full_join(df8, by = "NSID") %>% full_join(df9, by = "NSID")

# 6-category labels
labels_6cat <- c("-9" = "Refused", "-8" = "Insufficient information", "-1" = "Not applicable", 
                 "1" = "In paid work", "2" = "Apprenticeship / government training scheme / training",
                 "3" = "Education", "4" = "Unemployed", "5" = "Looking after home / family",
                 "6" = "Other (including voluntary work, sick/disabled, waiting for course, travelling, and other residual categories)")

# Wave 4 mapping (17)
df$ecoact17 <- dplyr::case_when(
  df$W4empsYP == -999 ~ -8,
  df$W4empsYP == -94 ~ -8,
  df$W4empsYP == -92 ~ -9,
  df$W4empsYP == -91 ~ -1,
  df$W4empsYP == 1 ~ 1,
  df$W4empsYP == 2 ~ 1,
  df$W4empsYP == 3 ~ 4,
  df$W4empsYP == 4 ~ 2,
  df$W4empsYP == 5 ~ 3,
  df$W4empsYP == 6 ~ 5,
  df$W4empsYP == 7 ~ 6,
  df$W4empsYP == 8 ~ 6,
  TRUE ~ NA_real_
)

# Wave 5 mapping (18)
df$ecoact18 <- dplyr::case_when(
  df$W5mainactYP == -94 ~ -8,
  df$W5mainactYP == 3 ~ 1,
  df$W5mainactYP == 1 ~ 2,
  df$W5mainactYP == 5 ~ 2,
  df$W5mainactYP == 6 ~ 2,
  df$W5mainactYP == 4 ~ 3,
  df$W5mainactYP == 7 ~ 4,
  df$W5mainactYP == 8 ~ 5,
  df$W5mainactYP == 9 ~ 5,
  df$W5mainactYP == 10 ~ 5,
  df$W5mainactYP == 11 ~ 5,
  TRUE ~ NA_real_
)

# Wave 6 mapping (19)
df$ecoact19 <- dplyr::case_when(
  df$W6TCurrentAct == -91 ~ -1,
  df$W6TCurrentAct == 3 ~ 1,
  df$W6TCurrentAct == 10 ~ 1,
  df$W6TCurrentAct == 5 ~ 2,
  df$W6TCurrentAct == 4 ~ 2,
  df$W6TCurrentAct == 11 ~ 2,
  df$W6TCurrentAct == 2 ~ 3,
  df$W6TCurrentAct == 1 ~ 3,
  df$W6TCurrentAct == 8 ~ 4,
  df$W6TCurrentAct == 7 ~ 5,
  df$W6TCurrentAct == 6 ~ 6,
  df$W6TCurrentAct == 9 ~ 6,
  TRUE ~ NA_real_
)

# Wave 7 mapping (20)
df$ecoact20 <- dplyr::case_when(
  df$W7TCurrentAct == -91 ~ -1,
  df$W7TCurrentAct == 3 ~ 1,
  df$W7TCurrentAct == 9 ~ 1,
  df$W7TCurrentAct == 5 ~ 2,
  df$W7TCurrentAct == 4 ~ 2,
  df$W7TCurrentAct == 1 ~ 3,
  df$W7TCurrentAct == 2 ~ 3,
  df$W7TCurrentAct == 8 ~ 4,
  df$W7TCurrentAct == 7 ~ 5,
  df$W7TCurrentAct == 6 ~ 6,
  df$W7TCurrentAct == 10 ~ 6,
  df$W7TCurrentAct == 11 ~ 6,
  df$W7TCurrentAct == 12 ~ 6,
  df$W7TCurrentAct == 13 ~ 6,
  df$W7TCurrentAct == 14 ~ 6,
  df$W7TCurrentAct == 15 ~ 6,
  TRUE ~ NA_real_
)

# Wave 8 mapping (25)
df$ecoact25 <- dplyr::case_when(
  df$W8DACTIVITYC == -9 ~ -9,
  df$W8DACTIVITYC == -8 ~ -8,
  df$W8DACTIVITYC == -1 ~ -1,
  df$W8DACTIVITYC == 1 ~ 1,
  df$W8DACTIVITYC == 3 ~ 6,
  df$W8DACTIVITYC == 4 ~ 4,
  df$W8DACTIVITYC == 5 ~ 3,
  df$W8DACTIVITYC == 6 ~ 2,
  df$W8DACTIVITYC == 7 ~ 2,
  df$W8DACTIVITYC == 8 ~ 6,
  df$W8DACTIVITYC == 10 ~ 6,
  df$W8DACTIVITYC == 2 ~ 6,
  df$W8DACTIVITYC == 9 ~ 5,
  TRUE ~ NA_real_
)

# Wave 9 mapping (32)
df$ecoact32 <- dplyr::case_when(
  df$W9DACTIVITYC == -9 ~ -9,
  df$W9DACTIVITYC == -8 ~ -8,
  df$W9DACTIVITYC == -1 ~ -1,
  df$W9DACTIVITYC == 1 ~ 1,
  df$W9DACTIVITYC == 3 ~ 6,
  df$W9DACTIVITYC == 4 ~ 4,
  df$W9DACTIVITYC == 5 ~ 3,
  df$W9DACTIVITYC == 6 ~ 2,
  df$W9DACTIVITYC == 7 ~ 2,
  df$W9DACTIVITYC == 8 ~ 6,
  df$W9DACTIVITYC == 10 ~ 6,
  df$W9DACTIVITYC == 2 ~ 6,
  df$W9DACTIVITYC == 9 ~ 5,
  TRUE ~ NA_real_
)

# Detailed variables
df$ecoactadu25 <- df$W8DACTIVITYC
df$ecoactadu32 <- df$W9DACTIVITYC

# Set value labels for collapsed variables
df$ecoact17 <- factor(df$ecoact17, levels = c(-9, -8, -1, 1:6), labels = names(labels_6cat))
df$ecoact18 <- factor(df$ecoact18, levels = c(-9, -8, -1, 1:6), labels = names(labels_6cat))
df$ecoact19 <- factor(df$ecoact19, levels = c(-9, -8, -1, 1:6), labels = names(labels_6cat))
df$ecoact20 <- factor(df$ecoact20, levels = c(-9, -8, -1, 1:6), labels = names(labels_6cat))
df$ecoact25 <- factor(df$ecoact25, levels = c(-9, -8, -1, 1:6), labels = names(labels_6cat))
df$ecoact32 <- factor(df$ecoact32, levels = c(-9, -8, -1, 1:6), labels = names(labels_6cat))

# Write output
write_csv(df, "data/output/cleaned_data.csv")
print("Script completed successfully")
print(paste("Total rows:", nrow(df)))
print(paste("Variables:", ncol(df)))
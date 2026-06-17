library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
# Wave 1 (14) - Frame only
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(NSID = col_character()))
# Wave 4 (17)
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(NSID = col_character(), W4empsYP = col_double()))
# Wave 5 (18)
w5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(NSID = col_character(), W5mainactYP = col_double()))
# Wave 6 (19)
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(NSID = col_character(), W6TCurrentAct = col_double()))
# Wave 7 (20)
w7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(NSID = col_character(), W7TCurrentAct = col_double()))
# Wave 8 (25)
w8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = readr::cols(NSID = col_character(), W8DACTIVITYC = col_double()))
# Wave 9 (32)
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols(NSID = col_character(), W9DACTIVITYC = col_double()))

# Merge all files
full_df <- w1 %>%
  full_join(w4, by = "NSID") %>%
  full_join(w5, by = "NSID") %>%
  full_join(w6, by = "NSID") %>%
  full_join(w7, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9, by = "NSID")

# Missing Value Mapping Helper
# -9 Refusal, -8 Don't know, -7 Prefer not to say, -3 Not asked, -2 Schedule not applicable/script error/lost, -1 Not applicable

# 1. ecoact17 (from W4empsYP)
# Labels: -999: lost (-2), -94: Insuff (-8), -92: Refused (-9), -91: Not applicable (-1)
full_df <- full_df %>%
  mutate(ecoact17 = case_when(
    W4empsYP == -999 ~ -2,
    W4empsYP == -94 ~ -8,
    W4empsYP == -92 ~ -9,
    W4empsYP == -91 ~ -1,
    W4empsYP == 1 | W4empsYP == 2 ~ 1, # Paid work
    W4empsYP == 4 ~ 2, # Training
    W4empsYP == 5 ~ 3, # Education
    W4empsYP == 3 ~ 4, # Unemployed
    W4empsYP == 6 ~ 5, # Family
    W4empsYP == 7 | W4empsYP == 8 | W4empsYP == 9 ~ 6, # Retired, Sick, Other
    TRUE ~ -3
  ))

# 2. ecoact18 (from W5mainactYP)
# Labels: -94: Insuff (-8)
full_df <- full_df %>%
  mutate(ecoact18 = case_when(
    W5mainactYP == -94 ~ -8,
    W5mainactYP == 3 ~ 1, # Paid work
    W5mainactYP == 1 | W5mainactYP == 5 | W5mainactYP == 6 ~ 2, # Apprenticeship, Training, Entry to Employment
    W5mainactYP == 4 ~ 3, # Education
    W5mainactYP == 7 ~ 4, # Unemployed
    W5mainactYP == 8 ~ 5, # Family
    W5mainactYP == 9 | W5mainactYP == 10 | W5mainactYP == 11 ~ 6, # Waiting
    TRUE ~ -3
  ))

# 3. ecoact19 (from W6TCurrentAct)
# Labels: -91: Unable to classify (-2)
full_df <- full_df %>%
  mutate(ecoact19 = case_when(
    W6TCurrentAct == -91 ~ -2,
    W6TCurrentAct == 3 ~ 1, # Paid work
    W6TCurrentAct == 4 | W6TCurrentAct == 5 ~ 2, # Training, Apprenticeship
    W6TCurrentAct == 1 | W6TCurrentAct == 2 ~ 3, # University, Education
    W6TCurrentAct == 8 ~ 4, # Unemployed
    W6TCurrentAct == 7 ~ 5, # Family
    W6TCurrentAct == 6 | W6TCurrentAct == 9 | W6TCurrentAct == 10 | W6TCurrentAct == 11 ~ 6, # Waiting, Part-time/college, Vol
    TRUE ~ -3
  ))

# 4. ecoact20 (from W7TCurrentAct)
# Labels: -91: Not applicable (-1)
full_df <- full_df %>%
  mutate(ecoact20 = case_when(
    W7TCurrentAct == -91 ~ -1,
    W7TCurrentAct == 3 ~ 1, # Paid work
    W7TCurrentAct == 4 | W7TCurrentAct == 5 | W7TCurrentAct == 11 ~ 2, # Training, Apprent, Gov Prog
    W7TCurrentAct == 1 | W7TCurrentAct == 2 ~ 3, # University, School
    W7TCurrentAct == 8 ~ 4, # Unemployed
    W7TCurrentAct == 7 ~ 5, # Family
    W7TCurrentAct == 6 | W7TCurrentAct == 9 | W7TCurrentAct == 10 | W7TCurrentAct == 12 | W7TCurrentAct == 13 | W7TCurrentAct == 14 | W7TCurrentAct == 15 ~ 6, # Other/Waiting/Sick/Travel
    TRUE ~ -3
  ))

# 5. ecoact25 (from W8DACTIVITYC)
# Labels: -9: Refused, -8: Insuff, -1: Not applicable
full_df <- full_df %>%
  mutate(ecoact25 = case_when(
    W8DACTIVITYC == -9 ~ -9,
    W8DACTIVITYC == -8 ~ -8,
    W8DACTIVITYC == -1 ~ -1,
    W8DACTIVITYC == 1 | W8DACTIVITYC == 2 ~ 1, # Paid work
    W8DACTIVITYC == 6 | W8DACTIVITYC == 7 ~ 2, # Apprent, Gov Training
    W8DACTIVITYC == 5 ~ 3, # Education
    W8DACTIVITYC == 4 ~ 4, # Unemployed
    W8DACTIVITYC == 9 ~ 5, # Family
    W8DACTIVITYC == 3 | W8DACTIVITYC == 8 | W8DACTIVITYC == 10 ~ 6, # Vol, Sick, Other
    TRUE ~ -3
  ))

# 6. ecoact32 (from W9DACTIVITYC)
# Labels: -9: Refused, -8: Insuff, -1: Not applicable
full_df <- full_df %>%
  mutate(ecoact32 = case_when(
    W9DACTIVITYC == -9 ~ -9,
    W9DACTIVITYC == -8 ~ -8,
    W9DACTIVITYC == -1 ~ -1,
    W9DACTIVITYC == 1 | W9DACTIVITYC == 2 ~ 1, # Paid work
    W9DACTIVITYC == 6 | W9DACTIVITYC == 7 ~ 2, # Apprent, Gov Training
    W9DACTIVITYC == 5 ~ 3, # Education
    W9DACTIVITYC == 4 ~ 4, # Unemployed
    W9DACTIVITYC == 9 ~ 5, # Family
    W9DACTIVITYC == 3 | W9DACTIVITYC == 8 | W9DACTIVITYC == 10 ~ 6, # Vol, Sick, Other
    TRUE ~ -3
  ))

# 7. Detailed variables ecoactadu25 and ecoactadu32
# Use the 10 substantive categories exactly as labelled in metadata
# W8DACTIVITYC labels: 1=Employee, 2=Self emp, 3=Unpaid/vol, 4=Unemp, 5=Edu, 6=Apprent, 7=Gov, 8=Sick, 9=Family, 10=Other
full_df <- full_df %>%
  mutate(ecoactadu25 = case_when(
    W8DACTIVITYC >= 1 & W8DACTIVITYC <= 10 ~ W8DACTIVITYC,
    W8DACTIVITYC == -9 ~ -9,
    W8DACTIVITYC == -8 ~ -8,
    W8DACTIVITYC == -1 ~ -1,
    TRUE ~ -3
  ))

# W9DACTIVITYC labels: same as W8
full_df <- full_df %>%
  mutate(ecoactadu32 = case_when(
    W9DACTIVITYC >= 1 & W9DACTIVITYC <= 10 ~ W9DACTIVITYC,
    W9DACTIVITYC == -9 ~ -9,
    W9DACTIVITYC == -8 ~ -8,
    W9DACTIVITYC == -1 ~ -1,
    TRUE ~ -3
  ))

# Final Variable Selection
final_vars <- c("NSID", "ecoact17", "ecoact18", "ecoact19", "ecoact20", "ecoact25", "ecoact32", "ecoactadu25", "ecoactadu32")
output_df <- full_df %>% select(all_of(final_vars))

# Factor labels for collapsed variables
collapsed_labels <- c(
  "1" = "In paid work",
  "2" = "Apprenticeship / government training scheme / training",
  "3" = "Education",
  "4" = "Unemployed",
  "5" = "Looking after home / family",
  "6" = "Other"
)
missing_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

all_labels <- c(collapsed_labels, missing_labels)

# Apply labels to collapsed variables
for (var in c("ecoact17", "ecoact18", "ecoact19", "ecoact20", "ecoact25", "ecoact32")) {
  output_df[[var]] <- factor(output_df[[var]], levels = as.numeric(names(all_labels)), labels = all_labels)
}

# Apply labels to detailed variables (1-10 + missing)
detailed_labels_25_32 <- c(
  "1" = "Employee - in paid work",
  "2" = "Self employed",
  "3" = "In unpaid/voluntary work",
  "4" = "Unemployed",
  "5" = "Education: School/college/university",
  "6" = "Apprenticeship",
  "7" = "On gov't scheme for employment training",
  "8" = "Sick or disabled",
  "9" = "Looking after home or family",
  "10" = "Something else",
  "-9" = "Refusal",
  "-8" = "Insufficient information",
  "-1" = "Not applicable",
  "-3" = "Not asked at the fieldwork stage / not interviewed"
)

output_df$ecoactadu25 <- factor(output_df$ecoactadu25, levels = as.numeric(names(detailed_labels_25_32)), labels = detailed_labels_25_32)
output_df$ecoactadu32 <- factor(output_df$ecoactadu32, levels = as.numeric(names(detailed_labels_25_32)), labels = detailed_labels_25_32)

write_csv(output_df, "data/output/cleaned_data.csv")
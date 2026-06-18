
library(readr)
library(dplyr)

# Load required files
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets
merged <- full_join(wave4, wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Create educaim17 variable
merged$educaim17 <- case_when(
  merged$w4saim %in% c(1, 5, 6, 9) ~ 3,  # NVQ 3/AVCE/A-Level
  merged$w4saim %in% c(2, 8) ~ 2,       # GCSE
  merged$w4saim %in% c(4, 7) ~ 3,       # Other level 2/3
  merged$w4saim %in% c(10, 11) ~ 1,     # NVQ 1/Foundation
  merged$w4saim %in% c(12, 13) ~ 0,     # Other/No detail
  merged$w4saim == 14 ~ -1,            # Not studying
  TRUE ~ -3                           # Missing
)

# Create educaim19 variable
merged$educaim19 <- case_when(
  merged$W6Saim %in% c(1, 2) ~ 5,       # NVQ 5/Degree
  merged$W6Saim %in% c(3, 4) ~ 4,       # NVQ 4/Other HE
  merged$W6Saim %in% c(5, 6, 7, 8) ~ 3, # NVQ 3/AVCE/A-Level
  merged$W6Saim %in% c(9, 10, 11) ~ 2,  # NVQ 2/GCSE
  merged$W6Saim %in% c(12, 13) ~ 1,     # NVQ 1
  merged$W6Saim %in% c(14, 15) ~ 0,     # Other/No detail
  merged$W6Saim == 16 ~ -1,            # Not studying
  TRUE ~ -3                           # Missing
)

# Create educaim20 variable
merged$educaim20 <- case_when(
  merged$W7SAim %in% c(10, 11, 12, 13) ~ 5, # Degree/NVQ 5
  merged$W7SAim %in% c(6, 7, 8, 9) ~ 3,     # NVQ 3/A-Level
  merged$W7SAim %in% c(3, 4, 5) ~ 2,       # NVQ 2/GCSE
  merged$W7SAim %in% c(1, 2) ~ 1,          # NVQ 1
  merged$W7SAim == -91 ~ -1,             # Not applicable
  TRUE ~ -3                           # Missing
)

# Create educaim25 variable
merged$educaim25 <- case_when(
  any(merged$W8ACQUC0A == 1, merged$W8ACQUC0B == 1, merged$W8ACQUC0C == 1,
       merged$W8ACQUC0D == 1, merged$W8ACQUC0E == 1) ~ 5,  # Degree
  any(merged$W8VCQUC0J == 1, merged$W8VCQUC0K == 1) ~ 4,  # NVQ 3-5/HND
  any(merged$W8ACQUC0F == 1, merged$W8ACQUC0G == 1, merged$W8ACQUC0H == 1,
       merged$W8ACQUC0I == 1) ~ 3,  # A-Level
  any(merged$W8ACQUC0L == 1, merged$W8ACQUC0M == 1) ~ 2,  # GCSE
  any(merged$W8VCQUC0B == 1, merged$W8VCQUC0C == 1) ~ 1,  # NVQ 1
  TRUE ~ -3
)

# Create educaim32 variable
merged$educaim32 <- case_when(
  any(merged$W9ACQUC0A == 1, merged$W9ACQUC0B == 1, merged$W9ACQUC0C == 1,
       merged$W9ACQUC0D == 1, merged$W9ACQUC0E == 1) ~ 5,  # Degree
  any(merged$W9VCQUC0A == 1, merged$W9VCQUC0C == 1) ~ 4,  # NVQ Level 4
  any(merged$W9ACQUC0F == 1, merged$W9ACQUC0G == 1) ~ 3,  # NVQ Level 3
  any(merged$W9ACQUC0H == 1, merged$W9ACQUC0I == 1) ~ 2,  # NVQ Level 2
  any(merged$W9VCQUC0F == 1) ~ 1,  # NVQ Level 1
  TRUE ~ -3
)

# Convert to factors
merged$educaim17 <- factor(merged$educaim17,
                          levels = c(-3, -1, 0:5),
                          labels = c("Missing", "Not Applicable", "Other", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5"))

merged$educaim19 <- factor(merged$educaim19,
                          levels = c(-3, -1, 0:5),
                          labels = c("Missing", "Not Applicable", "Other", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5"))

merged$educaim20 <- factor(merged$educaim20,
                          levels = c(-3, -1, 0:5),
                          labels = c("Missing", "Not Applicable", "Other", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5"))

merged$educaim25 <- factor(merged$educaim25,
                          levels = c(-3, 0:5),
                          labels = c("Missing", "Other", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5"))

merged$educaim32 <- factor(merged$educaim32,
                          levels = c(-3, 0:5),
                          labels = c("Missing", "Other", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5"))

# Select and save final variables
final_data <- merged %>% select(NSID, starts_with("educaim"))
write_csv(final_data, "data/output/cleaned_data.csv")

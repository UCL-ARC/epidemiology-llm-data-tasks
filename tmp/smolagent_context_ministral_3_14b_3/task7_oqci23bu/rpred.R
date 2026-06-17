# Load required libraries
library(readr)
library(dplyr)
library(tidyr)

# Load all required files
file_paths <- c(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave_eight = "data/input/ns8_2015_main_interview.tab",
  wave_nine = "data/input/ns9_2022_main_interview.tab"
)

# Load each file
wave1 <- read_delim(file_paths["wave_one"], delim = "\t")
wave4 <- read_delim(file_paths["wave_four"], delim = "\t")
wave6 <- read_delim(file_paths["wave_six"], delim = "\t")
wave7 <- read_delim(file_paths["wave_seven"], delim = "\t")
wave8 <- read_delim(file_paths["wave_eight"], delim = "\t")
wave9 <- read_delim(file_paths["wave_nine"], delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- full_join(wave1, wave4, by = "NSID")
merged_data <- full_join(merged_data, wave6, by = "NSID")
merged_data <- full_join(merged_data, wave7, by = "NSID")
merged_data <- full_join(merged_data, wave8, by = "NSID")
merged_data <- full_join(merged_data, wave9, by = "NSID")

# Define mapping for educaim17
merged_data <- merged_data %>% 
  mutate(educaim17 = case_when(
    w4saim %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) ~ 1,  # NVQ 1-3 equivalent
    w4saim %in% c(12, 13) ~ 3,  # Other
    w4saim == 14 ~ 5,  # Not studying
    w4saim == -92 ~ -9,  # Refusal
    w4saim == -91 ~ -1,  # Not applicable
    TRUE ~ -3
  ))

# Define mapping for educaim19
merged_data <- merged_data %>% 
  mutate(educaim19 = case_when(
    W6Saim %in% c(1, 3) ~ 0,  # NVQ 4-5 equivalent
    W6Saim %in% c(5, 6, 7, 8, 9, 10, 11, 12) ~ 1,  # NVQ 1-3 equivalent
    W6Saim %in% c(13, 14) ~ 3,  # Other
    W6Saim == 16 ~ 5,  # Not studying
    W6Saim == -92 ~ -9,  # Refusal
    W6Saim == -91 ~ -1,  # Not applicable
    TRUE ~ -3
  ))

# Define mapping for educaim20
merged_data <- merged_data %>% 
  mutate(educaim20 = case_when(
    W7SAim %in% c(10, 11, 12, 13) ~ 0,  # NVQ 4-5 equivalent
    W7SAim %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ 1,  # NVQ 1-3 equivalent
    W7SAim == 14 ~ 3,  # Other
    W7SAim == -94 ~ -8,  # Insufficient information
    W7SAim == -91 ~ -1,  # Not applicable
    TRUE ~ -3
  ))

# Define mapping for educaim25 (wave8)
merged_data <- merged_data %>% 
  mutate(
    educaim25 = case_when(
      W8ACTIVITY05 != 1 ~ 5,  # Not currently studying
      W8VCQUC0J == 1 | W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 | W8ACQUC0D == 1 | W8ACQUC0E == 1 ~ 0,  # NVQ 4-5 equivalent
      W8VCQUC0E == 1 | W8VCQUC0D == 1 | W8VCQUC0C == 1 | W8VCQUC0B == 1 | W8VCQUC0A == 1 | 
      W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 | W8ACQUC0J == 1 | 
      W8ACQUC0K == 1 | W8ACQUC0L == 1 | W8ACQUC0M == 1 ~ 1,  # NVQ 1-3 equivalent
      W8VCQUC0D == 1 | W8ACQUC0L == 1 ~ 2,  # Entry level
      W8ACQUC0N == 1 | W8ACQUC0F == 1 ~ 3,  # Other
      W8ACQUC0O == 1 ~ 4,  # None of these qualifications
      W8ACQUC0P == 1 ~ -8,  # Don't know
      W8ACQUC0Q == 1 ~ -9,  # Refused
      TRUE ~ -3
    )
  )

# Define mapping for educaim32 (wave9)
merged_data <- merged_data %>% 
  mutate(
    educaim32 = case_when(
      W9ECONACT2 %in% c(6, 7) == FALSE ~ 5,  # Not currently studying
      W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 | W9ACQUC0D == 1 | W9ACQUC0E == 1 | 
      W9VCQUC0A == 1 | W9VCQUC0C == 1 ~ 0,  # NVQ 4-5 equivalent
      W9ACQUC0F == 1 | W9ACQUC0G == 1 | W9ACQUC0H == 1 | W9ACQUC0I == 1 | 
      W9VCQUC0D == 1 | W9VCQUC0E == 1 | W9VCQUC0F == 1 | W9VCQUC0G == 1 | 
      W9VCQUC0H == 1 | W9VCQUC0I == 1 | W9VCQUC0J == 1 | W9VCQUC0K == 1 | 
      W9VCQUC0L == 1 | W9VCQUC0M == 1 | W9VCQUC0N == 1 | W9VCQUC0O == 1 | 
      W9VCQUC0P == 1 | W9VCQUC0Q == 1 ~ 1,  # NVQ 1-3 equivalent
      W9ACQUC0I == 1 | W9VCQUC0K == 1 ~ 2,  # Entry level
      W9ACQUC0R == 1 ~ 3,  # Other
      W9ACQUC0S == 1 ~ 4,  # None of these qualifications
      W9ACQUC0T == 1 ~ -8,  # Don't know
      W9ACQUC0U == 1 ~ -9,  # Refused
      TRUE ~ -3
    )
  )

# Write the cleaned data to CSV
write_csv(merged_data %>% select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32), "data/output/cleaned_data.csv")
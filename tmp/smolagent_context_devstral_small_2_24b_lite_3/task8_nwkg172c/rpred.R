library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_eight_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave_eight_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave_nine_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
wave_nine_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_eight_main, by = "NSID") %>%
  full_join(wave_eight_derived, by = "NSID") %>%
  full_join(wave_nine_main, by = "NSID") %>%
  full_join(wave_nine_derived, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(var, metadata) {
  if (is.numeric(var)) {
    var <- ifelse(var %in% c(-9, -92), -9, var)
    var <- ifelse(var %in% c(-8, -94), -8, var)
    var <- ifelse(var %in% c(-7), -7, var)
    var <- ifelse(var %in% c(-3, -99), -3, var)
    var <- ifelse(var %in% c(-2, -999, -998, -997, -995), -2, var)
    var <- ifelse(var %in% c(-1, -91), -1, var)
    var <- ifelse(is.na(var), -3, var)
  }
  return(var)
}

# Derive educ25 (harmonised NVQ scheme at age 25)
merged_data <- merged_data %>%
  mutate(educ25 = case_when(
    W8DHANVQH == 1 ~ 1,  # NVQ Level 1
    W8DHANVQH == 2 ~ 2,  # NVQ Level 2
    W8DHANVQH == 3 ~ 3,  # NVQ Level 3
    W8DHANVQH == 4 ~ 4,  # NVQ Level 4
    W8DHANVQH == 5 ~ 5,  # NVQ Level 5
    W8DHANVQH == 95 ~ 5, # Other academic qualification (map to NVQ Level 5)
    W8DHANVQH == 96 ~ 0, # None of these qualifications (map to NVQ Entry Level)
    TRUE ~ map_missing(W8DHANVQH, NULL)
  )) %>%
  mutate(educ25 = map_missing(educ25, NULL))

# Derive educ32 (harmonised NVQ scheme at age 32)
merged_data <- merged_data %>%
  mutate(educ32 = case_when(
    W9DANVQH == 0 ~ 0,  # NVQ Entry Level
    W9DANVQH == 1 ~ 1,  # NVQ Level 1
    W9DANVQH == 2 ~ 2,  # NVQ Level 2
    W9DANVQH == 3 ~ 3,  # NVQ Level 3
    W9DANVQH == 4 ~ 4,  # NVQ Level 4
    W9DANVQH == 5 ~ 5,  # NVQ Level 5
    W9DANVQH == 95 ~ 5, # Other academic qualification (map to NVQ Level 5)
    W9DANVQH == 96 ~ 0, # None of these qualifications (map to NVQ Entry Level)
    TRUE ~ map_missing(W9DANVQH, NULL)
  )) %>%
  mutate(educ32 = map_missing(educ32, NULL))

# Derive educadtl32 (detailed academic qualifications at age 32)
merged_data <- merged_data %>%
  mutate(
    educadtl32 = case_when(
      W9ACQU0A == 1 ~ 1,  # Doctorate or equivalent
      W9ACQU0B == 1 ~ 2,  # Masters or equivalent
      W9ACQU0C == 1 ~ 3,  # Undergraduate or equivalent
      W9ACQU0D == 1 ~ 4,  # Post-graduate Diplomas and Certificates
      W9ACQU0E == 1 ~ 5,  # Diplomas in higher education and other higher education qualifications
      W9ACQU0F == 1 ~ 6,  # Teaching qualifications for schools or further education (below degree level)
      W9ACQU0G == 1 ~ 7,  # A/AS Levels or equivalent
      W9ACQU0H == 1 ~ 8,  # Grade A-C, Level 4-9
      W9ACQU0I == 1 ~ 9,  # Grade D-G, Level 1-3
      W9ACQU0J == 1 ~ 10, # SCE Higher
      W9ACQU0K == 1 ~ 11, # Scottish Certificate Sixth Year Studies
      W9ACQU0L == 1 ~ 12, # SCE Standard
      W9ACQU0M == 1 ~ 13, # National 4 and 5
      W9ACQU0N == 1 ~ 14, # National 2 and 3
      W9ACQU0O == 1 ~ 15, # Leaving Certificate
      W9ACQU0P == 1 ~ 16, # Junior Certificate grade A-C
      W9ACQU0Q == 1 ~ 17, # Junior Certificate grade D and below
      W9ACQU0R == 1 ~ 18, # Other academic qualifications (including overseas)
      W9ACQU0S == 1 ~ 0,  # None of these qualifications
      TRUE ~ map_missing(W9ACQU0A, NULL)
    )
  )

# Derive educvdtl32 (detailed vocational qualifications at age 32)
merged_data <- merged_data %>%
  mutate(
    educvdtl32 = case_when(
      W9VCQU0A == 1 ~ 1,  # Professional qualifications at degree level
      W9VCQU0B == 1 ~ 2,  # Nursing or other medical qualifications (below degree level)
      W9VCQU0C == 1 ~ 3,  # Level 4 or 5
      W9VCQU0D == 1 ~ 4,  # Level 3
      W9VCQU0E == 1 ~ 5,  # Level 2
      W9VCQU0F == 1 ~ 6,  # Level 1
      W9VCQU0G == 1 ~ 7,  # GNVQ Advanced
      W9VCQU0H == 1 ~ 8,  # GNVQ Intermediate
      W9VCQU0I == 1 ~ 9,  # Level 3
      W9VCQU0J == 1 ~ 10, # Level 2
      W9VCQU0K == 1 ~ 11, # Level Foundation
      W9VCQU0L == 1 ~ 12, # Advanced Craft, Part III
      W9VCQU0M == 1 ~ 13, # Craft, Part II
      W9VCQU0N == 1 ~ 14, # Craft, Part I
      W9VCQU0O == 1 ~ 15, # Level 3
      W9VCQU0P == 1 ~ 16, # Level 2
      W9VCQU0Q == 1 ~ 17, # Level 1
      W9VCQU0R == 1 ~ 18, # Advanced Diploma
      W9VCQU0S == 1 ~ 19, # Higher Diploma
      W9VCQU0T == 1 ~ 20, # RSA Diploma
      W9VCQU0U == 1 ~ 21, # RSA Stage I, II, III
      W9VCQU0V == 1 ~ 22, # Higher Level BTEC
      W9VCQU0W == 1 ~ 23, # BTEC National
      W9VCQU0X == 1 ~ 24, # BTEC First
      W9VCQU0Y == 1 ~ 25, # SCOTVEC National Certificate
      W9VCQU0Z == 1 ~ 26, # SCOTVEC first or general diploma
      W9VCQUAA == 1 ~ 27, # SCOTVEC general diploma
      W9VCQUAB == 1 ~ 28, # SCOTVEC modules
      W9VCQUAC == 1 ~ 29, # HND or HNC
      W9VCQUAD == 1 ~ 30, # OND or ONCM
      W9VCQUAE == 1 ~ 31, # Junior certificate
      W9VCQUAF == 1 ~ 32, # Other vocational qualifications (including some overseas)
      W9VCQUAG == 1 ~ 0,  # None of these qualifications
      TRUE ~ map_missing(W9VCQU0A, NULL)
    )
  )

# Select only the required variables for output
output_data <- merged_data %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"
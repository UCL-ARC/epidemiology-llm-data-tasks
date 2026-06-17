library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets using full_join
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to map NVQ levels to a common 6-category scheme
map_to_nvq6 <- function(x) {
  case_when(
    x %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13) ~ 1,  # NVQ 1 or equivalent
    x %in% c(14, 15, 16, 17, 18, 19, 20) ~ 2,  # NVQ 2 or equivalent
    x %in% c(21, 22, 23, 24, 25, 26, 27) ~ 3,  # NVQ 3 or equivalent
    x %in% c(28, 29, 30, 31, 32) ~ 4,  # NVQ 4 or equivalent
    x %in% c(33, 34, 35) ~ 5,  # NVQ 5 or equivalent
    is.na(x) ~ NA_integer_,
    TRUE ~ 6  # Other or unknown
  )
}

# Derive educaim17 from wave4
merged_data <- merged_data %>%
  mutate(educaim17 = case_when(
    w4saim == 1 ~ 3,  # NVQ 3
    w4saim == 2 ~ 3,  # AVCE
    w4saim == 3 ~ 3,  # A/AS
    w4saim == 4 ~ 3,  # Other level 3
    w4saim == 5 ~ 2,  # NVQ 2
    w4saim == 6 ~ 2,  # Intermediate GNVQ
    w4saim == 7 ~ 2,  # Other level 2
    w4saim == 8 ~ 2,  # GCSE
    w4saim == 9 ~ 1,  # NVQ 1
    w4saim == 10 ~ 1, # Foundation
    w4saim == 11 ~ 1, # Other level 1
    w4saim == 12 ~ 6, # Other
    w4saim == 13 ~ 6, # No detail
    w4saim == 14 ~ -1, # Not studying
    is.na(w4saim) ~ -3
  ))

# Derive educaim19 from wave6
merged_data <- merged_data %>%
  mutate(educaim19 = case_when(
    W6Saim == 1 ~ 5,  # NVQ 5
    W6Saim == 2 ~ 5,  # First/Other Degree
    W6Saim == 3 ~ 4,  # NVQ 4
    W6Saim == 4 ~ 4,  # Other HE
    W6Saim == 5 ~ 3,  # NVQ 3
    W6Saim == 6 ~ 3,  # AVCE
    W6Saim == 7 ~ 3,  # A/AS
    W6Saim == 8 ~ 3,  # Other level 3
    W6Saim == 9 ~ 2,  # NVQ 2
    W6Saim == 10 ~ 2, # Other level 2
    W6Saim == 11 ~ 2, # GCSE
    W6Saim == 12 ~ 1, # NVQ 1
    W6Saim == 13 ~ 1, # Other level 1
    W6Saim == 14 ~ 6, # Other (level unknown)
    W6Saim == 15 ~ 6, # No detail
    W6Saim == 16 ~ -1, # Not studying
    is.na(W6Saim) ~ -3
  ))

# Derive educaim20 from wave7
merged_data <- merged_data %>%
  mutate(educaim20 = case_when(
    W7SAim == 1 ~ 1,  # NVQ 1
    W7SAim == 2 ~ 1,  # Other level 1
    W7SAim == 3 ~ 2,  # NVQ 2
    W7SAim == 4 ~ 2,  # GCSE
    W7SAim == 5 ~ 2,  # Other level 2
    W7SAim == 6 ~ 3,  # NVQ 3
    W7SAim == 7 ~ 3,  # A/AS
    W7SAim == 8 ~ 3,  # AVCE
    W7SAim == 9 ~ 3,  # Other level 3
    W7SAim == 10 ~ 4, # NVQ 4
    W7SAim == 11 ~ 5, # First/Other Degree
    W7SAim == 12 ~ 4, # Other HE
    W7SAim == 13 ~ 5, # NVQ 5
    W7SAim == 14 ~ 6, # Other (level unknown)
    W7SAim == -91 ~ -1, # Not applicable (not studying)
    W7SAim == -94 ~ -8, # Insufficient information
    is.na(W7SAim) ~ -3
  ))

# Derive educaim25 from wave8
# For wave8, we need to infer the educational aim from the qualifications being studied
# We will prioritize the highest qualification being studied
merged_data <- merged_data %>%
  mutate(
    educaim25 = case_when(
      W8ACQUC0A == 1 ~ 5,  # University Higher Degree
      W8ACQUC0B == 1 ~ 5,  # First degree level qualification
      W8ACQUC0C == 1 ~ 4,  # Diploma in higher education
      W8ACQUC0D == 1 ~ 4,  # Teaching qualification (excl PGCE)
      W8ACQUC0E == 1 ~ 4,  # Nursing or other medical qualification
      W8ACQUC0F == 1 ~ 3,  # A Level
      W8ACQUC0G == 1 ~ 3,  # Welsh Baccalaureate
      W8ACQUC0H == 1 ~ 3,  # International Baccalaureate
      W8ACQUC0I == 1 ~ 3,  # AS Level
      W8ACQUC0J == 1 ~ 3,  # Higher Grade/Advanced Higher (Scotland)
      W8ACQUC0K == 1 ~ 3,  # Certificate of sixth year studies
      W8ACQUC0L == 1 ~ 2,  # GCSE
      W8ACQUC0M == 1 ~ 2,  # Standard Grade / Lower (Scotland)
      W8ACQUC0N == 1 ~ 2,  # Other school
      W8VCQUC0J == 1 ~ 3,  # NVQ/SVQ - Level 3 - 5
      W8VCQUC0K == 1 ~ 4,  # HNC/HND
      W8VCQUC0A == 1 ~ 1,  # Youth training certificate
      W8VCQUC0B == 1 ~ 1,  # Key Skills
      W8VCQUC0C == 1 ~ 1,  # Basic skills
      W8VCQUC0D == 1 ~ 1,  # Entry level qualifications (Wales)
      W8VCQUC0E == 1 ~ 2,  # Modern apprenticeship/trade apprenticeship
      W8ACQUC0O == 1 ~ 6,  # None of the above
      W8ACQUC0P == 1 ~ -8,  # Don't know
      W8ACQUC0Q == 1 ~ -9,  # Refused
      W8ACTIVITY05 == 0 ~ -1,  # Not studying
      is.na(W8ACTIVITY05) ~ -3
    )
  )

# Derive educaim32 from wave9
# For wave9, we will also prioritize the highest qualification being studied
merged_data <- merged_data %>%
  mutate(
    educaim32 = case_when(
      W9ACQUC0A == 1 ~ 5,  # Doctorate or equivalent
      W9ACQUC0B == 1 ~ 5,  # Masters or equivalent
      W9ACQUC0C == 1 ~ 5,  # Undergraduate or equivalent
      W9ACQUC0D == 1 ~ 4,  # Post-graduate Diplomas and Certificates
      W9ACQUC0E == 1 ~ 4,  # Diplomas in higher education and other higher education qualifications
      W9ACQUC0F == 1 ~ 4,  # Teaching qualifications for schools or further education (below degree level)
      W9ACQUC0G == 1 ~ 3,  # A/AS Levels or equivalent
      W9ACQUC0H == 1 ~ 3,  # Grade A-C, Level 4-9
      W9ACQUC0I == 1 ~ 2,  # Grade D-G, Level 1-3
      W9ACQUC0J == 1 ~ 3,  # SCE Higher
      W9ACQUC0K == 1 ~ 3,  # Scottish Certificate Sixth Year Studies
      W9ACQUC0L == 1 ~ 2,  # SCE Standard
      W9ACQUC0M == 1 ~ 2,  # National 4 and 5
      W9ACQUC0N == 1 ~ 1,  # National 2 and 3
      W9ACQUC0O == 1 ~ 2,  # Leaving Certificate
      W9ACQUC0P == 1 ~ 2,  # Junior Certificate grade A-C
      W9ACQUC0Q == 1 ~ 1,  # Junior Certificate grade D and below
      W9VCQUC0A == 1 ~ 5,  # Professional qualifications at degree level
      W9VCQUC0B == 1 ~ 4,  # Nursing or other medical qualifications (below degree level)
      W9VCQUC0C == 1 ~ 4,  # Level 4 or 5
      W9VCQUC0D == 1 ~ 3,  # Level 3
      W9VCQUC0E == 1 ~ 2,  # Level 2
      W9VCQUC0F == 1 ~ 1,  # Level 1
      W9VCQUC0G == 1 ~ 3,  # GNVQ Advanced
      W9VCQUC0H == 1 ~ 2,  # GNVQ Intermediate
      W9VCQUC0I == 1 ~ 3,  # Level 3
      W9VCQUC0J == 1 ~ 2,  # Level 2
      W9VCQUC0K == 1 ~ 1,  # Level Foundation
      W9VCQUC0L == 1 ~ 2,  # Advanced Craft, Part III
      W9VCQUC0M == 1 ~ 2,  # Craft, Part II
      W9VCQUC0N == 1 ~ 1,  # Craft, Part I
      W9VCQUC0O == 1 ~ 3,  # Level 3
      W9VCQUC0P == 1 ~ 2,  # Level 2
      W9VCQUC0Q == 1 ~ 1,  # Level 1
      W9VCQUC0R == 1 ~ 4,  # Advanced Diploma
      W9VCQUC0S == 1 ~ 4,  # Higher Diploma
      W9VCQUC0T == 1 ~ 4,  # RSA Diploma
      W9VCQUC0U == 1 ~ 4,  # RSA Stage I, II,III
      W9VCQUC0V == 1 ~ 4,  # Higher Level BTEC
      W9VCQUC0W == 1 ~ 3,  # BTEC National
      W9VCQUC0X == 1 ~ 2,  # BTEC First
      W9VCQUC0Y == 1 ~ 3,  # SCOTVEC National Certificate
      W9VCQUC0Z == 1 ~ 2,  # SCOTVEC first or general diploma
      W9VCQUCAA == 1 ~ 2, # SCOTVEC general diploma
      W9VCQUCAB == 1 ~ 2, # SCOTVEC modules
      W9VCQUCAC == 1 ~ 4, # HND or HNC
      W9VCQUCAD == 1 ~ 4, # OND or ONCM
      W9VCQUCAE == 1 ~ 1, # Junior certificate
      W9ACQUC0R == 1 ~ 6,  # Other academic qualifications (including overseas)
      W9ACQUC0S == 1 ~ 6,  # None of these qualifications
      W9ACQUC0T == 1 ~ -8,  # Don't know
      W9ACQUC0U == 1 ~ -9,  # Refused
      W9ACQUC0V == 1 ~ -3,  # No answer
      W9VCQUCAG == 1 ~ 6,  # None of these qualifications
      W9VCQUCAH == 1 ~ -8,  # Don't know
      W9VCQUCAI == 1 ~ -9,  # Refused
      W9ECONACT2 == 6 ~ -1,  # In full-time education
      W9ECONACT2 == 7 ~ -1,  # In part-time education
      is.na(W9ECONACT2) ~ -3
    )
  )

# Select only the ID variable and the derived variables
cleaned_data <- merged_data %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write the cleaned data to a CSV file
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return the path to the cleaned data file
"data/output/cleaned_data.csv"
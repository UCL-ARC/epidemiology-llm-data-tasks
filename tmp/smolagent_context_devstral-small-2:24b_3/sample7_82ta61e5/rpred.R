library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
ns8_2015_main_interview <- readr::read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns9_2022_main_interview <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(ns8_2015_main_interview, by = "NSID") %>%
  full_join(ns9_2022_main_interview, by = "NSID")

# Recoding logic for each wave
# Wave 17 (w4saim)
merged_data <- merged_data %>%
  mutate(educaim17 = case_when(
    w4saim == 1 ~ 1,  # NVQ 3
    w4saim == 2 ~ 1,  # AVCE
    w4saim == 3 ~ 1,  # A/AS
    w4saim == 4 ~ 1,  # Other level 3
    w4saim == 5 ~ 1,  # NVQ 2
    w4saim == 6 ~ 1,  # Intermediate GNVQ
    w4saim == 7 ~ 1,  # Other level 2
    w4saim == 8 ~ 1,  # GCSE
    w4saim == 9 ~ 1,  # NVQ 1
    w4saim == 10 ~ 1, # Foundation
    w4saim == 11 ~ 1, # Other level 1
    w4saim == 12 ~ 3, # Other
    w4saim == 13 ~ 3, # No detail
    w4saim == 14 ~ 5, # Not studying
    w4saim %in% -999:-1 ~ -3, # Missing values
    TRUE ~ -3
  ))

# Wave 19 (W6Saim)
merged_data <- merged_data %>%
  mutate(educaim19 = case_when(
    W6Saim == 1 ~ 0,  # NVQ 5
    W6Saim == 2 ~ 0,  # First/Other Degree
    W6Saim == 3 ~ 0,  # NVQ 4
    W6Saim == 4 ~ 0,  # Other HE
    W6Saim == 5 ~ 1,  # NVQ 3
    W6Saim == 6 ~ 1,  # AVCE
    W6Saim == 7 ~ 1,  # A/AS
    W6Saim == 8 ~ 1,  # Other level 3
    W6Saim == 9 ~ 1,  # NVQ 2
    W6Saim == 10 ~ 1, # Other level 2
    W6Saim == 11 ~ 1, # GCSE
    W6Saim == 12 ~ 1, # NVQ 1
    W6Saim == 13 ~ 1, # Other level 1
    W6Saim == 14 ~ 3, # Other (level unknown)
    W6Saim == 15 ~ 3, # No detail
    W6Saim == 16 ~ 5, # Not studying
    W6Saim %in% -999:-1 ~ -3, # Missing values
    TRUE ~ -3
  ))

# Wave 20 (W7SAim)
merged_data <- merged_data %>%
  mutate(educaim20 = case_when(
    W7SAim == 1 ~ 1,  # NVQ 1
    W7SAim == 2 ~ 1,  # Other level 1
    W7SAim == 3 ~ 1,  # NVQ 2
    W7SAim == 4 ~ 1,  # GCSE
    W7SAim == 5 ~ 1,  # Other level 2
    W7SAim == 6 ~ 1,  # NVQ 3
    W7SAim == 7 ~ 1,  # A/AS
    W7SAim == 8 ~ 1,  # AVCE
    W7SAim == 9 ~ 1,  # Other level 3
    W7SAim == 10 ~ 0, # NVQ 4
    W7SAim == 11 ~ 0, # First/Other Degree
    W7SAim == 12 ~ 0, # Other HE
    W7SAim == 13 ~ 0, # NVQ 5
    W7SAim == 14 ~ 3, # Other (level unknown)
    W7SAim == -94 ~ -8, # Insufficient information
    W7SAim == -91 ~ -1, # Not applicable (not studying)
    W7SAim %in% -999:-92 ~ -3, # Missing values
    TRUE ~ -3
  ))

# Wave 25 (ns8_2015_main_interview)
merged_data <- merged_data %>%
  mutate(educaim25 = case_when(
    W8ACTIVITY05 == 1 ~ case_when(
      W8ACQUC0A == 1 ~ 0, # University Higher Degree
      W8ACQUC0B == 1 ~ 0, # First degree level qualification
      W8ACQUC0C == 1 ~ 0, # Diploma in higher education
      W8ACQUC0D == 1 ~ 0, # Teaching qualification (excl PGCE)
      W8ACQUC0E == 1 ~ 0, # Nursing or other medical qualification
      W8ACQUC0F == 1 ~ 1, # A Level
      W8ACQUC0G == 1 ~ 1, # Welsh Baccalaureate
      W8ACQUC0H == 1 ~ 1, # International Baccalaureate
      W8ACQUC0I == 1 ~ 1, # AS Level
      W8ACQUC0J == 1 ~ 1, # Higher Grade/Advanced Higher (Scotland)
      W8ACQUC0K == 1 ~ 1, # Certificate of sixth year studies
      W8ACQUC0L == 1 ~ 1, # GCSE
      W8ACQUC0M == 1 ~ 1, # Standard Grade / Lower (Scotland)
      W8ACQUC0N == 1 ~ 1, # Other school
      W8ACQUC0O == 1 ~ 4, # None of the above
      W8ACQUC0P == 1 ~ -8, # Don't know
      W8ACQUC0Q == 1 ~ -9, # Refused
      W8VCQUC0A == 1 ~ 1, # Youth training certificate
      W8VCQUC0B == 1 ~ 1, # Key Skills
      W8VCQUC0C == 1 ~ 2, # Basic skills
      W8VCQUC0D == 1 ~ 2, # Entry level qualifications (Wales)
      W8VCQUC0E == 1 ~ 1, # Modern apprenticeship/trade apprenticeship
      W8VCQUC0J == 1 ~ 0, # NVQ/SVQ - Level 3 - 5
      W8VCQUC0K == 1 ~ 0, # HNC/HND
      TRUE ~ 3
    ),
    W8ACTIVITY05 == 0 ~ 5, # Not studying
    W8ACTIVITY05 %in% -9:-1 ~ -3, # Missing values
    TRUE ~ -3
  ))

# Wave 32 (ns9_2022_main_interview)
merged_data <- merged_data %>%
  mutate(educaim32 = case_when(
    W9ECONACT2 %in% 6:7 ~ case_when(
      W9ACQUC0A == 1 ~ 0, # Doctorate or equivalent
      W9ACQUC0B == 1 ~ 0, # Masters or equivalent
      W9ACQUC0C == 1 ~ 0, # Undergraduate or equivalent
      W9ACQUC0D == 1 ~ 0, # Post-graduate Diplomas and Certificates
      W9ACQUC0E == 1 ~ 0, # Diplomas in higher education and other higher education qualifications
      W9ACQUC0F == 1 ~ 0, # Teaching qualifications for schools or further education (below degree level)
      W9ACQUC0G == 1 ~ 1, # A/AS Levels or equivalent
      W9ACQUC0H == 1 ~ 1, # Grade A-C, Level 4-9
      W9ACQUC0I == 1 ~ 1, # Grade D-G, Level 1-3
      W9ACQUC0J == 1 ~ 1, # SCE Higher
      W9ACQUC0K == 1 ~ 1, # Scottish Certificate Sixth Year Studies
      W9ACQUC0L == 1 ~ 1, # SCE Standard
      W9ACQUC0M == 1 ~ 1, # National 4 and 5
      W9ACQUC0N == 1 ~ 1, # National 2 and 3
      W9ACQUC0O == 1 ~ 1, # Leaving Certificate
      W9ACQUC0P == 1 ~ 1, # Junior Certificate grade A-C
      W9ACQUC0Q == 1 ~ 1, # Junior Certificate grade D and below
      W9ACQUC0R == 1 ~ 1, # Other academic qualifications (including overseas)
      W9ACQUC0S == 1 ~ 4, # None of these qualifications
      W9ACQUC0T == 1 ~ -8, # Don't know
      W9ACQUC0U == 1 ~ -9, # Refused
      W9ACQUC0V == 1 ~ -3, # No answer
      W9VCQUC0A == 1 ~ 0, # Professional qualifications at degree level
      W9VCQUC0B == 1 ~ 0, # Nursing or other medical qualifications (below degree level)
      W9VCQUC0C == 1 ~ 0, # Level 4 or 5
      W9VCQUC0D == 1 ~ 1, # Level 3
      W9VCQUC0E == 1 ~ 1, # Level 2
      W9VCQUC0F == 1 ~ 1, # Level 1
      W9VCQUC0G == 1 ~ 1, # GNVQ Advanced
      W9VCQUC0H == 1 ~ 1, # GNVQ Intermediate
      W9VCQUC0I == 1 ~ 1, # Level 3
      W9VCQUC0J == 1 ~ 1, # Level 2
      W9VCQUC0K == 1 ~ 1, # Level Foundation
      W9VCQUC0L == 1 ~ 1, # Advanced Craft, Part III
      W9VCQUC0M == 1 ~ 1, # Craft, Part II
      W9VCQUC0N == 1 ~ 1, # Craft, Part I
      W9VCQUC0O == 1 ~ 1, # Level 3
      W9VCQUC0P == 1 ~ 1, # Level 2
      W9VCQUC0Q == 1 ~ 1, # Level 1
      W9VCQUC0R == 1 ~ 1, # Advanced Diploma
      W9VCQUC0S == 1 ~ 1, # Higher Diploma
      W9VCQUC0T == 1 ~ 1, # RSA Diploma
      W9VCQUC0U == 1 ~ 1, # RSA Stage I, II,III
      W9VCQUC0V == 1 ~ 1, # Higher Level BTEC
      W9VCQUC0W == 1 ~ 1, # BTEC National
      W9VCQUC0X == 1 ~ 1, # BTEC First
      W9VCQUC0Y == 1 ~ 1, # SCOTVEC National Certificate
      W9VCQUC0Z == 1 ~ 1, # SCOTVEC first or general diploma
      W9VCQUCAA == 1 ~ 1, # SCOTVEC general diploma
      W9VCQUCAB == 1 ~ 1, # SCOTVEC modules
      W9VCQUCAC == 1 ~ 0, # HND or HNC
      W9VCQUCAD == 1 ~ 1, # OND or ONCM
      W9VCQUCAE == 1 ~ 1, # Junior certificate
      W9VCQUCAF == 1 ~ 1, # Other vocational qualifications (including some overseas)
      W9VCQUCAG == 1 ~ 4, # None of these qualifications
      W9VCQUCAH == 1 ~ -8, # Don't know
      W9VCQUCAI == 1 ~ -9, # Refused
      TRUE ~ 3
    ),
    W9ECONACT2 %in% 1:5 ~ 5, # Not studying
    W9ECONACT2 %in% 8:14 ~ 5, # Other activities
    W9ECONACT2 %in% -9:-1 ~ -3, # Missing values
    TRUE ~ -3
  ))

# Select only the required variables
cleaned_data <- merged_data %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write the cleaned data to a CSV file
write_csv(cleaned_data, "data/output/cleaned_data.csv")
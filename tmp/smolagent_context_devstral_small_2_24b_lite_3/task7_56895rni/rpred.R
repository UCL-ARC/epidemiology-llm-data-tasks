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

# Merge datasets using NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define the harmonized 6-category scheme for educational aims
# The scheme will be based on the NVQ levels:
# 1. NVQ 5 or equivalent (e.g., Doctorate, Masters)
# 2. NVQ 4 or equivalent (e.g., Undergraduate, Higher Degree)
# 3. NVQ 3 or equivalent (e.g., A/AS Levels, AVCE)
# 4. NVQ 2 or equivalent (e.g., GCSE grades A-C)
# 5. NVQ 1 or equivalent (e.g., GCSE grades D-G)
# 6. Other or no qualifications

# Function to map source variables to the harmonized scheme
harmonize_educaim <- function(data, wave_var, wave) {
  # Initialize the output variable with missing values
  educaim_var <- rep(-3, nrow(data))
  
  # Map source variable values to harmonized categories based on wave-specific metadata
  if (wave == 17) {
    # Wave 4 (Age 17) - w4saim
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 1] <- 3  # NVQ 3
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 2] <- 3  # AVCE
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 3] <- 3  # A/AS
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 4] <- 3  # Other level 3
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 5] <- 2  # NVQ 2
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 6] <- 2  # Intermediate GNVQ
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 7] <- 2  # Other level 2
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 8] <- 2  # GCSE
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 9] <- 1  # NVQ 1
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 10] <- 1 # Foundation
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 11] <- 1 # Other level 1
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 12] <- 6  # Other
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 13] <- 6  # No detail
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 14] <- 6  # Not studying

    # Handle missing values
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] %in% c(-999, -998, -997, -995)] <- -2  # Schedule not applicable
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] %in% c(-94, -92)] <- -9  # Refusal
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] %in% c(-91)] <- -1  # Not applicable
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] %in% c(-99)] <- -3  # Not interviewed
  } else if (wave == 19) {
    # Wave 6 (Age 19) - W6Saim
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 1] <- 5  # NVQ 5
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 2] <- 5  # First/Other Degree
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 3] <- 4  # NVQ 4
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 4] <- 4  # Other HE
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 5] <- 3  # NVQ 3
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 6] <- 3  # AVCE
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 7] <- 3  # A/AS
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 8] <- 3  # Other level 3
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 9] <- 2  # NVQ 2
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 10] <- 2 # Other level 2
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 11] <- 2 # GCSE
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 12] <- 1 # NVQ 1
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 13] <- 1 # Other level 1
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 14] <- 6 # Other (level unknown)
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 15] <- 6 # No detail
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 16] <- 6 # Not studying

    # Handle missing values
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] %in% c(-999, -998, -997, -995)] <- -2  # Schedule not applicable
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] %in% c(-94, -92)] <- -9  # Refusal
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] %in% c(-91)] <- -1  # Not applicable
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] %in% c(-99)] <- -3  # Not interviewed
  } else if (wave == 20) {
    # Wave 7 (Age 20) - W7SAim
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 1] <- 1  # NVQ 1
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 2] <- 1  # Other level 1
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 3] <- 2  # NVQ 2
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 4] <- 2  # GCSE
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 5] <- 2  # Other level 2
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 6] <- 3  # NVQ 3
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 7] <- 3  # A/AS
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 8] <- 3  # AVCE
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 9] <- 3  # Other level 3
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 10] <- 4  # NVQ 4
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 11] <- 5  # First/Other Degree
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 12] <- 4  # Other HE
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 13] <- 5  # NVQ 5
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == 14] <- 6  # Other (level unknown)

    # Handle missing values
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == -94] <- -8  # Insufficient information
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] == -91] <- -1  # Not applicable (not studying)
    educaim_var[!is.na(data[[wave_var]]) & data[[wave_var]] %in% c(-999, -998, -997, -995)] <- -2  # Schedule not applicable
  } else if (wave == 25) {
    # Wave 8 (Age 25) - Derived from academic and vocational qualifications
    # Initialize with -3 (Not interviewed)
    educaim_var <- rep(-3, nrow(data))

    # Check if currently studying
    studying_indices <- which(!is.na(data$W8ACTIVITY05) & data$W8ACTIVITY05 == 1)
    educaim_var[studying_indices] <- 6  # Default to Other

    # Map academic qualifications to NVQ levels
    educaim_var[!is.na(data$W8ACQUC0A) & data$W8ACQUC0A == 1] <- 5  # University Higher Degree
    educaim_var[!is.na(data$W8ACQUC0B) & data$W8ACQUC0B == 1] <- 5  # First degree level qualification
    educaim_var[!is.na(data$W8ACQUC0C) & data$W8ACQUC0C == 1] <- 4  # Diploma in higher education
    educaim_var[!is.na(data$W8ACQUC0D) & data$W8ACQUC0D == 1] <- 4  # Teaching qualification (excl PGCE)
    educaim_var[!is.na(data$W8ACQUC0E) & data$W8ACQUC0E == 1] <- 4  # Nursing or other medical qualification
    educaim_var[!is.na(data$W8ACQUC0F) & data$W8ACQUC0F == 1] <- 3  # A Level
    educaim_var[!is.na(data$W8ACQUC0G) & data$W8ACQUC0G == 1] <- 3  # Welsh Baccalaureate
    educaim_var[!is.na(data$W8ACQUC0H) & data$W8ACQUC0H == 1] <- 3  # International Baccalaureate
    educaim_var[!is.na(data$W8ACQUC0I) & data$W8ACQUC0I == 1] <- 3  # AS Level
    educaim_var[!is.na(data$W8ACQUC0J) & data$W8ACQUC0J == 1] <- 3  # Higher Grade/Advanced Higher (Scotland)
    educaim_var[!is.na(data$W8ACQUC0K) & data$W8ACQUC0K == 1] <- 3  # Certificate of sixth year studies
    educaim_var[!is.na(data$W8ACQUC0L) & data$W8ACQUC0L == 1] <- 2  # GCSE
    educaim_var[!is.na(data$W8ACQUC0M) & data$W8ACQUC0M == 1] <- 2  # Standard Grade / Lower (Scotland)
    educaim_var[!is.na(data$W8ACQUC0N) & data$W8ACQUC0N == 1] <- 2  # Other school
    educaim_var[!is.na(data$W8VCQUC0J) & data$W8VCQUC0J == 1] <- 3  # NVQ/SVQ - Level 3 - 5
    educaim_var[!is.na(data$W8VCQUC0K) & data$W8VCQUC0K == 1] <- 4  # HNC/HND

    # Handle missing values
    educaim_var[!is.na(data$W8ACTIVITY05) & data$W8ACTIVITY05 == -9] <- -9  # Refused
    educaim_var[!is.na(data$W8ACTIVITY05) & data$W8ACTIVITY05 == -8] <- -8  # Don't know
    educaim_var[!is.na(data$W8ACTIVITY05) & data$W8ACTIVITY05 == -1] <- -1  # Not applicable
  } else if (wave == 32) {
    # Wave 9 (Age 32) - Derived from academic and vocational qualifications
    # Initialize with -3 (Not asked at fieldwork stage)
    educaim_var <- rep(-3, nrow(data))

    # Check if currently studying
    studying_indices <- which(!is.na(data$W9ECONACT2) & data$W9ECONACT2 %in% c(6, 7))
    educaim_var[studying_indices] <- 6  # Default to Other

    # Map academic qualifications to NVQ levels
    educaim_var[!is.na(data$W9ACQUC0A) & data$W9ACQUC0A == 1] <- 5  # Doctorate or equivalent
    educaim_var[!is.na(data$W9ACQUC0B) & data$W9ACQUC0B == 1] <- 5  # Masters or equivalent
    educaim_var[!is.na(data$W9ACQUC0C) & data$W9ACQUC0C == 1] <- 4  # Undergraduate or equivalent
    educaim_var[!is.na(data$W9ACQUC0D) & data$W9ACQUC0D == 1] <- 4  # Post-graduate Diplomas and Certificates
    educaim_var[!is.na(data$W9ACQUC0E) & data$W9ACQUC0E == 1] <- 4  # Diplomas in higher education and other higher education qualifications
    educaim_var[!is.na(data$W9ACQUC0F) & data$W9ACQUC0F == 1] <- 4  # Teaching qualifications for schools or further education (below degree level)
    educaim_var[!is.na(data$W9ACQUC0G) & data$W9ACQUC0G == 1] <- 3  # A/AS Levels or equivalent
    educaim_var[!is.na(data$W9ACQUC0H) & data$W9ACQUC0H == 1] <- 2  # Grade A-C, Level 4-9
    educaim_var[!is.na(data$W9ACQUC0I) & data$W9ACQUC0I == 1] <- 1  # Grade D-G, Level 1-3
    educaim_var[!is.na(data$W9ACQUC0J) & data$W9ACQUC0J == 1] <- 3  # SCE Higher
    educaim_var[!is.na(data$W9ACQUC0K) & data$W9ACQUC0K == 1] <- 3  # Scottish Certificate Sixth Year Studies
    educaim_var[!is.na(data$W9ACQUC0L) & data$W9ACQUC0L == 1] <- 2  # SCE Standard
    educaim_var[!is.na(data$W9ACQUC0M) & data$W9ACQUC0M == 1] <- 2  # National 4 and 5
    educaim_var[!is.na(data$W9ACQUC0N) & data$W9ACQUC0N == 1] <- 1  # National 2 and 3
    educaim_var[!is.na(data$W9ACQUC0O) & data$W9ACQUC0O == 1] <- 2  # Leaving Certificate
    educaim_var[!is.na(data$W9ACQUC0P) & data$W9ACQUC0P == 1] <- 2  # Junior Certificate grade A-C
    educaim_var[!is.na(data$W9ACQUC0Q) & data$W9ACQUC0Q == 1] <- 1  # Junior Certificate grade D and below
    educaim_var[!is.na(data$W9VCQUC0A) & data$W9VCQUC0A == 1] <- 5  # Professional qualifications at degree level
    educaim_var[!is.na(data$W9VCQUC0B) & data$W9VCQUC0B == 1] <- 4  # Nursing or other medical qualifications (below degree level)
    educaim_var[!is.na(data$W9VCQUC0C) & data$W9VCQUC0C == 1] <- 4  # Level 4 or 5
    educaim_var[!is.na(data$W9VCQUC0D) & data$W9VCQUC0D == 1] <- 3  # Level 3
    educaim_var[!is.na(data$W9VCQUC0E) & data$W9VCQUC0E == 1] <- 2  # Level 2
    educaim_var[!is.na(data$W9VCQUC0F) & data$W9VCQUC0F == 1] <- 1  # Level 1
    educaim_var[!is.na(data$W9VCQUC0G) & data$W9VCQUC0G == 1] <- 3  # GNVQ Advanced
    educaim_var[!is.na(data$W9VCQUC0H) & data$W9VCQUC0H == 1] <- 2  # GNVQ Intermediate
    educaim_var[!is.na(data$W9VCQUC0I) & data$W9VCQUC0I == 1] <- 3  # Level 3
    educaim_var[!is.na(data$W9VCQUC0J) & data$W9VCQUC0J == 1] <- 2  # Level 2
    educaim_var[!is.na(data$W9VCQUC0K) & data$W9VCQUC0K == 1] <- 1  # Level Foundation
    educaim_var[!is.na(data$W9VCQUC0L) & data$W9VCQUC0L == 1] <- 3  # Advanced Craft, Part III
    educaim_var[!is.na(data$W9VCQUC0M) & data$W9VCQUC0M == 1] <- 2  # Craft, Part II
    educaim_var[!is.na(data$W9VCQUC0N) & data$W9VCQUC0N == 1] <- 1  # Craft, Part I
    educaim_var[!is.na(data$W9VCQUC0O) & data$W9VCQUC0O == 1] <- 3  # Level 3
    educaim_var[!is.na(data$W9VCQUC0P) & data$W9VCQUC0P == 1] <- 2  # Level 2
    educaim_var[!is.na(data$W9VCQUC0Q) & data$W9VCQUC0Q == 1] <- 1  # Level 1
    educaim_var[!is.na(data$W9VCQUC0R) & data$W9VCQUC0R == 1] <- 3  # Advanced Diploma
    educaim_var[!is.na(data$W9VCQUC0S) & data$W9VCQUC0S == 1] <- 4  # Higher Diploma
    educaim_var[!is.na(data$W9VCQUC0T) & data$W9VCQUC0T == 1] <- 3  # RSA Diploma
    educaim_var[!is.na(data$W9VCQUC0U) & data$W9VCQUC0U == 1] <- 2  # RSA Stage I, II,III
    educaim_var[!is.na(data$W9VCQUC0V) & data$W9VCQUC0V == 1] <- 4  # Higher Level BTEC
    educaim_var[!is.na(data$W9VCQUC0W) & data$W9VCQUC0W == 1] <- 3  # BTEC National
    educaim_var[!is.na(data$W9VCQUC0X) & data$W9VCQUC0X == 1] <- 2  # BTEC First
    educaim_var[!is.na(data$W9VCQUC0Y) & data$W9VCQUC0Y == 1] <- 3  # SCOTVEC National Certificate
    educaim_var[!is.na(data$W9VCQUC0Z) & data$W9VCQUC0Z == 1] <- 2  # SCOTVEC first or general diploma
    educaim_var[!is.na(data$W9VCQUCAA) & data$W9VCQUCAA == 1] <- 2  # SCOTVEC general diploma
    educaim_var[!is.na(data$W9VCQUCAB) & data$W9VCQUCAB == 1] <- 2  # SCOTVEC modules
    educaim_var[!is.na(data$W9VCQUCAC) & data$W9VCQUCAC == 1] <- 4  # HND or HNC
    educaim_var[!is.na(data$W9VCQUCAD) & data$W9VCQUCAD == 1] <- 3  # OND or ONCM
    educaim_var[!is.na(data$W9VCQUCAE) & data$W9VCQUCAE == 1] <- 1  # Junior certificate
    educaim_var[!is.na(data$W9VCQUCAF) & data$W9VCQUCAF == 1] <- 6  # Other vocational qualifications (including some overseas)

    # Handle missing values
    educaim_var[!is.na(data$W9ECONACT2) & data$W9ECONACT2 == -9] <- -9  # Refused
    educaim_var[!is.na(data$W9ECONACT2) & data$W9ECONACT2 == -8] <- -8  # Don't know
    educaim_var[!is.na(data$W9ECONACT2) & data$W9ECONACT2 == -3] <- -3  # Not asked at fieldwork stage
    educaim_var[!is.na(data$W9ECONACT2) & data$W9ECONACT2 == -1] <- -1  # Not applicable
  }
  
  return(educaim_var)
}

# Derive time-varying educational aims variables
merged_data$educaim17 <- harmonize_educaim(merged_data, "w4saim", 17)
merged_data$educaim19 <- harmonize_educaim(merged_data, "W6Saim", 19)
merged_data$educaim20 <- harmonize_educaim(merged_data, "W7SAim", 20)
merged_data$educaim25 <- harmonize_educaim(merged_data, "W8ACTIVITY05", 25)
merged_data$educaim32 <- harmonize_educaim(merged_data, "W9ECONACT2", 32)

# Select only the ID and derived variables
cleaned_data <- merged_data %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write the cleaned data to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return the path to the cleaned data file
"data/output/cleaned_data.csv"
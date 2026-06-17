library(readr)
library(dplyr)
library(haven)

# Load datasets
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets using NSID
merged_data <- wave4 %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define the harmonised coding scheme
educaim_coding <- function(wave_data, age) {
  # Initialize the output variable with missing values
  educaim_var <- rep(-3, nrow(wave_data))

  if (age == 17) {
    # Wave 4 (Age 17): w4saim
    educaim_var[wave_data$w4saim == 1] <- 1  # NVQ 3
    educaim_var[wave_data$w4saim == 2] <- 1  # AVCE
    educaim_var[wave_data$w4saim == 3] <- 1  # A/AS
    educaim_var[wave_data$w4saim == 4] <- 1  # Other level 3
    educaim_var[wave_data$w4saim == 5] <- 1  # NVQ 2
    educaim_var[wave_data$w4saim == 6] <- 1  # Intermediate GNVQ
    educaim_var[wave_data$w4saim == 7] <- 1  # Other level 2
    educaim_var[wave_data$w4saim == 8] <- 1  # GCSE
    educaim_var[wave_data$w4saim == 9] <- 2  # NVQ 1
    educaim_var[wave_data$w4saim == 10] <- 2  # Foundation
    educaim_var[wave_data$w4saim == 11] <- 2  # Other level 1
    educaim_var[wave_data$w4saim == 12] <- 3  # Other
    educaim_var[wave_data$w4saim == 13] <- 3  # No detail
    educaim_var[wave_data$w4saim == 14] <- 5  # Not studying
    educaim_var[is.na(wave_data$w4saim)] <- -3  # Not interviewed
  } else if (age == 19) {
    # Wave 6 (Age 19): W6Saim
    educaim_var[wave_data$W6Saim == 1] <- 0  # NVQ 5
    educaim_var[wave_data$W6Saim == 2] <- 0  # First/Other Degree
    educaim_var[wave_data$W6Saim == 3] <- 0  # NVQ 4
    educaim_var[wave_data$W6Saim == 4] <- 0  # Other HE
    educaim_var[wave_data$W6Saim == 5] <- 1  # NVQ 3
    educaim_var[wave_data$W6Saim == 6] <- 1  # AVCE
    educaim_var[wave_data$W6Saim == 7] <- 1  # A/AS
    educaim_var[wave_data$W6Saim == 8] <- 1  # Other level 3
    educaim_var[wave_data$W6Saim == 9] <- 1  # NVQ 2
    educaim_var[wave_data$W6Saim == 10] <- 1  # Other level 2
    educaim_var[wave_data$W6Saim == 11] <- 1  # GCSE
    educaim_var[wave_data$W6Saim == 12] <- 2  # NVQ 1
    educaim_var[wave_data$W6Saim == 13] <- 2  # Other level 1
    educaim_var[wave_data$W6Saim == 14] <- 3  # Other (level unknown)
    educaim_var[wave_data$W6Saim == 15] <- 3  # No detail
    educaim_var[wave_data$W6Saim == 16] <- 5  # Not studying
    educaim_var[is.na(wave_data$W6Saim)] <- -3  # Not interviewed
  } else if (age == 20) {
    # Wave 7 (Age 20): W7SAim
    educaim_var[wave_data$W7SAim == 1] <- 2  # NVQ 1
    educaim_var[wave_data$W7SAim == 2] <- 2  # Other level 1
    educaim_var[wave_data$W7SAim == 3] <- 1  # NVQ 2
    educaim_var[wave_data$W7SAim == 4] <- 1  # GCSE
    educaim_var[wave_data$W7SAim == 5] <- 1  # Other level 2
    educaim_var[wave_data$W7SAim == 6] <- 1  # NVQ 3
    educaim_var[wave_data$W7SAim == 7] <- 1  # A/AS
    educaim_var[wave_data$W7SAim == 8] <- 1  # AVCE
    educaim_var[wave_data$W7SAim == 9] <- 1  # Other level 3
    educaim_var[wave_data$W7SAim == 10] <- 0  # NVQ 4
    educaim_var[wave_data$W7SAim == 11] <- 0  # First/Other Degree
    educaim_var[wave_data$W7SAim == 12] <- 0  # Other HE
    educaim_var[wave_data$W7SAim == 13] <- 0  # NVQ 5
    educaim_var[wave_data$W7SAim == 14] <- 3  # Other (level unknown)
    educaim_var[wave_data$W7SAim == -91] <- 5  # Not applicable (not studying)
    educaim_var[wave_data$W7SAim == -94] <- -8  # Insufficient information
    educaim_var[is.na(wave_data$W7SAim)] <- -3  # Not interviewed
  } else if (age == 25) {
    # Wave 8 (Age 25): Check economic activity and qualification indicators
    educaim_var[wave_data$W8ACTIVITY05 == 0] <- 5  # Not currently studying
    educaim_var[wave_data$W8ACQUC0A == 1 | wave_data$W8ACQUC0B == 1 | wave_data$W8ACQUC0C == 1 | 
                wave_data$W8ACQUC0D == 1 | wave_data$W8ACQUC0E == 1] <- 0  # NVQ 4-5
    educaim_var[wave_data$W8ACQUC0F == 1 | wave_data$W8ACQUC0G == 1 | wave_data$W8ACQUC0H == 1 | 
                wave_data$W8ACQUC0I == 1 | wave_data$W8ACQUC0J == 1 | wave_data$W8ACQUC0K == 1 | 
                wave_data$W8ACQUC0L == 1 | wave_data$W8ACQUC0M == 1 | wave_data$W8ACQUC0N == 1] <- 1  # NVQ 1-3
    educaim_var[wave_data$W8VCQUC0D == 1] <- 2  # Entry level
    educaim_var[wave_data$W8VCQUC0A == 1 | wave_data$W8VCQUC0B == 1 | wave_data$W8VCQUC0C == 1 | 
                wave_data$W8VCQUC0E == 1 | wave_data$W8VCQUC0J == 1 | wave_data$W8VCQUC0K == 1] <- 3  # Other
    educaim_var[wave_data$W8ACQUC0O == 1] <- 4  # None of these
    educaim_var[wave_data$W8ACQUC0P == 1] <- -8  # Don't know
    educaim_var[wave_data$W8ACQUC0Q == 1] <- -9  # Refused
    educaim_var[is.na(wave_data$W8ACTIVITY05) & is.na(wave_data$W8ACQUC0A) & is.na(wave_data$W8ACQUC0B) & 
                is.na(wave_data$W8ACQUC0C) & is.na(wave_data$W8ACQUC0D) & is.na(wave_data$W8ACQUC0E) & 
                is.na(wave_data$W8ACQUC0F) & is.na(wave_data$W8ACQUC0G) & is.na(wave_data$W8ACQUC0H) & 
                is.na(wave_data$W8ACQUC0I) & is.na(wave_data$W8ACQUC0J) & is.na(wave_data$W8ACQUC0K) & 
                is.na(wave_data$W8ACQUC0L) & is.na(wave_data$W8ACQUC0M) & is.na(wave_data$W8ACQUC0N) & 
                is.na(wave_data$W8ACQUC0O) & is.na(wave_data$W8ACQUC0P) & is.na(wave_data$W8ACQUC0Q) & 
                is.na(wave_data$W8VCQUC0A) & is.na(wave_data$W8VCQUC0B) & is.na(wave_data$W8VCQUC0C) & 
                is.na(wave_data$W8VCQUC0D) & is.na(wave_data$W8VCQUC0E) & is.na(wave_data$W8VCQUC0J) & 
                is.na(wave_data$W8VCQUC0K)] <- -3  # Not interviewed
  } else if (age == 32) {
    # Wave 9 (Age 32): Check economic activity and qualification indicators
    educaim_var[wave_data$W9ECONACT2 == 6 | wave_data$W9ECONACT2 == 7] <- 5  # In full-time or part-time education
    educaim_var[wave_data$W9ACQUC0A == 1 | wave_data$W9ACQUC0B == 1 | wave_data$W9ACQUC0C == 1 | 
                wave_data$W9ACQUC0D == 1 | wave_data$W9ACQUC0E == 1] <- 0  # NVQ 4-5
    educaim_var[wave_data$W9ACQUC0F == 1 | wave_data$W9ACQUC0G == 1 | wave_data$W9ACQUC0H == 1 | 
                wave_data$W9ACQUC0I == 1 | wave_data$W9ACQUC0J == 1 | wave_data$W9ACQUC0K == 1 | 
                wave_data$W9ACQUC0L == 1 | wave_data$W9ACQUC0M == 1 | wave_data$W9ACQUC0N == 1 | 
                wave_data$W9ACQUC0O == 1 | wave_data$W9ACQUC0P == 1 | wave_data$W9ACQUC0Q == 1 | 
                wave_data$W9ACQUC0R == 1] <- 1  # NVQ 1-3
    educaim_var[wave_data$W9VCQUC0F == 1] <- 2  # Entry level
    educaim_var[wave_data$W9VCQUC0A == 1 | wave_data$W9VCQUC0B == 1 | wave_data$W9VCQUC0C == 1 | 
                wave_data$W9VCQUC0D == 1 | wave_data$W9VCQUC0E == 1 | wave_data$W9VCQUC0G == 1 | 
                wave_data$W9VCQUC0H == 1 | wave_data$W9VCQUC0I == 1 | wave_data$W9VCQUC0J == 1 | 
                wave_data$W9VCQUC0K == 1 | wave_data$W9VCQUC0L == 1 | wave_data$W9VCQUC0M == 1 | 
                wave_data$W9VCQUC0N == 1 | wave_data$W9VCQUC0O == 1 | wave_data$W9VCQUC0P == 1 | 
                wave_data$W9VCQUC0Q == 1 | wave_data$W9VCQUC0R == 1 | wave_data$W9VCQUC0S == 1 | 
                wave_data$W9VCQUC0T == 1 | wave_data$W9VCQUC0U == 1 | wave_data$W9VCQUC0V == 1 | 
                wave_data$W9VCQUC0W == 1 | wave_data$W9VCQUC0X == 1 | wave_data$W9VCQUC0Y == 1 | 
                wave_data$W9VCQUC0Z == 1 | wave_data$W9VCQUCAA == 1 | wave_data$W9VCQUCAB == 1 | 
                wave_data$W9VCQUCAC == 1 | wave_data$W9VCQUCAD == 1 | wave_data$W9VCQUCAE == 1 | 
                wave_data$W9VCQUCAF == 1] <- 3  # Other
    educaim_var[wave_data$W9ACQUC0S == 1] <- 4  # None of these
    educaim_var[wave_data$W9ACQUC0T == 1] <- -8  # Don't know
    educaim_var[wave_data$W9ACQUC0U == 1] <- -9  # Refused
    educaim_var[is.na(wave_data$W9ECONACT2) & is.na(wave_data$W9ACQUC0A) & is.na(wave_data$W9ACQUC0B) & 
                is.na(wave_data$W9ACQUC0C) & is.na(wave_data$W9ACQUC0D) & is.na(wave_data$W9ACQUC0E) & 
                is.na(wave_data$W9ACQUC0F) & is.na(wave_data$W9ACQUC0G) & is.na(wave_data$W9ACQUC0H) & 
                is.na(wave_data$W9ACQUC0I) & is.na(wave_data$W9ACQUC0J) & is.na(wave_data$W9ACQUC0K) & 
                is.na(wave_data$W9ACQUC0L) & is.na(wave_data$W9ACQUC0M) & is.na(wave_data$W9ACQUC0N) & 
                is.na(wave_data$W9ACQUC0O) & is.na(wave_data$W9ACQUC0P) & is.na(wave_data$W9ACQUC0Q) & 
                is.na(wave_data$W9ACQUC0R) & is.na(wave_data$W9ACQUC0S) & is.na(wave_data$W9ACQUC0T) & 
                is.na(wave_data$W9ACQUC0U) & is.na(wave_data$W9VCQUC0A) & is.na(wave_data$W9VCQUC0B) & 
                is.na(wave_data$W9VCQUC0C) & is.na(wave_data$W9VCQUC0D) & is.na(wave_data$W9VCQUC0E) & 
                is.na(wave_data$W9VCQUC0F) & is.na(wave_data$W9VCQUC0G) & is.na(wave_data$W9VCQUC0H) & 
                is.na(wave_data$W9VCQUC0I) & is.na(wave_data$W9VCQUC0J) & is.na(wave_data$W9VCQUC0K) & 
                is.na(wave_data$W9VCQUC0L) & is.na(wave_data$W9VCQUC0M) & is.na(wave_data$W9VCQUC0N) & 
                is.na(wave_data$W9VCQUC0O) & is.na(wave_data$W9VCQUC0P) & is.na(wave_data$W9VCQUC0Q) & 
                is.na(wave_data$W9VCQUC0R) & is.na(wave_data$W9VCQUC0S) & is.na(wave_data$W9VCQUC0T) & 
                is.na(wave_data$W9VCQUC0U) & is.na(wave_data$W9VCQUC0V) & is.na(wave_data$W9VCQUC0W) & 
                is.na(wave_data$W9VCQUC0X) & is.na(wave_data$W9VCQUC0Y) & is.na(wave_data$W9VCQUC0Z) & 
                is.na(wave_data$W9VCQUCAA) & is.na(wave_data$W9VCQUCAB) & is.na(wave_data$W9VCQUCAC) & 
                is.na(wave_data$W9VCQUCAD) & is.na(wave_data$W9VCQUCAE) & is.na(wave_data$W9VCQUCAF) & 
                is.na(wave_data$W9VCQUCAG) & is.na(wave_data$W9VCQUCAH) & is.na(wave_data$W9VCQUCAI)] <- -3  # Not interviewed
  }

  return(educaim_var)
}

# Apply the coding function to each wave
merged_data$educaim17 <- educaim_coding(merged_data, 17)
merged_data$educaim19 <- educaim_coding(merged_data, 19)
merged_data$educaim20 <- educaim_coding(merged_data, 20)
merged_data$educaim25 <- educaim_coding(merged_data, 25)
merged_data$educaim32 <- educaim_coding(merged_data, 32)

# Select only the ID and derived variables
output_data <- merged_data %>% select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

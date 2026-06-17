library(dplyr)
library(readr)
library(labelled)
library(tidyr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE)

# Define the 6-category scheme:
# 1: Higher Education / NVQ 5
# 2: NVQ 4
# 3: NVQ 3
# 4: NVQ 2
# 5: NVQ 1 / GCSE / Level 1
# 6: Not studying

# Load all waves
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge all waves by NSID
data <- full_join(wave1, wave4, by = "NSID")
data <- full_join(data, wave6, by = "NSID")
data <- full_join(data, wave7, by = "NSID")
data <- full_join(data, wave8, by = "NSID")
data <- full_join(data, wave9, by = "NSID")

# Function to map missing values to standard codes
# NA values in the source are treated as -3 (not asked) by default
map_missing_generic <- function(x) {
  # First, handle NA values
  result <- ifelse(is.na(x), -3, x)
  # Map specific missing codes by label meaning
  result[result == -9] <- -9    # Refusal
  result[result == -8] <- -8    # Don't know
  result[result == -7] <- -7    # Prefer not to say
  result[result == -3] <- -3    # Not asked at fieldwork stage
  result[result == -2] <- -2    # Schedule not applicable
  result[result == -1] <- -1    # Item not applicable
  result[result %in% c(-999, -998, -997, -995)] <- -2  # Schedule not applicable / script error
  result[result == -94] <- -8   # Insufficient information
  result[result == -92] <- -9   # Refusal
  result[result == -91] <- -1   # Not applicable
  result[result == -99] <- -3   # Not asked
  result[result %in% c(-100, -97)] <- -2  # Depends on label
  result[result == -10] <- -3   # Not asked
  # All other negative values -> -3 (default for unhandled missing codes)
  result[result < 0 & !result %in% c(-9, -8, -7, -3, -2, -1, -999, -998, -997, -995, -94, -92, -91, -99, -100, -97, -10)] <- -3
  return(result)
}

# Create educaim17 from w4saim (Wave 4, Age 17)
data <- data %>%
  mutate(
    educaim17 = case_when(
      w4saim == 1 ~ 3,    # NVQ 3
      w4saim == 2 ~ 3,    # AVCE
      w4saim == 3 ~ 3,    # A/AS
      w4saim == 4 ~ 3,    # Other level 3
      w4saim == 5 ~ 4,    # NVQ 2
      w4saim == 6 ~ 4,    # Intermediate GNVQ
      w4saim == 7 ~ 4,    # Other level 2
      w4saim == 8 ~ 5,    # GCSE
      w4saim == 9 ~ 5,    # NVQ 1
      w4saim == 10 ~ 5,   # Foundation
      w4saim == 11 ~ 5,   # Other level 1
      w4saim == 12 ~ 5,   # Other
      w4saim == 13 ~ 5,   # No detail
      w4saim == 14 ~ 6,   # Not studying
      TRUE ~ NA_real_
    )
  )

# Map missing values for educaim17
data <- data %>%
  mutate(
    educaim17 = ifelse(is.na(educaim17), map_missing_generic(w4saim), educaim17)
  )

# Create educaim19 from W6Saim (Wave 6, Age 19)
data <- data %>%
  mutate(
    educaim19 = case_when(
      W6Saim == 1 ~ 1,    # NVQ 5
      W6Saim == 2 ~ 1,    # First/Other Degree
      W6Saim == 3 ~ 2,    # NVQ 4
      W6Saim == 4 ~ 1,    # Other HE
      W6Saim == 5 ~ 3,    # NVQ 3
      W6Saim == 6 ~ 3,    # AVCE
      W6Saim == 7 ~ 3,    # A/AS
      W6Saim == 8 ~ 3,    # Other level 3
      W6Saim == 9 ~ 4,    # NVQ 2
      W6Saim == 10 ~ 4,   # Other level 2
      W6Saim == 11 ~ 5,   # GCSE
      W6Saim == 12 ~ 5,   # NVQ 1
      W6Saim == 13 ~ 5,   # Other level 1
      W6Saim == 14 ~ 5,   # Other (level unknown)
      W6Saim == 15 ~ 5,   # No detail
      W6Saim == 16 ~ 6,   # Not studying
      TRUE ~ NA_real_
    )
  )

# Map missing values for educaim19
data <- data %>%
  mutate(
    educaim19 = ifelse(is.na(educaim19), map_missing_generic(W6Saim), educaim19)
  )

# Create educaim20 from W7SAim (Wave 7, Age 20)
data <- data %>%
  mutate(
    educaim20 = case_when(
      W7SAim == -94 ~ -8,  # Insufficient information
      W7SAim == -91 ~ 6,   # Not applicable (not studying)
      W7SAim == 1 ~ 5,     # NVQ 1
      W7SAim == 2 ~ 5,     # Other level 1
      W7SAim == 3 ~ 4,     # NVQ 2
      W7SAim == 4 ~ 5,     # GCSE
      W7SAim == 5 ~ 4,     # Other level 2
      W7SAim == 6 ~ 3,     # NVQ 3
      W7SAim == 7 ~ 3,     # A/AS
      W7SAim == 8 ~ 3,     # AVCE
      W7SAim == 9 ~ 3,     # Other level 3
      W7SAim == 10 ~ 2,    # NVQ 4
      W7SAim == 11 ~ 1,    # First/Other Degree
      W7SAim == 12 ~ 1,    # Other HE
      W7SAim == 13 ~ 1,    # NVQ 5
      W7SAim == 14 ~ 5,    # Other (level unknown)
      TRUE ~ NA_real_
    )
  )

# Map missing values for educaim20
data <- data %>%
  mutate(
    educaim20 = ifelse(is.na(educaim20), map_missing_generic(W7SAim), educaim20)
  )

# Create educaim25 from Wave 8 variables (Age 25)
# First check if W8ACTIVITY05 indicates education status
data <- data %>%
  mutate(
    educaim25 = case_when(
      # Missing values in W8ACTIVITY05 will be handled by map_missing_generic
      W8ACTIVITY05 == -9 ~ -9,    # Refused
      W8ACTIVITY05 == -8 ~ -8,    # Don't know
      W8ACTIVITY05 == -3 ~ -3,    # Not asked
      W8ACTIVITY05 == -2 ~ -2,    # Schedule not applicable
      W8ACTIVITY05 == -1 ~ -1,    # Not applicable
      # Not in education
      W8ACTIVITY05 == 0 ~ 6,      # No - Not studying
      # In education - derive from qualification variables
      W8ACTIVITY05 == 1 ~ case_when(
        W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 ~ 1,  # HE qualifications
        W8VCQUC0J == 1 ~ 2,  # NVQ Level 3-5 (treat as NVQ 4)
        W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | 
        W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1 ~ 3,  # A Level / equivalent
        TRUE ~ 5  # Default to Level 1
      ),
      TRUE ~ NA_real_
    )
  )

# Map remaining missing values for educaim25
data <- data %>%
  mutate(
    educaim25 = ifelse(is.na(educaim25), map_missing_generic(W8ACTIVITY05), educaim25)
  )

# Create educaim32 from Wave 9 variables (Age 32)
# First check if W9ECONACT2 indicates education status
data <- data %>%
  mutate(
    educaim32 = case_when(
      # Missing values in W9ECONACT2 will be handled by map_missing_generic
      W9ECONACT2 == -9 ~ -9,    # Refused
      W9ECONACT2 == -8 ~ -8,    # Don't know
      W9ECONACT2 == -3 ~ -3,    # Not asked
      W9ECONACT2 == -2 ~ -2,    # Schedule not applicable
      W9ECONACT2 == -1 ~ -1,    # Not applicable
      # In education
      W9ECONACT2 %in% c(6, 7, 12) ~ case_when(
        W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 | 
        W9ACQUC0D == 1 | W9ACQUC0E == 1 | W9VCQUC0A == 1 | 
        W9VCQUC0S == 1 | W9VCQUCAC == 1 ~ 1,  # HE qualifications
        W9VCQUC0C == 1 | W9VCQUC0I == 1 | W9VCQUC0K == 1 ~ 2,  # Level 4-5 vocational
        W9VCQUC0D == 1 | W9VCQUC0O == 1 | 
        W9ACQUC0G == 1 ~ 3,  # Level 3 vocational or A/AS Levels
        TRUE ~ 5  # Default to Level 1
      ),
      # Not in education
      TRUE ~ 6  # Not studying
    )
  )

# Map remaining missing values for educaim32
data <- data %>%
  mutate(
    educaim32 = ifelse(is.na(educaim32), map_missing_generic(W9ECONACT2), educaim32)
  )

# Select only ID and derived variables
output <- data %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write output to CSV
write_csv(output, "data/output/cleaned_data.csv")

# Print summary
cat("Output dimensions:", dim(output), "\n")
cat("Number of NSIDs:", nrow(output), "\n")
cat("\neducaim17 summary:\n")
print(table(output$educaim17, useNA = "ifany"))
cat("\neducaim19 summary:\n")
print(table(output$educaim19, useNA = "ifany"))
cat("\neducaim20 summary:\n")
print(table(output$educaim20, useNA = "ifany"))
cat("\neducaim25 summary:\n")
print(table(output$educaim25, useNA = "ifany"))
cat("\neducaim32 summary:\n")
print(table(output$educaim32, useNA = "ifany"))

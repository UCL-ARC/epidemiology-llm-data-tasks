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

# Merge datasets
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define harmonised categories
educaim_categories <- c(
  "NVQ 4–5 equivalent (higher / HE-level qualifications)" = 0,
  "NVQ 1–3 equivalent (lower / mid-level qualifications)" = 1,
  "None / entry level" = 2,
  "Other (level unknown or unclassifiable)" = 3,
  "None of these qualifications" = 4,
  "Not currently studying" = 5
)

# Function to map missing values
map_missing <- function(x, wave) {
  if (wave == "wave4") {
    x <- na_if(x, -999)
    x <- na_if(x, -998)
    x <- na_if(x, -997)
    x <- na_if(x, -995)
  } else if (wave == "wave6") {
    x <- na_if(x, -999)
    x <- na_if(x, -998)
    x <- na_if(x, -997)
    x <- na_if(x, -995)
  } else if (wave == "wave7") {
    x <- na_if(x, -999)
    x <- na_if(x, -998)
    x <- na_if(x, -997)
    x <- na_if(x, -995)
    x <- na_if(x, -94)
    x <- na_if(x, -91)
  } else if (wave == "wave8") {
    x <- na_if(x, -9)
    x <- na_if(x, -8)
    x <- na_if(x, -1)
  } else if (wave == "wave9") {
    x <- na_if(x, -9)
    x <- na_if(x, -8)
    x <- na_if(x, -3)
    x <- na_if(x, -1)
  }
  return(x)
}

# Derive educaim17 (wave4)
merged_data <- merged_data %>%
  mutate(educaim17 = case_when(
    w4saim == 1 | w4saim == 2 | w4saim == 3 | w4saim == 4 ~ 1,  # NVQ 3, AVCE, A/AS, Other level 3
    w4saim == 5 | w4saim == 6 | w4saim == 7 | w4saim == 8 ~ 1,  # NVQ 2, Intermediate GNVQ, Other level 2, GCSE
    w4saim == 9 | w4saim == 10 | w4saim == 11 ~ 2,                # NVQ 1, Foundation, Other level 1
    w4saim == 12 ~ 3,                                              # Other
    w4saim == 13 ~ 4,                                              # No detail
    w4saim == 14 ~ 5,                                              # Not studying
    TRUE ~ as.numeric(NA)
  ))

# Derive educaim19 (wave6)
merged_data <- merged_data %>%
  mutate(educaim19 = case_when(
    W6Saim == 1 | W6Saim == 2 | W6Saim == 3 | W6Saim == 4 ~ 0,  # NVQ 5, First/Other Degree, NVQ 4, Other HE
    W6Saim == 5 | W6Saim == 6 | W6Saim == 7 | W6Saim == 8 ~ 1,  # NVQ 3, AVCE, A/AS, Other level 3
    W6Saim == 9 | W6Saim == 10 | W6Saim == 11 ~ 1,                # NVQ 2, Other level 2, GCSE
    W6Saim == 12 ~ 2,                                              # NVQ 1
    W6Saim == 13 ~ 2,                                              # Other level 1
    W6Saim == 14 ~ 3,                                              # Other (level unknown)
    W6Saim == 15 ~ 4,                                              # No detail
    W6Saim == 16 ~ 5,                                              # Not studying
    TRUE ~ as.numeric(NA)
  ))

# Derive educaim20 (wave7)
merged_data <- merged_data %>%
  mutate(educaim20 = case_when(
    W7SAim == 1 | W7SAim == 2 ~ 2,                                  # NVQ 1, Other level 1
    W7SAim == 3 | W7SAim == 4 | W7SAim == 5 ~ 1,                    # NVQ 2, GCSE, Other level 2
    W7SAim == 6 | W7SAim == 7 | W7SAim == 8 | W7SAim == 9 ~ 1,     # NVQ 3, A/AS, AVCE, Other level 3
    W7SAim == 10 | W7SAim == 11 | W7SAim == 12 | W7SAim == 13 ~ 0, # NVQ 4, First/Other Degree, Other HE, NVQ 5
    W7SAim == 14 ~ 3,                                              # Other (level unknown)
    W7SAim == -91 ~ 5,                                             # Not applicable (not studying)
    TRUE ~ as.numeric(NA)
  ))

# Derive educaim25 (wave8)
merged_data <- merged_data %>%
  mutate(
    educaim25 = case_when(
      W8ACTIVITY05 == 0 ~ 5,  # Not currently studying
      W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 | W8ACQUC0D == 1 | W8ACQUC0E == 1 ~ 0,  # NVQ 4–5 equivalent
      W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1 | W8ACQUC0L == 1 | W8ACQUC0M == 1 | W8ACQUC0N == 1 ~ 1,  # NVQ 1–3 equivalent
      W8VCQUC0A == 1 | W8VCQUC0B == 1 | W8VCQUC0C == 1 | W8VCQUC0D == 1 ~ 2,  # Entry level
      W8VCQUC0E == 1 | W8VCQUC0J == 1 | W8VCQUC0K == 1 ~ 3,  # Other (level unknown or unclassifiable)
      W8ACQUC0O == 1 ~ 4,  # None of these qualifications
      W8ACQUC0P == 1 ~ -8,  # Don't know
      W8ACQUC0Q == 1 ~ -9,  # Refused
      TRUE ~ -3  # Not asked at the fieldwork stage / not interviewed
    )
  )

# Derive educaim32 (wave9)
merged_data <- merged_data %>%
  mutate(
    educaim32 = case_when(
      W9ECONACT2 == 6 | W9ECONACT2 == 7 ~ NA_integer_,  # In full-time or part-time education
      W9ECONACT2 != 6 & W9ECONACT2 != 7 ~ 5,  # Not currently studying
      W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 | W9ACQUC0D == 1 | W9ACQUC0E == 1 ~ 0,  # NVQ 4–5 equivalent
      W9ACQUC0F == 1 | W9ACQUC0G == 1 | W9ACQUC0H == 1 | W9ACQUC0I == 1 | W9ACQUC0J == 1 | W9ACQUC0K == 1 | W9ACQUC0L == 1 | W9ACQUC0M == 1 | W9ACQUC0N == 1 | W9ACQUC0O == 1 | W9ACQUC0P == 1 | W9ACQUC0Q == 1 | W9ACQUC0R == 1 ~ 1,  # NVQ 1–3 equivalent
      W9VCQUC0A == 1 | W9VCQUC0B == 1 | W9VCQUC0C == 1 | W9VCQUC0D == 1 | W9VCQUC0E == 1 | W9VCQUC0F == 1 | W9VCQUC0G == 1 | W9VCQUC0H == 1 | W9VCQUC0I == 1 | W9VCQUC0J == 1 | W9VCQUC0K == 1 | W9VCQUC0L == 1 | W9VCQUC0M == 1 | W9VCQUC0N == 1 | W9VCQUC0O == 1 | W9VCQUC0P == 1 | W9VCQUC0Q == 1 | W9VCQUC0R == 1 | W9VCQUC0S == 1 | W9VCQUC0T == 1 | W9VCQUC0U == 1 | W9VCQUC0V == 1 | W9VCQUC0W == 1 | W9VCQUC0X == 1 | W9VCQUC0Y == 1 | W9VCQUC0Z == 1 | W9VCQUCAA == 1 | W9VCQUCAB == 1 | W9VCQUCAC == 1 | W9VCQUCAD == 1 | W9VCQUCAE == 1 | W9VCQUCAF == 1 ~ 2,  # Entry level
      W9VCQUC0G == 1 | W9VCQUC0H == 1 | W9VCQUC0I == 1 | W9VCQUC0J == 1 | W9VCQUC0K == 1 | W9VCQUC0L == 1 | W9VCQUC0M == 1 | W9VCQUC0N == 1 | W9VCQUC0O == 1 | W9VCQUC0P == 1 | W9VCQUC0Q == 1 | W9VCQUC0R == 1 | W9VCQUC0S == 1 | W9VCQUC0T == 1 | W9VCQUC0U == 1 | W9VCQUC0V == 1 | W9VCQUC0W == 1 | W9VCQUC0X == 1 | W9VCQUC0Y == 1 | W9VCQUC0Z == 1 | W9VCQUCAA == 1 | W9VCQUCAB == 1 | W9VCQUCAC == 1 | W9VCQUCAD == 1 | W9VCQUCAE == 1 | W9VCQUCAF == 1 | W9VCQUCAG == 1 ~ 3,  # Other (level unknown or unclassifiable)
      W9ACQUC0S == 1 ~ 4,  # None of these qualifications
      W9ACQUC0T == 1 ~ -8,  # Don't know
      W9ACQUC0U == 1 ~ -9,  # Refused
      TRUE ~ -3  # Not asked at the fieldwork stage / not interviewed
    )
  )

# Handle missing values
merged_data$educaim17 <- map_missing(merged_data$educaim17, "wave4")
merged_data$educaim19 <- map_missing(merged_data$educaim19, "wave6")
merged_data$educaim20 <- map_missing(merged_data$educaim20, "wave7")
merged_data$educaim25 <- map_missing(merged_data$educaim25, "wave8")
merged_data$educaim32 <- map_missing(merged_data$educaim32, "wave9")

# Convert NA to -3
merged_data$educaim17[is.na(merged_data$educaim17)] <- -3
merged_data$educaim19[is.na(merged_data$educaim19)] <- -3
merged_data$educaim20[is.na(merged_data$educaim20)] <- -3
merged_data$educaim25[is.na(merged_data$educaim25)] <- -3
merged_data$educaim32[is.na(merged_data$educaim32)] <- -3

# Select final variables
final_data <- merged_data %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")
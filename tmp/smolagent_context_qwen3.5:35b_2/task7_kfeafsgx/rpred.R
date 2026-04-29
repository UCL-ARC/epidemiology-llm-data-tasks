library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets from data/input/
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
ns8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
ns9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets using full_join by NSID
merged <- full_join(wave_one, wave_four, by = "NSID")
merged <- full_join(merged, wave_six, by = "NSID")
merged <- full_join(merged, wave_seven, by = "NSID")
merged <- full_join(merged, ns8, by = "NSID")
merged <- full_join(merged, ns9, by = "NSID")

# Function to recode age 17 (wave_four) w4saim
recode_age17 <- function(w4saim) {
  case_when(
    w4saim == 1 ~ 1,   # NVQ 3
    w4saim == 2 ~ 1,   # AVCE
    w4saim == 3 ~ 1,   # A/AS
    w4saim == 4 ~ 1,   # Other level 3
    w4saim == 5 ~ 1,   # NVQ 2
    w4saim == 6 ~ 1,   # Intermediate GNVQ
    w4saim == 7 ~ 1,   # Other level 2
    w4saim == 8 ~ 1,   # GCSE
    w4saim == 9 ~ 1,   # NVQ 1
    w4saim == 10 ~ 1,  # Foundation
    w4saim == 11 ~ 1,  # Other level 1
    w4saim == 12 ~ 3,  # Other
    w4saim == 13 ~ 3,  # No detail
    w4saim == 14 ~ 5,  # Not studying
    w4saim %in% c(-999, -998, -997, -996, -995, -994, -993, -992, -991, -990, -99, -98, -97, -96, -95, -94, -93, -92, -91, -90, -89, -88, -87, -86, -85, -84, -83, -82, -81, -80, -79, -78, -77, -76, -75, -74, -73, -72, -71, -70, -69, -68, -67, -66, -65, -64, -63, -62, -61, -60, -59, -58, -57, -56, -55, -54, -53, -52, -51, -50, -49, -48, -47, -46, -45, -44, -43, -42, -41, -40, -39, -38, -37, -36, -35, -34, -33, -32, -31, -30, -29, -28, -27, -26, -25, -24, -23, -22, -21, -20, -19, -18, -17, -16, -15, -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1) ~ -3,
    TRUE ~ NA_real_
  )
}

# Function to recode age 19 (wave_six) W6Saim
recode_age19 <- function(W6Saim) {
  case_when(
    W6Saim == 1 ~ 0,   # NVQ 5
    W6Saim == 2 ~ 0,   # First/Other Degree
    W6Saim == 3 ~ 0,   # NVQ 4
    W6Saim == 4 ~ 0,   # Other HE
    W6Saim == 5 ~ 1,   # NVQ 3
    W6Saim == 6 ~ 1,   # AVCE
    W6Saim == 7 ~ 1,   # A/AS
    W6Saim == 8 ~ 1,   # Other level 3
    W6Saim == 9 ~ 1,   # NVQ 2
    W6Saim == 10 ~ 1,  # Other level 2
    W6Saim == 11 ~ 1,  # GCSE
    W6Saim == 12 ~ 1,  # NVQ 1
    W6Saim == 13 ~ 1,  # Other level 1
    W6Saim == 14 ~ 3,  # Other (level unknown)
    W6Saim == 15 ~ 3,  # No detail
    W6Saim == 16 ~ 5,  # Not studying
    W6Saim %in% c(-999, -998, -997, -996, -995, -994, -993, -992, -991, -990, -99, -98, -97, -96, -95, -94, -93, -92, -91, -90, -89, -88, -87, -86, -85, -84, -83, -82, -81, -80, -79, -78, -77, -76, -75, -74, -73, -72, -71, -70, -69, -68, -67, -66, -65, -64, -63, -62, -61, -60, -59, -58, -57, -56, -55, -54, -53, -52, -51, -50, -49, -48, -47, -46, -45, -44, -43, -42, -41, -40, -39, -38, -37, -36, -35, -34, -33, -32, -31, -30, -29, -28, -27, -26, -25, -24, -23, -22, -21, -20, -19, -18, -17, -16, -15, -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1) ~ -3,
    TRUE ~ NA_real_
  )
}

# Function to recode age 20 (wave_seven) W7SAim
recode_age20 <- function(W7SAim) {
  case_when(
    W7SAim == -94 ~ -8,  # Insufficient information
    W7SAim == -91 ~ -1,  # Not applicable (not studying)
    W7SAim == 1 ~ 1,     # NVQ 1
    W7SAim == 2 ~ 1,     # Other level 1
    W7SAim == 3 ~ 1,     # NVQ 2
    W7SAim == 4 ~ 1,     # GCSE
    W7SAim == 5 ~ 1,     # Other level 2
    W7SAim == 6 ~ 1,     # NVQ 3
    W7SAim == 7 ~ 1,     # A/AS
    W7SAim == 8 ~ 1,     # AVCE
    W7SAim == 9 ~ 1,     # Other level 3
    W7SAim == 10 ~ 0,    # NVQ 4
    W7SAim == 11 ~ 0,    # First/Other Degree
    W7SAim == 12 ~ 0,    # Other HE
    W7SAim == 13 ~ 0,    # NVQ 5
    W7SAim == 14 ~ 3,    # Other (level unknown)
    W7SAim %in% c(-999, -998, -997, -996, -995, -994, -993, -992, -990, -99, -98, -97, -96, -95, -93, -92, -90, -89, -88, -87, -86, -85, -84, -83, -82, -81, -80, -79, -78, -77, -76, -75, -74, -73, -72, -71, -70, -69, -68, -67, -66, -65, -64, -63, -62, -61, -60, -59, -58, -57, -56, -55, -54, -53, -52, -51, -50, -49, -48, -47, -46, -45, -44, -43, -42, -41, -40, -39, -38, -37, -36, -35, -34, -33, -32, -31, -30, -29, -28, -27, -26, -25, -24, -23, -22, -21, -20, -19, -18, -17, -16, -15, -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2) ~ -3,
    TRUE ~ NA_real_
  )
}

# Function to recode age 25 (ns8) - complex logic
recode_age25 <- function(W8ACTIVITY05, W8ACQUC0A, W8ACQUC0B, W8ACQUC0C, W8ACQUC0D, W8ACQUC0E, W8ACQUC0F, W8ACQUC0G, W8ACQUC0H, W8ACQUC0I, W8ACQUC0J, W8ACQUC0K, W8ACQUC0L, W8ACQUC0M, W8ACQUC0N, W8ACQUC0O, W8ACQUC0P, W8ACQUC0Q, W8VCQUC0A, W8VCQUC0B, W8VCQUC0C, W8VCQUC0D, W8VCQUC0E, W8VCQUC0J, W8VCQUC0K) {
  # First check if studying
  studying <- W8ACTIVITY05 == 1
  not_studying <- W8ACTIVITY05 == 0
  na_activity <- W8ACTIVITY05 %in% c(-1, -8, -9)
  
  case_when(
    # Not studying
    not_studying ~ 5,
    # Activity missing
    na_activity ~ case_when(
      W8ACTIVITY05 == -1 ~ -1,
      W8ACTIVITY05 == -8 ~ -8,
      W8ACTIVITY05 == -9 ~ -9
    ),
    # Studying - check qualifications
    TRUE ~ case_when(
      # Higher level (NVQ 4-5, HE)
      W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 | W8ACQUC0D == 1 | W8ACQUC0E == 1 | W8VCQUC0J == 1 | W8VCQUC0K == 1 ~ 0,
      # Mid level (NVQ 1-3, A/AS, GCSE, etc.)
      W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1 | W8ACQUC0L == 1 | W8ACQUC0M == 1 | W8ACQUC0N == 1 | W8VCQUC0A == 1 | W8VCQUC0B == 1 | W8VCQUC0E == 1 ~ 1,
      # Entry level
      W8VCQUC0C == 1 | W8VCQUC0D == 1 ~ 2,
      # Don't know
      W8ACQUC0P == 1 ~ -8,
      # Refused
      W8ACQUC0Q == 1 ~ -9,
      # None of the above
      W8ACQUC0O == 1 ~ 4,
      TRUE ~ NA_real_
    )
  )
}

# Function to recode age 32 (ns9) - complex logic
recode_age32 <- function(W9ECONACT2, W9ACQUC0A, W9ACQUC0B, W9ACQUC0C, W9ACQUC0D, W9ACQUC0E, W9ACQUC0F, W9ACQUC0G, W9ACQUC0H, W9ACQUC0I, W9ACQUC0J, W9ACQUC0K, W9ACQUC0L, W9ACQUC0M, W9ACQUC0N, W9ACQUC0O, W9ACQUC0P, W9ACQUC0Q, W9ACQUC0R, W9ACQUC0S, W9ACQUC0T, W9ACQUC0U, W9ACQUC0V, W9VCQUC0A, W9VCQUC0B, W9VCQUC0C, W9VCQUC0D, W9VCQUC0E, W9VCQUC0F, W9VCQUC0G, W9VCQUC0H, W9VCQUC0I, W9VCQUC0J, W9VCQUC0K, W9VCQUC0L, W9VCQUC0M, W9VCQUC0N, W9VCQUC0O, W9VCQUC0P, W9VCQUC0Q, W9VCQUC0R, W9VCQUC0S, W9VCQUC0T, W9VCQUC0U, W9VCQUC0V, W9VCQUC0W, W9VCQUC0X, W9VCQUC0Y, W9VCQUC0Z, W9VCQUCAA, W9VCQUCAB, W9VCQUCAC, W9VCQUCAD, W9VCQUCAE, W9VCQUCAF, W9VCQUCAG, W9VCQUCAH, W9VCQUCAI) {
  # First check economic activity
  studying <- W9ECONACT2 %in% c(6, 7, 12)  # Full-time education, Part-time education, Apprenticeship
  not_studying <- W9ECONACT2 %in% c(1, 2, 3, 4, 5, 8, 9, 10, 11, 13, 14)
  na_econ <- W9ECONACT2 %in% c(-1, -3, -8, -9)
  
  case_when(
    # Not studying
    not_studying ~ 5,
    # Activity missing
    na_econ ~ case_when(
      W9ECONACT2 == -1 ~ -1,
      W9ECONACT2 == -3 ~ -3,
      W9ECONACT2 == -8 ~ -8,
      W9ECONACT2 == -9 ~ -9
    ),
    # Studying - check qualifications
    TRUE ~ case_when(
      # Higher level (NVQ 4-5, HE)
      W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 | W9ACQUC0D == 1 | W9ACQUC0E == 1 | W9ACQUC0F == 1 | W9VCQUC0A == 1 | W9VCQUC0B == 1 | W9VCQUC0C == 1 | W9VCQUC0R == 1 | W9VCQUC0S == 1 | W9VCQUC0T == 1 | W9VCQUC0U == 1 | W9VCQUC0V == 1 | W9VCQUCAC == 1 ~ 0,
      # Mid level (NVQ 1-3, A/AS, GCSE, etc.)
      W9ACQUC0G == 1 | W9ACQUC0H == 1 | W9ACQUC0I == 1 | W9ACQUC0J == 1 | W9ACQUC0K == 1 | W9ACQUC0L == 1 | W9ACQUC0M == 1 | W9ACQUC0N == 1 | W9ACQUC0O == 1 | W9ACQUC0P == 1 | W9ACQUC0Q == 1 | W9VCQUC0D == 1 | W9VCQUC0E == 1 | W9VCQUC0F == 1 | W9VCQUC0G == 1 | W9VCQUC0H == 1 | W9VCQUC0I == 1 | W9VCQUC0J == 1 | W9VCQUC0K == 1 | W9VCQUC0L == 1 | W9VCQUC0M == 1 | W9VCQUC0N == 1 | W9VCQUC0O == 1 | W9VCQUC0P == 1 | W9VCQUC0Q == 1 | W9VCQUC0W == 1 | W9VCQUC0X == 1 | W9VCQUC0Y == 1 | W9VCQUC0Z == 1 | W9VCQUCAA == 1 | W9VCQUCAB == 1 | W9VCQUCAD == 1 | W9VCQUCAE == 1 ~ 1,
      # Other (level unknown)
      W9ACQUC0R == 1 | W9VCQUCAF == 1 ~ 3,
      # Don't know
      W9ACQUC0T == 1 | W9ACQUC0V == 1 | W9VCQUCAH == 1 ~ -8,
      # Refused
      W9ACQUC0U == 1 | W9VCQUCAI == 1 ~ -9,
      # None of the above
      W9ACQUC0S == 1 | W9VCQUCAG == 1 ~ 4,
      TRUE ~ NA_real_
    )
  )
}

# Apply recoding functions
merged <- merged %>%
  mutate(
    educaim17 = recode_age17(w4saim),
    educaim19 = recode_age19(W6Saim),
    educaim20 = recode_age20(W7SAim),
    educaim25 = recode_age25(
      W8ACTIVITY05, W8ACQUC0A, W8ACQUC0B, W8ACQUC0C, W8ACQUC0D, W8ACQUC0E, W8ACQUC0F, W8ACQUC0G, W8ACQUC0H, W8ACQUC0I, W8ACQUC0J, W8ACQUC0K, W8ACQUC0L, W8ACQUC0M, W8ACQUC0N, W8ACQUC0O, W8ACQUC0P, W8ACQUC0Q, W8VCQUC0A, W8VCQUC0B, W8VCQUC0C, W8VCQUC0D, W8VCQUC0E, W8VCQUC0J, W8VCQUC0K
    ),
    educaim32 = recode_age32(
      W9ECONACT2, W9ACQUC0A, W9ACQUC0B, W9ACQUC0C, W9ACQUC0D, W9ACQUC0E, W9ACQUC0F, W9ACQUC0G, W9ACQUC0H, W9ACQUC0I, W9ACQUC0J, W9ACQUC0K, W9ACQUC0L, W9ACQUC0M, W9ACQUC0N, W9ACQUC0O, W9ACQUC0P, W9ACQUC0Q, W9ACQUC0R, W9ACQUC0S, W9ACQUC0T, W9ACQUC0U, W9ACQUC0V, W9VCQUC0A, W9VCQUC0B, W9VCQUC0C, W9VCQUC0D, W9VCQUC0E, W9VCQUC0F, W9VCQUC0G, W9VCQUC0H, W9VCQUC0I, W9VCQUC0J, W9VCQUC0K, W9VCQUC0L, W9VCQUC0M, W9VCQUC0N, W9VCQUC0O, W9VCQUC0P, W9VCQUC0Q, W9VCQUC0R, W9VCQUC0S, W9VCQUC0T, W9VCQUC0U, W9VCQUC0V, W9VCQUC0W, W9VCQUC0X, W9VCQUC0Y, W9VCQUC0Z, W9VCQUCAA, W9VCQUCAB, W9VCQUCAC, W9VCQUCAD, W9VCQUCAE, W9VCQUCAF, W9VCQUCAG, W9VCQUCAH, W9VCQUCAI
    )
  )

# Select only NSID and harmonized variables
output <- merged %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")

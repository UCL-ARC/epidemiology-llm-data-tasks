library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load all datasets
s1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
s4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
s6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
s7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
s8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
s9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Select needed variables from each wave
s4_sel <- s4 %>% select(NSID, w4saim)
s6_sel <- s6 %>% select(NSID, W6Saim)
s7_sel <- s7 %>% select(NSID, W7SAim)

# For age 25 (s8): activity + all qualification indicators
s8_sel <- s8 %>% select(NSID, W8ACTIVITY05,
  W8ACQUC0A, W8ACQUC0B, W8ACQUC0C, W8ACQUC0D, W8ACQUC0E,
  W8ACQUC0F, W8ACQUC0G, W8ACQUC0H, W8ACQUC0I, W8ACQUC0J,
  W8ACQUC0K, W8ACQUC0L, W8ACQUC0M, W8ACQUC0N, W8ACQUC0O,
  W8ACQUC0P, W8ACQUC0Q,
  W8VCQUC0A, W8VCQUC0B, W8VCQUC0C, W8VCQUC0D, W8VCQUC0E,
  W8VCQUC0J, W8VCQUC0K)

# For age 32 (s9): activity + all qualification indicators
s9_sel <- s9 %>% select(NSID, W9ECONACT2,
  W9ACQUC0A, W9ACQUC0B, W9ACQUC0C, W9ACQUC0D, W9ACQUC0E,
  W9ACQUC0F, W9ACQUC0G, W9ACQUC0H, W9ACQUC0I, W9ACQUC0J,
  W9ACQUC0K, W9ACQUC0L, W9ACQUC0M, W9ACQUC0N, W9ACQUC0O,
  W9ACQUC0P, W9ACQUC0Q, W9ACQUC0R, W9ACQUC0S, W9ACQUC0T,
  W9ACQUC0U, W9ACQUC0V,
  W9VCQUC0A, W9VCQUC0B, W9VCQUC0C, W9VCQUC0D, W9VCQUC0E,
  W9VCQUC0F, W9VCQUC0G, W9VCQUC0H, W9VCQUC0I, W9VCQUC0J,
  W9VCQUC0K, W9VCQUC0L, W9VCQUC0M, W9VCQUC0N, W9VCQUC0O,
  W9VCQUC0P, W9VCQUC0Q, W9VCQUC0R, W9VCQUC0S, W9VCQUC0T,
  W9VCQUC0U, W9VCQUC0V, W9VCQUC0W, W9VCQUC0X, W9VCQUC0Y,
  W9VCQUC0Z, W9VCQUCAA, W9VCQUCAB, W9VCQUCAC, W9VCQUCAD,
  W9VCQUCAE, W9VCQUCAF, W9VCQUCAG, W9VCQUCAH, W9VCQUCAI)

# Merge all datasets using full_join by NSID
merged <- s1 %>%
  full_join(s4_sel, by = "NSID") %>%
  full_join(s6_sel, by = "NSID") %>%
  full_join(s7_sel, by = "NSID") %>%
  full_join(s8_sel, by = "NSID") %>%
  full_join(s9_sel, by = "NSID")

# ===== AGE 17 (w4saim) =====
# 1-11: NVQ 1-3 (category 1)
# 12: Other (category 3)
# 13: No detail (category 2 - entry level)
# 14: Not studying (category 5)

merged <- merged %>%
  mutate(educaim17 = case_when(
    w4saim >= 1 & w4saim <= 11 ~ 1,  # NVQ 1-3
    w4saim == 12 ~ 3,                  # Other
    w4saim == 13 ~ 2,                  # No detail / entry level
    w4saim == 14 ~ 5,                  # Not studying
    w4saim == -9 ~ -9,                 # Refused
    w4saim == -8 ~ -8,                 # Don't know
    w4saim == -1 ~ -1,                 # Not applicable
    w4saim == -2 ~ -2,                 # Schedule not applicable
    w4saim == -3 ~ -3,                 # Not asked
    is.na(w4saim) ~ -3,                # NA -> -3
    TRUE ~ -3                          # Default
  ))

# ===== AGE 19 (W6Saim) =====
# 1-4: NVQ 4-5, HE (category 0)
# 5-13: NVQ 1-3 (category 1)
# 14: Other level unknown (category 3)
# 15: No detail (category 2)
# 16: Not studying (category 5)

merged <- merged %>%
  mutate(educaim19 = case_when(
    W6Saim >= 1 & W6Saim <= 4 ~ 0,     # NVQ 4-5, HE
    W6Saim >= 5 & W6Saim <= 13 ~ 1,    # NVQ 1-3
    W6Saim == 14 ~ 3,                   # Other
    W6Saim == 15 ~ 2,                   # No detail / entry level
    W6Saim == 16 ~ 5,                   # Not studying
    W6Saim == -9 ~ -9,                  # Refused
    W6Saim == -8 ~ -8,                  # Don't know
    W6Saim == -1 ~ -1,                  # Not applicable
    W6Saim == -2 ~ -2,                  # Schedule not applicable
    W6Saim == -3 ~ -3,                  # Not asked
    is.na(W6Saim) ~ -3,                 # NA -> -3
    TRUE ~ -3                           # Default
  ))

# ===== AGE 20 (W7SAim) =====
# 1-9: NVQ 1-3 (category 1)
# 10-13: NVQ 4-5, HE (category 0)
# 14: Other level unknown (category 3)
# -94: Insufficient information -> -8
# -91: Not applicable -> -1

merged <- merged %>%
  mutate(educaim20 = case_when(
    W7SAim >= 1 & W7SAim <= 9 ~ 1,     # NVQ 1-3
    W7SAim >= 10 & W7SAim <= 13 ~ 0,   # NVQ 4-5, HE
    W7SAim == 14 ~ 3,                   # Other
    W7SAim == -94 ~ -8,                 # Insufficient information -> -8
    W7SAim == -91 ~ -1,                 # Not applicable -> -1
    W7SAim == -9 ~ -9,                  # Refused
    W7SAim == -8 ~ -8,                  # Don't know
    W7SAim == -1 ~ -1,                  # Not applicable
    W7SAim == -2 ~ -2,                  # Schedule not applicable
    W7SAim == -3 ~ -3,                  # Not asked
    is.na(W7SAim) ~ -3,                 # NA -> -3
    TRUE ~ -3                           # Default
  ))

# ===== AGE 25 (W8) =====
# First determine if studying using W8ACTIVITY05
# Then classify using qualification indicators

merged <- merged %>%
  mutate(educaim25 = case_when(
    # First check if not studying
    W8ACTIVITY05 == 0 ~ 5,
    # Check for missing codes in activity
    W8ACTIVITY05 == -9 ~ -9,
    W8ACTIVITY05 == -8 ~ -8,
    W8ACTIVITY05 == -1 ~ -1,
    W8ACTIVITY05 == -3 ~ -3,
    is.na(W8ACTIVITY05) ~ -3,
    # If studying (W8ACTIVITY05 == 1), classify by qualifications
    # NVQ 4-5 (category 0)
    W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 | 
    W8ACQUC0D == 1 | W8ACQUC0E == 1 | W8VCQUC0K == 1 ~ 0,
    # NVQ 1-3 (category 1)
    W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 |
    W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1 |
    W8ACQUC0L == 1 | W8ACQUC0M == 1 | W8ACQUC0N == 1 |
    W8VCQUC0A == 1 | W8VCQUC0B == 1 | W8VCQUC0E == 1 |
    W8VCQUC0J == 1 ~ 1,
    # Entry level (category 2)
    W8VCQUC0C == 1 | W8VCQUC0D == 1 ~ 2,
    # None (category 4)
    W8ACQUC0O == 1 ~ 4,
    # Check for don't know / refused in qualifications
    W8ACQUC0P == 1 ~ -8,
    W8ACQUC0Q == 1 ~ -9,
    # Default for studying but no clear qualification
    W8ACTIVITY05 == 1 ~ 3,
    TRUE ~ -3
  ))

# ===== AGE 32 (W9) =====
# First determine if studying using W9ECONACT2 (6 = full-time education, 7 = part-time education)
# Then classify using qualification indicators

merged <- merged %>%
  mutate(educaim32 = case_when(
    # Check for missing codes in activity first
    W9ECONACT2 == -9 ~ -9,
    W9ECONACT2 == -8 ~ -8,
    W9ECONACT2 == -3 ~ -3,
    W9ECONACT2 == -1 ~ -1,
    is.na(W9ECONACT2) ~ -3,
    # Not studying (category 5)
    !(W9ECONACT2 %in% c(6, 7, 8, 12)) ~ 5,
    # If studying, classify by qualifications
    # NVQ 4-5 (category 0)
    W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 |
    W9ACQUC0D == 1 | W9ACQUC0E == 1 |
    W9VCQUC0A == 1 | W9VCQUC0C == 1 |
    W9VCQUC0R == 1 | W9VCQUC0S == 1 |
    W9VCQUC0V == 1 | W9VCQUCAC == 1 ~ 0,
    # NVQ 1-3 (category 1)
    W9ACQUC0F == 1 | W9ACQUC0G == 1 | W9ACQUC0H == 1 |
    W9ACQUC0I == 1 | W9ACQUC0J == 1 | W9ACQUC0K == 1 |
    W9ACQUC0L == 1 | W9ACQUC0M == 1 | W9ACQUC0N == 1 |
    W9ACQUC0O == 1 | W9ACQUC0P == 1 | W9ACQUC0Q == 1 |
    W9VCQUC0B == 1 | W9VCQUC0D == 1 | W9VCQUC0E == 1 |
    W9VCQUC0F == 1 | W9VCQUC0G == 1 | W9VCQUC0H == 1 |
    W9VCQUC0I == 1 | W9VCQUC0J == 1 | W9VCQUC0K == 1 |
    W9VCQUC0L == 1 | W9VCQUC0M == 1 | W9VCQUC0N == 1 |
    W9VCQUC0O == 1 | W9VCQUC0P == 1 | W9VCQUC0Q == 1 |
    W9VCQUC0W == 1 | W9VCQUC0X == 1 | W9VCQUC0Y == 1 |
    W9VCQUC0Z == 1 | W9VCQUCAA == 1 | W9VCQUCAB == 1 |
    W9VCQUCAD == 1 | W9VCQUCAE == 1 ~ 1,
    # Entry level (category 2) - basic skills etc
    W9VCQUC0K == 1 ~ 2,
    # None (category 4)
    W9ACQUC0S == 1 | W9VCQUCAG == 1 ~ 4,
    # Other / don't know / refused
    W9ACQUC0T == 1 | W9VCQUCAH == 1 ~ -8,
    W9ACQUC0U == 1 | W9VCQUCAI == 1 ~ -9,
    # Default for studying but no clear qualification
    W9ECONACT2 %in% c(6, 7, 8, 12) ~ 3,
    TRUE ~ -3
  ))

# Select final output variables
output <- merged %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write output
write_csv(output, "data/output/cleaned_data.csv")

# Print summary
cat("Data cleaning complete.\n")
cat("Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(output), "\n")
cat("Number of columns:", ncol(output), "\n")
print(summary(output))

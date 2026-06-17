# Load required libraries
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(haven)
library(labelled)

# Define input files
wave1_file <- "data/input/wave_one_lsype_young_person_2020.tab"
wave4_file <- "data/input/wave_four_lsype_young_person_2020.tab"
wave6_file <- "data/input/wave_six_lsype_young_person_2020.tab"
wave7_file <- "data/input/wave_seven_lsype_young_person_2020.tab"
wave8_file <- "data/input/ns8_2015_main_interview.tab"
wave9_file <- "data/input/ns9_2022_main_interview.tab"

# Load all files
w1 <- read_delim(wave1_file, delim = "\t", show_col_types = FALSE)
w4 <- read_delim(wave4_file, delim = "\t", show_col_types = FALSE)
w6 <- read_delim(wave6_file, delim = "\t", show_col_types = FALSE)
w7 <- read_delim(wave7_file, delim = "\t", show_col_types = FALSE)
w8 <- read_delim(wave8_file, delim = "\t", show_col_types = FALSE)
w9 <- read_delim(wave9_file, delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID
df <- full_join(w1, w4, by = "NSID")
df <- full_join(df, w6, by = "NSID")
df <- full_join(df, w7, by = "NSID")
df <- full_join(df, w8, by = "NSID")
df <- full_join(df, w9, by = "NSID")

# --- Age 17: w4saim ---
# 1: NVQ 3 -> 1, 2: AVCE -> 1, 3: A/AS -> 1, 4: Other level 3 -> 1
# 5: NVQ 2 -> 1, 6: Intermediate GNVQ -> 1, 7: Other level 2 -> 1
# 8: GCSE -> 2, 9: NVQ 1 -> 1, 10: Foundation -> 2
# 11: Other level 1 -> 1, 12: Other -> 3, 13: No detail -> 3
# 14: Not studying -> 5
df <- df %>%
  mutate(
    educaim17 = case_when(
      w4saim == 1 ~ 1,
      w4saim == 2 ~ 1,
      w4saim == 3 ~ 1,
      w4saim == 4 ~ 1,
      w4saim == 5 ~ 1,
      w4saim == 6 ~ 1,
      w4saim == 7 ~ 1,
      w4saim == 8 ~ 2,
      w4saim == 9 ~ 1,
      w4saim == 10 ~ 2,
      w4saim == 11 ~ 1,
      w4saim == 12 ~ 3,
      w4saim == 13 ~ 3,
      w4saim == 14 ~ 5,
      w4saim < 0 ~ -3,
      is.na(w4saim) ~ -3,
      TRUE ~ -3
    )
  )

# --- Age 19: W6Saim ---
# 1: NVQ 5 -> 0, 2: First/Other Degree -> 0, 3: NVQ 4 -> 0, 4: Other HE -> 0
# 5: NVQ 3 -> 1, 6: AVCE -> 1, 7: A/AS -> 1, 8: Other level 3 -> 1
# 9: NVQ 2 -> 1, 10: Other level 2 -> 1, 11: GCSE -> 2
# 12: NVQ 1 -> 1, 13: Other level 1 -> 1, 14: Other (level unknown) -> 3
# 15: No detail -> 3, 16: Not studying -> 5
df <- df %>%
  mutate(
    educaim19 = case_when(
      W6Saim == 1 ~ 0,
      W6Saim == 2 ~ 0,
      W6Saim == 3 ~ 0,
      W6Saim == 4 ~ 0,
      W6Saim == 5 ~ 1,
      W6Saim == 6 ~ 1,
      W6Saim == 7 ~ 1,
      W6Saim == 8 ~ 1,
      W6Saim == 9 ~ 1,
      W6Saim == 10 ~ 1,
      W6Saim == 11 ~ 2,
      W6Saim == 12 ~ 1,
      W6Saim == 13 ~ 1,
      W6Saim == 14 ~ 3,
      W6Saim == 15 ~ 3,
      W6Saim == 16 ~ 5,
      W6Saim < 0 ~ -3,
      is.na(W6Saim) ~ -3,
      TRUE ~ -3
    )
  )

# --- Age 20: W7SAim ---
# -94: Insufficient information -> -8
# -91: Not applicable (not studying) -> 5
# 1: NVQ 1 -> 1, 2: Other level 1 -> 1, 3: NVQ 2 -> 1, 4: GCSE -> 2
# 5: Other level 2 -> 1, 6: NVQ 3 -> 1, 7: A/AS -> 1, 8: AVCE -> 1
# 9: Other level 3 -> 1, 10: NVQ 4 -> 0, 11: First/Other Degree -> 0
# 12: Other HE -> 0, 13: NVQ 5 -> 0, 14: Other (level unknown) -> 3
df <- df %>%
  mutate(
    educaim20 = case_when(
      W7SAim == 1 ~ 1,
      W7SAim == 2 ~ 1,
      W7SAim == 3 ~ 1,
      W7SAim == 4 ~ 2,
      W7SAim == 5 ~ 1,
      W7SAim == 6 ~ 1,
      W7SAim == 7 ~ 1,
      W7SAim == 8 ~ 1,
      W7SAim == 9 ~ 1,
      W7SAim == 10 ~ 0,
      W7SAim == 11 ~ 0,
      W7SAim == 12 ~ 0,
      W7SAim == 13 ~ 0,
      W7SAim == 14 ~ 3,
      W7SAim == -94 ~ -8,
      W7SAim == -91 ~ 5,
      is.na(W7SAim) ~ -3,
      TRUE ~ -3
    )
  )

# --- Age 25: Wave 8 ---
# W8ACTIVITY05: 0=No, 1=Yes, -9=Refused, -8=Don't know, -1=Not applicable
df <- df %>%
  mutate(
    educaim25 = case_when(
      W8ACTIVITY05 == 0 ~ 5,
      W8ACTIVITY05 == -9 ~ -9,
      W8ACTIVITY05 == -8 ~ -8,
      W8ACTIVITY05 == -1 ~ -3,
      is.na(W8ACTIVITY05) ~ -3,
      TRUE ~ NA_real_
    )
  )

# For those with W8ACTIVITY05 == 1, check qualification tick-boxes in priority order
# Use logical comparisons so pmax returns TRUE if ANY column has 1
df <- df %>%
  mutate(
    # NVQ 4-5 tick-boxes: check if ANY is Yes
    nvq45_w8 = pmax(
      W8ACQUC0A == 1, W8ACQUC0B == 1, W8ACQUC0C == 1, W8ACQUC0D == 1, W8ACQUC0E == 1,
      W8VCQUC0J == 1, W8VCQUC0K == 1,
      na.rm = TRUE
    ),
    # NVQ 1-3 tick-boxes
    nvq13_w8 = pmax(
      W8ACQUC0F == 1, W8ACQUC0G == 1, W8ACQUC0H == 1, W8ACQUC0I == 1, W8ACQUC0J == 1,
      W8ACQUC0K == 1, W8ACQUC0N == 1,
      W8VCQUC0A == 1, W8VCQUC0B == 1, W8VCQUC0C == 1, W8VCQUC0E == 1,
      na.rm = TRUE
    ),
    # Entry level tick-boxes
    entry_w8 = pmax(
      W8ACQUC0L == 1, W8ACQUC0M == 1, W8VCQUC0D == 1,
      na.rm = TRUE
    ),
    # Don't know
    dk_w8 = pmax(
      W8ACQUC0P == 1,
      na.rm = TRUE
    ),
    # Refused
    ref_w8 = pmax(
      W8ACQUC0Q == 1,
      na.rm = TRUE
    )
  )

df <- df %>%
  mutate(
    educaim25 = case_when(
      W8ACTIVITY05 == 0 ~ 5,
      W8ACTIVITY05 == -9 ~ -9,
      W8ACTIVITY05 == -8 ~ -8,
      W8ACTIVITY05 == -1 ~ -3,
      is.na(W8ACTIVITY05) ~ -3,
      nvq45_w8 == TRUE ~ 0,
      nvq13_w8 == TRUE ~ 1,
      entry_w8 == TRUE ~ 2,
      dk_w8 == TRUE ~ -8,
      ref_w8 == TRUE ~ -9,
      TRUE ~ -3
    )
  )

# Clean up intermediate variables
df <- df %>% select(-nvq45_w8, -nvq13_w8, -entry_w8, -dk_w8, -ref_w8)

# --- Age 32: Wave 9 ---
# W9ECONACT2 categories:
# 6: Full-time education, 7: Part-time education, 8: Gov scheme training, 12: Apprenticeship -> studying
# Others: not studying
df <- df %>%
  mutate(
    educaim32 = case_when(
      # Studying: derive from qualifications
      W9ECONACT2 %in% c(6, 7, 8, 12) ~ NA_real_,
      # Not studying
      W9ECONACT2 %in% c(1, 2, 3, 4, 5, 9, 10, 11, 13, 14) ~ 5,
      # Missing codes
      W9ECONACT2 == -9 ~ -9,
      W9ECONACT2 == -8 ~ -8,
      W9ECONACT2 == -3 ~ -3,
      W9ECONACT2 == -1 ~ -3,
      is.na(W9ECONACT2) ~ -3,
      TRUE ~ 5
    )
  )

# For those in education/apprenticeship, check qualification tick-boxes in priority order
df <- df %>%
  mutate(
    # NVQ 4-5 tick-boxes
    nvq45_w9 = pmax(
      W9ACQUC0A == 1, W9ACQUC0B == 1, W9ACQUC0C == 1, W9ACQUC0D == 1, W9ACQUC0E == 1,
      W9ACQUC0H == 1, W9ACQUC0J == 1, W9ACQUC0K == 1,
      W9VCQUC0A == 1, W9VCQUC0C == 1, W9VCQUC0S == 1, W9VCQUCAC == 1, W9VCQUCAD == 1,
      na.rm = TRUE
    ),
    # NVQ 1-3 tick-boxes
    nvq13_w9 = pmax(
      W9ACQUC0F == 1, W9ACQUC0G == 1, W9ACQUC0I == 1,
      W9VCQUC0B == 1, W9VCQUC0D == 1, W9VCQUC0E == 1, W9VCQUC0F == 1,
      W9VCQUC0I == 1, W9VCQUC0J == 1, W9VCQUC0O == 1, W9VCQUC0P == 1, W9VCQUC0Q == 1,
      na.rm = TRUE
    ),
    # Entry level tick-boxes
    entry_w9 = pmax(
      W9ACQUC0L == 1, W9ACQUC0M == 1, W9ACQUC0N == 1, W9ACQUC0O == 1,
      W9ACQUC0P == 1, W9ACQUC0Q == 1,
      W9VCQUC0K == 1, W9VCQUCAE == 1,
      na.rm = TRUE
    ),
    # Other tick-boxes
    other_w9 = pmax(
      W9ACQUC0R == 1,
      W9VCQUC0G == 1, W9VCQUC0H == 1, W9VCQUC0L == 1, W9VCQUC0M == 1, W9VCQUC0N == 1,
      W9VCQUC0R == 1, W9VCQUC0T == 1, W9VCQUC0U == 1, W9VCQUC0V == 1, W9VCQUC0W == 1,
      W9VCQUC0X == 1, W9VCQUC0Y == 1, W9VCQUC0Z == 1, W9VCQUCAA == 1, W9VCQUCAB == 1,
      W9VCQUCAF == 1,
      na.rm = TRUE
    ),
    # None of these
    none_w9 = pmax(
      W9ACQUC0S == 1, W9VCQUCAG == 1,
      na.rm = TRUE
    ),
    # Don't know
    dk_w9 = pmax(
      W9ACQUC0T == 1, W9VCQUCAH == 1,
      na.rm = TRUE
    ),
    # Refused
    ref_w9 = pmax(
      W9ACQUC0U == 1, W9VCQUCAI == 1,
      na.rm = TRUE
    )
  )

df <- df %>%
  mutate(
    educaim32 = case_when(
      # Studying: derive from qualifications
      W9ECONACT2 %in% c(6, 7, 8, 12) ~ case_when(
        nvq45_w9 == TRUE ~ 0,
        nvq13_w9 == TRUE ~ 1,
        entry_w9 == TRUE ~ 2,
        other_w9 == TRUE ~ 3,
        none_w9 == TRUE ~ 4,
        dk_w9 == TRUE ~ -8,
        ref_w9 == TRUE ~ -9,
        TRUE ~ -3
      ),
      # Not studying
      W9ECONACT2 %in% c(1, 2, 3, 4, 5, 9, 10, 11, 13, 14) ~ 5,
      # Missing codes
      W9ECONACT2 == -9 ~ -9,
      W9ECONACT2 == -8 ~ -8,
      W9ECONACT2 == -3 ~ -3,
      W9ECONACT2 == -1 ~ -3,
      is.na(W9ECONACT2) ~ -3,
      TRUE ~ 5
    )
  )

# Clean up intermediate variables
df <- df %>% select(-nvq45_w9, -nvq13_w9, -entry_w9, -other_w9, -none_w9, -dk_w9, -ref_w9)

# Select only NSID and derived variables
df_out <- df %>% select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write output
dir.create("data/output", showWarnings = FALSE)
write_csv(df_out, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(df_out), "\n")
cat("Variables:", paste(names(df_out), collapse = ", "), "\n")

# Print summary of each variable
cat("\nSummary of educaim17:\n")
print(table(df_out$educaim17, useNA = "ifany"))
cat("\nSummary of educaim19:\n")
print(table(df_out$educaim19, useNA = "ifany"))
cat("\nSummary of educaim20:\n")
print(table(df_out$educaim20, useNA = "ifany"))
cat("\nSummary of educaim25:\n")
print(table(df_out$educaim25, useNA = "ifany"))
cat("\nSummary of educaim32:\n")
print(table(df_out$educaim32, useNA = "ifany"))

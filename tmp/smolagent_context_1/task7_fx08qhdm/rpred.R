library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
path <- "data/input/"

files <- list(
  wave1 = "wave_one_lsype_young_person_2020.tab",
  wave4 = "wave_four_lsype_young_person_2020.tab",
  wave6 = "wave_six_lsype_young_person_2020.tab",
  wave7 = "wave_seven_lsype_young_person_2020.tab",
  wave8 = "ns8_2015_main_interview.tab",
  wave9 = "ns9_2022_main_interview.tab"
)

load_file <- function(name) {
  read_delim(paste0(path, name), delim = "\t", col_types = cols(.default = "numeric"), guess = 0)
}

# Need to handle NSID as string for joining
load_file_id_string <- function(name) {
  read_delim(paste0(path, name), delim = "\t", col_types = cols(NSID = col_character(), .default = "numeric"), guess = 0)
}

df1 <- load_file_id_string(files$wave1)
df4 <- load_file_id_string(files$wave4)
df6 <- load_file_id_string(files$wave6)
df7 <- load_file_id_string(files$wave7)
df8 <- load_file_id_string(files$wave8)
df9 <- load_file_id_string(files$wave9)

# Merge datasets
cohort <- df1 %>%
  full_join(df4, by = "NSID") %>%
  full_join(df6, by = "NSID") %>%
  full_join(df7, by = "NSID") %>%
  full_join(df8, by = "NSID") %>%
  full_join(df9, by = "NSID")

# 2. Target Variables & Harmonisation
# Scheme:
# 0 = NVQ 4–5 equivalent (higher / HE-level qualifications)
# 1 = NVQ 1–3 equivalent (lower / mid-level qualifications)
# 2 = None / entry level
# 3 = Other (level unknown or unclassifiable)
# 4 = None of these qualifications
# 5 = Not currently studying

# Wave 4 (Age 17)
cohort <- cohort %>%
  mutate(edu17 = case_when(
    w4saim == 14 ~ 5,
    w4saim %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) ~ 1,
    w4saim == 12 ~ 3,
    w4saim == 13 ~ 3,
    w4saim >= -1 & w4saim < 0 ~ -3, # Simplified missing
    TRUE ~ -3
  ))

# Wave 6 (Age 19)
cohort <- cohort %>%
  mutate(edu19 = case_when(
    W6Saim == 16 ~ 5,
    W6Saim %in% c(1, 2, 3, 4) ~ 0,
    W6Saim %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13) ~ 1,
    W6Saim %in% c(14, 15) ~ 3,
    W6Saim >= -1 & W6Saim < 0 ~ -3,
    TRUE ~ -3
  ))

# Wave 7 (Age 20)
cohort <- cohort %>%
  mutate(edu20 = case_when(
    W7SAim == -91 ~ 5,
    W7SAim %in% c(10, 11, 12, 13) ~ 0,
    W7SAim %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ 1,
    W7SAim == 14 ~ 3,
    W7SAim == -94 ~ -8, # Insufficient info
    W7SAim >= -1 & W7SAim < 0 ~ -3,
    TRUE ~ -3
  ))

# Wave 8 (Age 25)
# Adult logic
cohort <- cohort %>%
  mutate(edu25 = case_when(
    W8ACTIVITY05 == 0 ~ 5,
    (W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 | W8ACQUC0D == 1 | W8ACQUC0E == 1 | W8VCQUC0K == 1 | W8VCQUC0J == 1) ~ 0,
    (W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1 | W8ACQUC0L == 1 | W8VCQUC0E == 1) ~ 1,
    (W8ACQUC0M == 1 | W8ACQUC0N == 1 | W8VCQUC0A == 1 | W8VCQUC0B == 1 | W8VCQUC0C == 1 | W8VCQUC0D == 1) ~ 2,
    W8ACQUC0O == 1 ~ 4,
    # Refusal/DK
    W8ACTIVITY05 == -9 | W8ACQUC0P == 1 | W8ACQUC0Q == 1 ~ -9,
    W8ACTIVITY05 == -8 ~ -8,
    TRUE ~ -3
  ))

# Wave 9 (Age 32)
# Adult logic
cohort <- cohort %>%
  mutate(edu32 = case_when(
    # Not studying: 1-5, 8-14
    W9ECONACT2 %in% c(1, 2, 3, 4, 5, 8, 9, 10, 11, 13, 14) ~ 5,
    # HE level
    (W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 | W9ACQUC0D == 1 | W9ACQUC0E == 1 | W9VCQUC0A == 1 | W9VCQUC0C == 1 | W9VCQUC0R == 1 | W9VCQUC0S == 1 | W9VCQUC0V == 1 | W9VCQUC0W == 1 | W9VCQUCAC == 1) ~ 0,
    # Mid/Low level
    (W9ACQUC0F == 1 | W9ACQUC0G == 1 | W9ACQUC0H == 1 | W9ACQUC0J == 1 | W9ACQUC0K == 1 | W9VCQUC0B == 1 | W9VCQUC0D == 1 | W9VCQUC0G == 1 | W9VCQUC0I == 1 | W9VCQUC0O == 1) ~ 1,
    # Entry level
    (W9ACQUC0I == 1 | W9ACQUC0L == 1 | W9ACQUC0M == 1 | W9ACQUC0N == 1 | W9ACQUC0O == 1 | W9ACQUC0P == 1 | W9ACQUC0Q == 1 | W9VCQUC0E == 1 | W9VCQUC0F == 1 | W9VCQUC0H == 1 | W9VCQUC0J == 1 | W9VCQUC0K == 1 | W9VCQUC0L == 1 | W9VCQUC0M == 1 | W9VCQUC0N == 1 | W9VCQUC0P == 1 | W9VCQUC0Q == 1 | W9VCQUCAE == 1) ~ 2,
    W9ACQUC0R == 1 | W9VCQUCAF == 1 ~ 3,
    W9ACQUC0S == 1 | W9VCQUCAG == 1 ~ 4,
    # Missing
    W9ECONACT2 == -9 | W9ACQUC0U == 1 | W9VCQUCAI == 1 ~ -9,
    W9ECONACT2 == -8 | W9ACQUC0T == 1 | W9VCQUCAH == 1 ~ -8,
    TRUE ~ -3
  ))

# Final labels
edu_labels <- c("0" = "NVQ 4–5 equivalent", "1" = "NVQ 1–3 equivalent", "2" = "None / entry level", "3" = "Other", "4" = "None of these", "5" = "Not currently studying", "-9" = "Refusal", "-8" = "Don't know", "-7" = "Prefer not to say", "-3" = "Not asked", "-2" = "Not applicable/Error", "-1" = "Not applicable")

final_df <- cohort %>%
  select(NSID, edu17, edu19, edu20, edu25, edu32) %>%
  mutate(across(starts_with("edu"), ~as.factor(.)))

write_csv(final_df, "data/output/cleaned_data.csv")

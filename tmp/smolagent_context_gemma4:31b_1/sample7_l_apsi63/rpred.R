library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define input paths
path_prefix <- "data/input/"

# 1. File Loading
# Load all datasets listed in metadata
S1 <- read_delim(paste0(path_prefix, "wave_one_lsype_young_person_2020.tab"), delim = "\t") %>% select(NSID)
S4 <- read_delim(paste0(path_prefix, "wave_four_lsype_young_person_2020.tab"), delim = "\t") %>% select(NSID, w4saim)
S6 <- read_delim(paste0(path_prefix, "wave_six_lsype_young_person_2020.tab"), delim = "\t") %>% select(NSID, W6Saim)
S7 <- read_delim(paste0(path_prefix, "wave_seven_lsype_young_person_2020.tab"), delim = "\t") %>% select(NSID, W7SAim)

# Wave 8 (Age 25)
S8_full <- read_delim(paste0(path_prefix, "ns8_2015_main_interview.tab"), delim = "\t")
S8 <- S8_full %>% select(NSID, W8ACTIVITY05, starts_with("W8ACQUC0"), starts_with("W8VCQUC0"))

# Wave 9 (Age 32)
S9_full <- read_delim(paste0(path_prefix, "ns9_2022_main_interview.tab"), delim = "\t")
# Explicitly select all variables mentioned in metadata to avoid 'not found' errors
S9 <- S9_full %>% select(NSID, W9ECONACT2, starts_with("W9ACQUC0"), starts_with("W9VCQUC0"))

# Merge all
cohort_data <- S1 %>%
  full_join(S4, by = "NSID") %>%
  full_join(S6, by = "NSID") %>%
  full_join(S7, by = "NSID") %>%
  full_join(S8, by = "NSID") %>%
  full_join(S9, by = "NSID")

# Recoding Wave 4 (Age 17)
cohort_data <- cohort_data %>%
  mutate(educaim17 = case_when(
    w4saim == 14 ~ 5,
    w4saim %in% 1:11 ~ 1,
    w4saim == 12 ~ 3,
    w4saim == 13 ~ 3,
    TRUE ~ -3
  ))

# Recoding Wave 6 (Age 19)
cohort_data <- cohort_data %>%
  mutate(educaim19 = case_when(
    W6Saim == 16 ~ 5,
    W6Saim %in% 1:4 ~ 0,
    W6Saim %in% 5:13 ~ 1,
    W6Saim %in% 14:15 ~ 3,
    TRUE ~ -3
  ))

# Recoding Wave 7 (Age 20)
cohort_data <- cohort_data %>%
  mutate(educaim20 = case_when(
    W7SAim == -94 ~ -8,
    W7SAim == -91 ~ -1,
    W7SAim %in% 10:13 ~ 0,
    W7SAim %in% 1:9 ~ 1,
    W7SAim == 14 ~ 3,
    TRUE ~ -3
  ))

# Recoding Wave 8 (Age 25)
cohort_data <- cohort_data %>%
  mutate(educaim25 = case_when(
    W8ACTIVITY05 == 0 ~ 5,
    W8ACTIVITY05 == 1 & (W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 | W8ACQUC0D == 1 | W8ACQUC0E == 1 | W8VCQUC0J == 1 | W8VCQUC0K == 1) ~ 0,
    W8ACTIVITY05 == 1 & (W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1 | W8ACQUC0L == 1 | W8ACQUC0M == 1 | W8ACQUC0N == 1 | W8VCQUC0A == 1 | W8VCQUC0B == 1 | W8VCQUC0D == 1 | W8VCQUC0E == 1) ~ 1,
    W8ACTIVITY05 == 1 & W8VCQUC0C == 1 ~ 2,
    W8ACTIVITY05 == 1 & W8ACQUC0O == 1 ~ 4,
    W8ACTIVITY05 == 1 ~ 3,
    W8ACTIVITY05 == -9 ~ -9,
    W8ACTIVITY05 == -8 ~ -8,
    W8ACTIVITY05 == -1 ~ -1,
    TRUE ~ -3
  ))

# Recoding Wave 9 (Age 32)
# Using get() or checking column existence to be safe, but the metadata says they exist.
# The previous error was 'W9VCQUCAA' not found. Let's check columns of S9 first.
# Actually, I will use a safer approach: only use columns that exist in the data.

# Define the groupings based on metadata labels
higher_vars_w9 <- c("W9ACQUC0A", "W9ACQUC0B", "W9ACQUC0C", "W9ACQUC0D", "W9ACQUC0E", "W9VCQUC0A", "W9VCQUC0C", "W9VCQUC0R", "W9VCQUC0S", "W9VCQUC0V")
mid_vars_w9 <- c("W9ACQUC0F", "W9ACQUC0G", "W9ACQUC0H", "W9ACQUC0I", "W9ACQUC0J", "W9ACQUC0K", "W9ACQUC0L", "W9ACQUC0M", "W9VCQUC0B", "W9VCQUC0D", "W9VCQUC0E", "W9VCQUC0G", "W9VCQUC0H", "W9VCQUC0I", "W9VCQUC0J", "W9VCQUC0L", "W9VCQUC0M", "W9VCQUC0N", "W9VCQUC0O", "W9VCQUC0P", "W9VCQUC0W", "W9VCQUC0X", "W9VCQUC0Y")
entry_vars_w9 <- c("W9ACQUC0N", "W9VCQUC0F", "W9VCQUC0K", "W9VCQUC0Z", "W9VCQUCAA", "W9VCQUCAB")

# Filter lists to only include variables that actually exist in the dataframe
higher_vars_exist <- intersect(higher_vars_w9, names(cohort_data))
mid_vars_exist <- intersect(mid_vars_w9, names(cohort_data))
entry_vars_exist <- intersect(entry_vars_w9, names(cohort_data))

# Helper function to check if any of a set of columns are == 1
any_one <- function(df, vars) {
  if(length(vars) == 0) return(rep(FALSE, nrow(df)))
  rowSums(df[, vars] == 1, na.rm = TRUE) > 0
}

cohort_data <- cohort_data %>%
  mutate(
    is_studying = W9ECONACT2 %in% c(6, 7, 12),
    is_higher = any_one(cohort_data, higher_vars_exist),
    is_mid = any_one(cohort_data, mid_vars_exist),
    is_entry = any_one(cohort_data, entry_vars_exist),
    is_none = if("W9ACQUC0S" %in% names(cohort_data)) W9ACQUC0S == 1 else rep(FALSE, n()),
    
    educaim32 = case_when(
      !is_studying & !is.na(W9ECONACT2) & W9ECONACT2 >= 1 ~ 5,
      is_studying & is_higher ~ 0,
      is_studying & is_mid ~ 1,
      is_studying & is_entry ~ 2,
      is_studying & is_none ~ 4,
      is_studying ~ 3,
      W9ECONACT2 == -9 ~ -9,
      W9ECONACT2 == -8 ~ -8,
      W9ECONACT2 == -3 ~ -3,
      W9ECONACT2 == -1 ~ -1,
      TRUE ~ -3
    )
  ) %>% select(-is_studying, -is_higher, -is_mid, -is_entry, -is_none)

# Final Selection
final_data <- cohort_data %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write to CSV
write_csv(final_data, "data/output/cleaned_data.csv")
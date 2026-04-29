library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Function to safely check if a column exists and equals 1
check_val <- function(df, col_name) {
  if (col_name %in% names(df)) {
    res <- df[[col_name]] == 1
    res[is.na(res)] <- FALSE
    return(res)
  } else {
    return(rep(FALSE, nrow(df)))
  }
}

load_and_clean <- function(filename, vars) {
  df <- read_delim(paste0("data/input/", filename), delim = "\t", col_types = cols(.default = "c"))
  df <- df %>% 
    select(any_of(vars)) %>% 
    distinct(NSID, .keep_all = TRUE) %>% 
    mutate(NSID = as.character(NSID))
  
  cols_to_num <- setdiff(names(df), "NSID")
  df <- df %>% mutate(across(all_of(cols_to_num), as.numeric))
  return(df)
}

S1_vars <- c("NSID")
S4_vars <- c("NSID", "w4saim")
S6_vars <- c("NSID", "W6Saim")
S7_vars <- c("NSID", "W7SAim")
S8_vars <- c("NSID", "W8ACTIVITY05", "W8ACQUC0A", "W8ACQUC0B", "W8ACQUC0C", "W8ACQUC0D", "W8ACQUC0E", "W8ACQUC0F", "W8ACQUC0G", "W8ACQUC0H", "W8ACQUC0I", "W8ACQUC0J", "W8ACQUC0K", "W8ACQUC0L", "W8ACQUC0M", "W8ACQUC0N", "W8ACQUC0O", "W8ACQUC0P", "W8ACQUC0Q", "W8VCQUC0A", "W8VCQUC0B", "W8VCQUC0C", "W8VCQUC0D", "W8VCQUC0E", "W8VCQUC0J", "W8VCQUC0K")
S9_vars <- c("NSID", "W9ECONACT2", "W9ACQUC0A", "W9ACQUC0B", "W9ACQUC0C", "W9ACQUC0D", "W9ACQUC0E", "W9ACQUC0F", "W9ACQUC0G", "W9ACQUC0H", "W9ACQUC0I", "W9ACQUC0J", "W9ACQUC0K", "W9ACQUC0L", "W9ACQUC0M", "W9ACQUC0N", "W9ACQUC0O", "W9ACQUC0P", "W9ACQUC0Q", "W9ACQUC0R", "W9ACQUC0S", "W9ACQUC0T", "W9ACQUC0U", "W9ACQUC0V", "W9VCQUC0A", "W9VCQUC0B", "W9VCQUC0C", "W9VCQUC0D", "W9VCQUC0E", "W9VCQUC0F", "W9VCQUC0G", "W9VCQUC0H", "W9VCQUC0I", "W9VCQUC0J", "W9VCQUC0O", "W9VCQUC0P", "W9VCQUC0Q", "W9VCQUC0R", "W9VCQUC0T", "W9VCQUC0U", "W9VCQUC0W", "W9VCQUC0X", "W9VCQUC0Y", "W9VCQUC0Z", "W9VCQUCAA", "W9VCQUCAB", "W9VCQUCAC", "W9VCQUCAD", "W9VCQUCAE", "W9VCQUCAF", "W9VCQUCAG", "W9VCQUCAH", "W9VCQUCAI")

S1 <- load_and_clean("wave_one_lsype_young_person_2020.tab", S1_vars)
S4 <- load_and_clean("wave_four_lsype_young_person_2020.tab", S4_vars)
S6 <- load_and_clean("wave_six_lsype_young_person_2020.tab", S6_vars)
S7 <- load_and_clean("wave_seven_lsype_young_person_2020.tab", S7_vars)
S8 <- load_and_clean("ns8_2015_main_interview.tab", S8_vars)
S9 <- load_and_clean("ns9_2022_main_interview.tab", S9_vars)

cohort_df <- S1 %>%
  full_join(S4, by = "NSID") %>%
  full_join(S6, by = "NSID") %>%
  full_join(S7, by = "NSID") %>%
  full_join(S8, by = "NSID") %>%
  full_join(S9, by = "NSID")

fix_missing <- function(x) {
  x[is.na(x)] <- -3
  return(x)
}

cohort_df <- cohort_df %>%
  mutate(educaim17 = case_when(w4saim %in% 1:11 ~ 1, w4saim %in% 12:13 ~ 3, w4saim == 14 ~ 5, w4saim < 0 ~ w4saim, TRUE ~ -3)) %>%
  mutate(educaim17 = fix_missing(educaim17)) %>%
  mutate(educaim19 = case_when(W6Saim %in% 1:4 ~ 0, W6Saim %in% 5:13 ~ 1, W6Saim %in% 14:15 ~ 3, W6Saim == 16 ~ 5, W6Saim < 0 ~ W6Saim, TRUE ~ -3)) %>%
  mutate(educaim19 = fix_missing(educaim19)) %>%
  mutate(educaim20 = case_when(W7SAim %in% 10:13 ~ 0, W7SAim %in% 1:9 ~ 1, W7SAim == 14 ~ 3, W7SAim == -91 ~ 5, W7SAim == -94 ~ -8, W7SAim < 0 ~ W7SAim, TRUE ~ -3)) %>%
  mutate(educaim20 = fix_missing(educaim20))

cohort_df <- cohort_df %>%
  mutate(educaim25 = case_when(
    W8ACTIVITY05 == 0 ~ 5,
    W8ACTIVITY05 == 1 ~ case_when(
      (W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 | W8ACQUC0D == 1 | W8ACQUC0E == 1 | W8VCQUC0K == 1) ~ 0,
      (W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1 | W8ACQUC0L == 1 | W8ACQUC0M == 1 | W8ACQUC0N == 1 | W8VCQUC0A == 1 | W8VCQUC0B == 1 | W8VCQUC0D == 1 | W8VCQUC0E == 1 | W8VCQUC0J == 1) ~ 1,
      (W8VCQUC0C == 1) ~ 2,
      (W8ACQUC0O == 1) ~ 4,
      TRUE ~ -3
    ),
    W8ACTIVITY05 < 0 ~ W8ACTIVITY05,
    TRUE ~ -3
  )) %>%
  mutate(educaim25 = fix_missing(educaim25))

# Age 32 logic - using base R to avoid NA subscript issues
cohort_df$educaim32 <- NA

# Non-studying
not_studying_idx <- which(!is.na(cohort_df$W9ECONACT2) & cohort_df$W9ECONACT2 >= 0 & !cohort_df$W9ECONACT2 %in% c(6, 7, 12))
cohort_df$educaim32[not_studying_idx] <- 5

# Studying
studying_idx <- which(!is.na(cohort_df$W9ECONACT2) & cohort_df$W9ECONACT2 %in% c(6, 7, 12))
if(length(studying_idx) > 0) {
    h_vars <- c("W9ACQUC0A", "W9ACQUC0B", "W9ACQUC0C", "W9ACQUC0D", "W9ACQUC0E", "W9VCQUC0A", "W9VCQUC0C", "W9VCQUC0S", "W9VCQUC0V", "W9VCQUCAC")
    m_vars <- c("W9ACQUC0G", "W9ACQUC0H", "W9ACQUC0I", "W9ACQUC0J", "W9ACQUC0K", "W9ACQUC0L", "W9ACQUC0M", "W9ACQUC0N", "W9ACQUC0O", "W9ACQUC0P", "W9ACQUC0Q", "W9VCQUC0B", "W9VCQUC0D", "W9VCQUC0E", "W9VCQUC0F", "W9VCQUC0G", "W9VCQUC0H", "W9VCQUC0I", "W9VCQUC0J", "W9VCQUC0O", "W9VCQUC0P", "W9VCQUC0Q", "W9VCQUC0R", "W9VCQUC0T", "W9VCQUC0U", "W9VCQUC0W", "W9VCQUC0X")
    e_vars <- c("W9ACQUC0R", "W9VCQUC0K", "W9VCQUC0Y", "W9VCQUC0Z", "W9VCQUCAA", "W9VCQUCAB", "W9VCQUCAE")
    
    is_h <- reduce(map(h_vars, ~check_val(cohort_df[studying_idx, ], .x)), `|`)
    is_m <- reduce(map(m_vars, ~check_val(cohort_df[studying_idx, ], .x)), `|`)
    is_e <- reduce(map(e_vars, ~check_val(cohort_df[studying_idx, ], .x)), `|`)
    
    vals <- case_when(
        is_h ~ 0,
        is_m ~ 1,
        is_e ~ 2,
        check_val(cohort_df[studying_idx, ], "W9ACQUC0R") ~ 3,
        (check_val(cohort_df[studying_idx, ], "W9ACQUC0S") | check_val(cohort_df[studying_idx, ], "W9VCQUCAG")) ~ 4,
        TRUE ~ -3
    )
    cohort_df$educaim32[studying_idx] <- vals
}

# Missing codes for W9ECONACT2
missing_idx <- which(!is.na(cohort_df$W9ECONACT2) & cohort_df$W9ECONACT2 < 0)
cohort_df$educaim32[missing_idx] <- cohort_df$W9ECONACT2[missing_idx]

cohort_df$educaim32 <- fix_missing(cohort_df$educaim32)

final_df <- cohort_df %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

write_csv(final_df, "data/output/cleaned_data.csv")
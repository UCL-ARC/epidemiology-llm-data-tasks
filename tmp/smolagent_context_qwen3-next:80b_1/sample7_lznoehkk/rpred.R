library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Process wave4 (age 17)
wave4_processed <- wave4 %>%
  select(NSID, w4saim) %>%
  mutate(
    educaim17 = case_when(
      w4saim == 1.0 ~ 1,
      w4saim == 2.0 ~ 1,
      w4saim == 3.0 ~ 1,
      w4saim == 4.0 ~ 1,
      w4saim == 5.0 ~ 1,
      w4saim == 6.0 ~ 1,
      w4saim == 7.0 ~ 1,
      w4saim == 8.0 ~ 1,
      w4saim == 9.0 ~ 1,
      w4saim == 10.0 ~ 2,
      w4saim == 11.0 ~ 2,
      w4saim == 12.0 ~ 3,
      w4saim == 13.0 ~ 3,
      w4saim == 14.0 ~ 5,
      w4saim == -9 ~ -9,
      w4saim == -8 ~ -8,
      w4saim == -1 ~ -1,
      w4saim == -3 ~ -3,
      w4saim == -2 ~ -2,
      w4saim < -1 & w4saim != -9 & w4saim != -8 & w4saim != -3 & w4saim != -2 ~ -3,
      TRUE ~ NA_real_
    )
  )

# Process wave6 (age 19)
wave6_processed <- wave6 %>%
  select(NSID, W6Saim) %>%
  mutate(
    educaim19 = case_when(
      W6Saim == 1.0 ~ 0,
      W6Saim == 2.0 ~ 0,
      W6Saim == 3.0 ~ 0,
      W6Saim == 4.0 ~ 0,
      W6Saim == 5.0 ~ 1,
      W6Saim == 6.0 ~ 1,
      W6Saim == 7.0 ~ 1,
      W6Saim == 8.0 ~ 1,
      W6Saim == 9.0 ~ 1,
      W6Saim == 10.0 ~ 1,
      W6Saim == 11.0 ~ 1,
      W6Saim == 12.0 ~ 1,
      W6Saim == 13.0 ~ 2,
      W6Saim == 14.0 ~ 3,
      W6Saim == 15.0 ~ 3,
      W6Saim == 16.0 ~ 5,
      W6Saim == -9 ~ -9,
      W6Saim == -8 ~ -8,
      W6Saim == -1 ~ -1,
      W6Saim == -3 ~ -3,
      W6Saim == -2 ~ -2,
      W6Saim < -1 & W6Saim != -9 & W6Saim != -8 & W6Saim != -3 & W6Saim != -2 ~ -3,
      TRUE ~ NA_real_
    )
  )

# Process wave7 (age 20)
wave7_processed <- wave7 %>%
  select(NSID, W7SAim) %>%
  mutate(
    educaim20 = case_when(
      W7SAim == -91.0 ~ 5,
      W7SAim == -94.0 ~ -2,
      W7SAim == 1.0 ~ 1,
      W7SAim == 2.0 ~ 2,
      W7SAim == 3.0 ~ 1,
      W7SAim == 4.0 ~ 1,
      W7SAim == 5.0 ~ 1,
      W7SAim == 6.0 ~ 1,
      W7SAim == 7.0 ~ 1,
      W7SAim == 8.0 ~ 1,
      W7SAim == 9.0 ~ 1,
      W7SAim == 10.0 ~ 0,
      W7SAim == 11.0 ~ 0,
      W7SAim == 12.0 ~ 0,
      W7SAim == 13.0 ~ 0,
      W7SAim == 14.0 ~ 3,
      W7SAim == -9 ~ -9,
      W7SAim == -8 ~ -8,
      W7SAim == -1 ~ -1,
      W7SAim == -3 ~ -3,
      W7SAim == -2 ~ -2,
      W7SAim < -1 & W7SAim != -9 & W7SAim != -8 & W7SAim != -3 & W7SAim != -2 & W7SAim != -91 & W7SAim != -94 ~ -3,
      TRUE ~ NA_real_
    )
  )

# Process wave8 (age 25)
wave8_processed <- wave8 %>%
  select(NSID, W8ACTIVITY05, W8ACQUC0A, W8ACQUC0B, W8ACQUC0C, W8ACQUC0D, W8ACQUC0E, W8ACQUC0F, W8ACQUC0G, W8ACQUC0H, W8ACQUC0I, W8ACQUC0J, W8ACQUC0K, W8ACQUC0L, W8ACQUC0M, W8ACQUC0N, W8ACQUC0O, W8ACQUC0P, W8ACQUC0Q, W8VCQUC0A, W8VCQUC0B, W8VCQUC0C, W8VCQUC0D, W8VCQUC0E, W8VCQUC0J, W8VCQUC0K) %>%
  mutate(
    studying = case_when(
      W8ACTIVITY05 == 1.0 ~ TRUE,
      W8ACTIVITY05 == 0.0 ~ FALSE,
      TRUE ~ NA
    ),
    educaim25 = case_when(
      !is.na(studying) & !studying ~ 5,
      !is.na(studying) & studying & (W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 | W8ACQUC0D == 1 | W8ACQUC0E == 1 | W8VCQUC0J == 1 | W8VCQUC0K == 1) ~ 0,
      !is.na(studying) & studying & (W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1 | W8ACQUC0L == 1 | W8ACQUC0M == 1 | W8ACQUC0N == 1 | W8VCQUC0A == 1 | W8VCQUC0B == 1 | W8VCQUC0C == 1 | W8VCQUC0D == 1 | W8VCQUC0E == 1) ~ 1,
      !is.na(studying) & studying & (W8VCQUC0C == 1 | W8VCQUC0D == 1) ~ 2,
      !is.na(studying) & studying & (W8ACQUC0O == 1 | W8ACQUC0P == 1 | W8ACQUC0Q == 1) ~ 3,
      !is.na(studying) & studying ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-studying)

# Process wave9 (age 32)
wave9_processed <- wave9 %>%
  select(NSID, W9ECONACT2, W9ACQUC0A, W9ACQUC0B, W9ACQUC0C, W9ACQUC0D, W9ACQUC0E, W9ACQUC0F, W9ACQUC0G, W9ACQUC0H, W9ACQUC0I, W9ACQUC0J, W9ACQUC0K, W9ACQUC0L, W9ACQUC0M, W9ACQUC0N, W9ACQUC0O, W9ACQUC0P, W9ACQUC0Q, W9ACQUC0R, W9ACQUC0S, W9ACQUC0T, W9ACQUC0U, W9ACQUC0V, W9VCQUC0A, W9VCQUC0B, W9VCQUC0C, W9VCQUC0D, W9VCQUC0E, W9VCQUC0F, W9VCQUC0G, W9VCQUC0H, W9VCQUC0I, W9VCQUC0J, W9VCQUC0K, W9VCQUC0L, W9VCQUC0M, W9VCQUC0N, W9VCQUC0O, W9VCQUC0P, W9VCQUC0Q, W9VCQUC0R, W9VCQUC0S, W9VCQUC0T, W9VCQUC0U, W9VCQUC0V, W9VCQUC0W, W9VCQUC0X, W9VCQUC0Y, W9VCQUC0Z, W9VCQUCAA, W9VCQUCAB, W9VCQUCAC, W9VCQUCAD, W9VCQUCAE, W9VCQUCAF, W9VCQUCAG, W9VCQUCAH, W9VCQUCAI) %>%
  mutate(
    studying = case_when(
      W9ECONACT2 == 6.0 | W9ECONACT2 == 7.0 ~ TRUE,
      W9ECONACT2 == 1.0 | W9ECONACT2 == 2.0 | W9ECONACT2 == 3.0 | W9ECONACT2 == 4.0 | W9ECONACT2 == 5.0 | W9ECONACT2 == 8.0 | W9ECONACT2 == 9.0 | W9ECONACT2 == 10.0 | W9ECONACT2 == 11.0 | W9ECONACT2 == 12.0 | W9ECONACT2 == 13.0 | W9ECONACT2 == 14.0 ~ FALSE,
      TRUE ~ NA
    ),
    educaim32 = case_when(
      !is.na(studying) & !studying ~ 5,
      !is.na(studying) & studying & (W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 | W9ACQUC0D == 1 | W9ACQUC0E == 1 | W9ACQUC0F == 1 | W9VCQUC0A == 1) ~ 0,
      !is.na(studying) & studying & (W9ACQUC0G == 1 | W9ACQUC0H == 1 | W9ACQUC0I == 1 | W9ACQUC0J == 1 | W9ACQUC0K == 1 | W9ACQUC0L == 1 | W9ACQUC0M == 1 | W9ACQUC0N == 1 | W9ACQUC0O == 1 | W9ACQUC0P == 1 | W9ACQUC0Q == 1 | W9ACQUC0R == 1 | W9ACQUC0S == 1 | W9ACQUC0T == 1 | W9ACQUC0U == 1 | W9ACQUC0V == 1 | W9VCQUC0B == 1 | W9VCQUC0C == 1 | W9VCQUC0D == 1 | W9VCQUC0E == 1 | W9VCQUC0F == 1 | W9VCQUC0G == 1 | W9VCQUC0H == 1 | W9VCQUC0I == 1 | W9VCQUC0J == 1 | W9VCQUC0K == 1 | W9VCQUC0L == 1 | W9VCQUC0M == 1 | W9VCQUC0N == 1 | W9VCQUC0O == 1 | W9VCQUC0P == 1 | W9VCQUC0Q == 1 | W9VCQUC0R == 1 | W9VCQUC0S == 1 | W9VCQUC0T == 1 | W9VCQUC0U == 1 | W9VCQUC0V == 1 | W9VCQUC0W == 1 | W9VCQUC0X == 1 | W9VCQUC0Y == 1 | W9VCQUC0Z == 1 | W9VCQUCAA == 1 | W9VCQUCAB == 1 | W9VCQUCAC == 1 | W9VCQUCAD == 1 | W9VCQUCAE == 1 | W9VCQUCAF == 1 | W9VCQUCAG == 1 | W9VCQUCAH == 1 | W9VCQUCAI == 1) ~ 1,
      !is.na(studying) & studying & (W9ACQUC0S == 1 | W9ACQUC0T == 1 | W9ACQUC0U == 1 | W9ACQUC0V == 1) ~ 2,
      !is.na(studying) & studying & (W9ACQUC0S == 1 | W9ACQUC0T == 1 | W9ACQUC0U == 1 | W9ACQUC0V == 1) ~ 3,
      !is.na(studying) & studying ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-studying)

# Merge all waves
cleaned_data <- wave1 %>%
  select(NSID) %>%
  full_join(wave4_processed, by = "NSID") %>%
  full_join(wave6_processed, by = "NSID") %>%
  full_join(wave7_processed, by = "NSID") %>%
  full_join(wave8_processed, by = "NSID") %>%
  full_join(wave9_processed, by = "NSID") %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write to output
write_csv(cleaned_data, "data/output/cleaned_data.csv")

cat("Script executed successfully. Output saved to data/output/cleaned_data.csv\n")
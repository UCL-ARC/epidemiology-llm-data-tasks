library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets from data/input/
df14 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
df17 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
df19 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
df20 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
df25 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
df32 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Process Age 17 (wave_four)
df17 <- df17 %>%
  mutate(w4saim = case_when(
    w4saim == -9 ~ -9,
    w4saim == -8 ~ -8,
    w4saim == -1 ~ -1,
    w4saim == -3 ~ -3,
    w4saim == -2 ~ -2,
    w4saim < 0 ~ -3,
    TRUE ~ w4saim
  )) %>%
  mutate(educaim17 = case_when(
    w4saim %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ 1,
    w4saim %in% c(9, 10, 11) ~ 2,
    w4saim %in% c(12, 13) ~ 3,
    w4saim == 14 ~ 5,
    w4saim %in% c(-9, -8, -1, -3, -2) ~ w4saim,
    TRUE ~ -3
  )) %>%
  select(NSID, educaim17)

# Process Age 19 (wave_six)
df19 <- df19 %>%
  mutate(W6Saim = case_when(
    W6Saim == -9 ~ -9,
    W6Saim == -8 ~ -8,
    W6Saim == -1 ~ -1,
    W6Saim == -3 ~ -3,
    W6Saim == -2 ~ -2,
    W6Saim < 0 ~ -3,
    TRUE ~ W6Saim
  )) %>%
  mutate(educaim19 = case_when(
    W6Saim %in% c(1, 2, 3, 4) ~ 0,
    W6Saim %in% c(5, 6, 7, 8, 9, 10, 11) ~ 1,
    W6Saim %in% c(12, 13) ~ 2,
    W6Saim %in% c(14, 15) ~ 3,
    W6Saim == 16 ~ 5,
    W6Saim %in% c(-9, -8, -1, -3, -2) ~ W6Saim,
    TRUE ~ -3
  )) %>%
  select(NSID, educaim19)

# Process Age 20 (wave_seven)
df20 <- df20 %>%
  mutate(W7SAim = case_when(
    W7SAim == -94 ~ -2,
    W7SAim == -91 ~ -1,
    W7SAim == -9 ~ -9,
    W7SAim == -8 ~ -8,
    W7SAim == -1 ~ -1,
    W7SAim == -3 ~ -3,
    W7SAim == -2 ~ -2,
    W7SAim < 0 ~ -3,
    TRUE ~ W7SAim
  )) %>%
  mutate(educaim20 = case_when(
    W7SAim %in% c(10, 11, 12, 13) ~ 0,
    W7SAim %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ 1,
    W7SAim %in% c(1, 2) ~ 2,
    W7SAim == 14 ~ 3,
    W7SAim %in% c(-9, -8, -1, -3, -2) ~ W7SAim,
    TRUE ~ -3
  )) %>%
  select(NSID, educaim20)

# Process Age 25 (ns8)
df25 <- df25 %>%
  mutate(W8ACTIVITY05 = case_when(
    W8ACTIVITY05 == -9 ~ -9,
    W8ACTIVITY05 == -8 ~ -8,
    W8ACTIVITY05 == -1 ~ -1,
    TRUE ~ W8ACTIVITY05
  ))

# Define category variables for academic and vocational
academic_cat0 <- c("W8ACQUC0A", "W8ACQUC0B", "W8ACQUC0C", "W8ACQUC0D", "W8ACQUC0E")
academic_cat1 <- c("W8ACQUC0F", "W8ACQUC0G", "W8ACQUC0H", "W8ACQUC0I", "W8ACQUC0J", "W8ACQUC0K", "W8ACQUC0L", "W8ACQUC0M")
academic_cat2 <- c()
academic_cat4 <- c("W8ACQUC0O")
academic_cat3 <- c("W8ACQUC0P", "W8ACQUC0Q")

vocational_cat0 <- c("W8VCQUC0K")
vocational_cat1 <- c("W8VCQUC0J")
vocational_cat2 <- c("W8VCQUC0C", "W8VCQUC0D")
vocational_cat4 <- c()
vocational_cat3 <- c("W8VCQUC0A", "W8VCQUC0B", "W8VCQUC0E", "W8VCQUC0F", "W8VCQUC0G", "W8VCQUC0H", "W8VCQUC0I")

# Create sum variables for categories
df25 <- df25 %>%
  mutate(
    cat0_sum = rowSums(across(all_of(c(academic_cat0, vocational_cat0))), na.rm = TRUE),
    cat1_sum = rowSums(across(all_of(c(academic_cat1, vocational_cat1))), na.rm = TRUE),
    cat2_sum = rowSums(across(all_of(vocational_cat2)), na.rm = TRUE),
    cat4_sum = rowSums(across(all_of(academic_cat4)), na.rm = TRUE),
    cat3_sum = rowSums(across(all_of(c(academic_cat3, vocational_cat3))), na.rm = TRUE)
  ) %>%
  mutate(
    educaim25 = case_when(
      W8ACTIVITY05 == 0 ~ 5,
      W8ACTIVITY05 == 1 & cat0_sum > 0 ~ 0,
      W8ACTIVITY05 == 1 & cat0_sum == 0 & cat1_sum > 0 ~ 1,
      W8ACTIVITY05 == 1 & cat0_sum == 0 & cat1_sum == 0 & cat2_sum > 0 ~ 2,
      W8ACTIVITY05 == 1 & cat0_sum == 0 & cat1_sum == 0 & cat2_sum == 0 & cat4_sum > 0 ~ 4,
      W8ACTIVITY05 == 1 & cat0_sum == 0 & cat1_sum == 0 & cat2_sum == 0 & cat4_sum == 0 & cat3_sum > 0 ~ 3,
      W8ACTIVITY05 %in% c(-9, -8, -1) ~ W8ACTIVITY05,
      TRUE ~ -3
    )
  ) %>%
  select(NSID, educaim25)

# Process Age 32 (ns9)
df32 <- df32 %>%
  mutate(W9ECONACT2 = case_when(
    W9ECONACT2 == -9 ~ -9,
    W9ECONACT2 == -8 ~ -8,
    W9ECONACT2 == -3 ~ -3,
    W9ECONACT2 == -1 ~ -1,
    TRUE ~ W9ECONACT2
  ))

# Define category variables for academic and vocational
academic_cat0_32 <- c("W9ACQUC0A", "W9ACQUC0B", "W9ACQUC0C", "W9ACQUC0D", "W9ACQUC0E", "W9ACQUC0F", "W9VCQUC0A", "W9VCQUC0B", "W9VCQUC0C", "W9VCQUC0R", "W9VCQUC0S", "W9VCQUC0V", "W9VCQUC0W", "W9VCQUC0X", "W9VCQUC0Y", "W9VCQUC0Z", "W9VCQUCAA", "W9VCQUCAC", "W9VCQUCAD")
academic_cat1_32 <- c("W9ACQUC0G", "W9ACQUC0H", "W9ACQUC0I", "W9ACQUC0J", "W9ACQUC0K", "W9ACQUC0L", "W9ACQUC0M", "W9ACQUC0N", "W9ACQUC0O", "W9ACQUC0P", "W9ACQUC0Q", "W9VCQUC0D", "W9VCQUC0E", "W9VCQUC0G", "W9VCQUC0H", "W9VCQUC0I", "W9VCQUC0J", "W9VCQUC0K", "W9VCQUC0L", "W9VCQUC0M", "W9VCQUC0N", "W9VCQUC0O", "W9VCQUC0P", "W9VCQUC0Q", "W9VCQUC0T", "W9VCQUC0U", "W9VCQUC0W", "W9VCQUC0X", "W9VCQUC0Y", "W9VCQUC0Z", "W9VCQUCAA", "W9VCQUCAB", "W9VCQUCAE")
academic_cat2_32 <- c("W9VCQUC0F", "W9VCQUC0K", "W9VCQUC0Q")
academic_cat4_32 <- c("W9ACQUC0S", "W9VCQUCAG")
academic_cat3_32 <- c("W9ACQUC0R", "W9ACQUC0T", "W9ACQUC0U", "W9VCQUCAF", "W9VCQUCAH", "W9VCQUCAI")

# Create sum variables for categories
df32 <- df32 %>%
  mutate(
    cat0_sum = rowSums(across(all_of(academic_cat0_32)), na.rm = TRUE),
    cat1_sum = rowSums(across(all_of(academic_cat1_32)), na.rm = TRUE),
    cat2_sum = rowSums(across(all_of(academic_cat2_32)), na.rm = TRUE),
    cat4_sum = rowSums(across(all_of(academic_cat4_32)), na.rm = TRUE),
    cat3_sum = rowSums(across(all_of(academic_cat3_32)), na.rm = TRUE)
  ) %>%
  mutate(
    educaim32 = case_when(
      W9ECONACT2 %in% c(6, 7) & cat0_sum > 0 ~ 0,
      W9ECONACT2 %in% c(6, 7) & cat0_sum == 0 & cat1_sum > 0 ~ 1,
      W9ECONACT2 %in% c(6, 7) & cat0_sum == 0 & cat1_sum == 0 & cat2_sum > 0 ~ 2,
      W9ECONACT2 %in% c(6, 7) & cat0_sum == 0 & cat1_sum == 0 & cat2_sum == 0 & cat4_sum > 0 ~ 4,
      W9ECONACT2 %in% c(6, 7) & cat0_sum == 0 & cat1_sum == 0 & cat2_sum == 0 & cat4_sum == 0 & cat3_sum > 0 ~ 3,
      !W9ECONACT2 %in% c(6, 7) & !is.na(W9ECONACT2) ~ 5,
      W9ECONACT2 %in% c(-9, -8, -3, -1) ~ W9ECONACT2,
      TRUE ~ -3
    )
  ) %>%
  select(NSID, educaim32)

# Merge all datasets on NSID
cleaned_data <- df14 %>%
  full_join(df17, by = "NSID") %>%
  full_join(df19, by = "NSID") %>%
  full_join(df20, by = "NSID") %>%
  full_join(df25, by = "NSID") %>%
  full_join(df32, by = "NSID")

# Select only required columns and write output
cleaned_data <- cleaned_data %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

write_csv(cleaned_data, "data/output/cleaned_data.csv")
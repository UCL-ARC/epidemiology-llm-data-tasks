library(readr)
library(dplyr)

# Load data
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = "f"))
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = "f"))
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = "f"))
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = "f"))
ns8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", col_types = cols(.default = "f"))
ns9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", col_types = cols(.default = "f"))

# Standard missing mapping
standardize_missing <- function(x){
  x <- ifelse(x %in% c(-999,-998,-997,-995), -2, x)
  x <- ifelse(x %in% c(-94), -8, x)
  x <- ifelse(x %in% c(-92), -9, x)
  x <- ifelse(x %in% c(-91), -1, x)
  x <- ifelse(x %in% c(-99), -3, x)
  return(x)
}

# Derived for each wave
# 17
educaim17 <- wave_four %>%
  mutate(educaim17 = case_when(
    w4saim %in% c(1,5,9) ~ 1,
    w4saim %in% c(2,4,6,10,11,12,13) ~ 3,
    w4saim %in% c(3,7,8) ~ 3,
    w4saim == 14 ~ 4,
    TRUE ~ NA_real_
  )) %>%
  mutate(educaim17 = standardize_missing(educaim17)) %>%
  select(NSID, educaim17)

# 19
educaim19 <- wave_six %>%
  mutate(educaim19 = case_when(
    W6Saim %in% c(1,5,9,12) ~ 1,
    W6Saim %in% c(2,3,4,6,7,8,10,11,13) ~ 0,
    W6Saim == 14 ~ 4,
    W6Saim == 16 ~ 5,
    TRUE ~ NA_real_
  )) %>%
  mutate(educaim19 = standardize_missing(educaim19)) %>%
  select(NSID, educaim19)

# 20
educaim20 <- wave_seven %>%
  mutate(educaim20 = case_when(
    W7SAim %in% c(1,3,6) ~ 1,
    W7SAim %in% c(2,5,8,10,11,12,13,14,9,4) ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(educaim20 = standardize_missing(educaim20)) %>%
  select(NSID, educaim20)

# 25
academics25_flag <- ns8 %>% select(starts_with("W8ACQUC0")) %>% rowwise() %>% mutate(ac_flag = any(c_across(everything()) == 1)) %>% pull(ac_flag)
voc25_flag <- ns8 %>% select(starts_with("W8VCQUC0")) %>% rowwise() %>% mutate(voc_flag = any(c_across(everything()) == 1)) %>% pull(voc_flag)

educaim25 <- ns8 %>%
  mutate(educaim25 = case_when(
    W8ACTIVITY05 == 0 ~ 5,
    W8ACTIVITY05 == 1 & W8VCQUC0J == 1 ~ 0,
    W8ACTIVITY05 == 1 & academics25_flag ~ 1,
    W8ACTIVITY05 == 1 & voc25_flag ~ 3,
    W8ACTIVITY05 == 1 & !academics25_flag & !voc25_flag ~ 4,
    TRUE ~ NA_real_
  )) %>%
  mutate(educaim25 = standardize_missing(educaim25)) %>%
  select(NSID, educaim25)

# 32
academics32_flag <- ns9 %>% select(starts_with("W9ACQUC0")) %>% rowwise() %>% mutate(ac_flag = any(c_across(everything()) == 1)) %>% pull(ac_flag)
voc32_flag <- ns9 %>% select(starts_with("W9VCQUC0")) %>% rowwise() %>% mutate(voc_flag = any(c_across(everything()) == 1)) %>% pull(voc_flag)

educaim32 <- ns9 %>%
  mutate(educaim32 = case_when(
    W9ECONACT2 %in% c(6,7) & (academics32_flag | voc32_flag) ~ 0,
    W9ECONACT2 %in% c(6,7) & !academics32_flag & !voc32_flag ~ 1,
    TRUE ~ NA_real_
  )) %>%
  mutate(educaim32 = standardize_missing(educaim32)) %>%
  select(NSID, educaim32)

# Merge all data on NSID
merged <- wave_one %>%
  full_join(wave_four %>% select(NSID, w4saim), by = "NSID") %>%
  full_join(wave_six %>% select(NSID, W6Saim), by = "NSID") %>%
  full_join(wave_seven %>% select(NSID, W7SAim), by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Add derived columns via left_join
merged <- merged %>%
  left_join(educaim17, by = "NSID") %>%
  left_join(educaim19, by = "NSID") %>%
  left_join(educaim20, by = "NSID") %>%
  left_join(educaim25, by = "NSID") %>%
  left_join(educaim32, by = "NSID")

final <- merged %>% select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

write_csv(final, "data/output/cleaned_data.csv")

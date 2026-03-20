library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

data_merged <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Wave 1 (Age 14)
data_merged <- data_merged %>%
  mutate(W1hous12HH = case_when(
    W1hous12HH == -999 ~ -2,
    W1hous12HH == -92 ~ -9,
    W1hous12HH == -91 ~ -1,
    W1hous12HH == -1 ~ -8,
    TRUE ~ W1hous12HH
  )) %>%
  mutate(hownteen14 = W1hous12HH) %>%
  mutate(hown14 = case_when(
    hownteen14 == 1 ~ 1,
    hownteen14 == 2 ~ 2,
    hownteen14 == 3 ~ 3,
    hownteen14 %in% c(4, 5, 6) ~ 4,
    hownteen14 == 7 ~ 5,
    hownteen14 == 8 ~ 6,
    TRUE ~ hownteen14
  ))

# Wave 2 (Age 15)
data_merged <- data_merged %>%
  mutate(W2Hous12HH = case_when(
    W2Hous12HH == -999 ~ -2,
    W2Hous12HH == -998 ~ -2,
    W2Hous12HH == -997 ~ -2,
    W2Hous12HH == -995 ~ -2,
    W2Hous12HH == -99 ~ -2,
    W2Hous12HH == -92 ~ -9,
    W2Hous12HH == -91 ~ -1,
    W2Hous12HH == -1 ~ -8,
    TRUE ~ W2Hous12HH
  )) %>%
  mutate(hownteen15 = W2Hous12HH) %>%
  mutate(hown15 = case_when(
    hownteen15 == 1 ~ 1,
    hownteen15 == 2 ~ 2,
    hownteen15 == 3 ~ 3,
    hownteen15 %in% c(4, 5, 6) ~ 4,
    hownteen15 == 7 ~ 5,
    hownteen15 == 8 ~ 6,
    TRUE ~ hownteen15
  ))

# Wave 3 (Age 16)
data_merged <- data_merged %>%
  mutate(W3hous12HH = case_when(
    W3hous12HH == -999 ~ -2,
    W3hous12HH == -99 ~ -2,
    W3hous12HH == -92 ~ -9,
    W3hous12HH == -91 ~ -1,
    W3hous12HH == -1 ~ -8,
    TRUE ~ W3hous12HH
  )) %>%
  mutate(hownteen16 = W3hous12HH) %>%
  mutate(hown16 = case_when(
    hownteen16 == 1 ~ 1,
    hownteen16 == 2 ~ 2,
    hownteen16 == 3 ~ 3,
    hownteen16 %in% c(4, 5, 6) ~ 4,
    hownteen16 == 7 ~ 5,
    hownteen16 == 8 ~ 6,
    TRUE ~ hownteen16
  ))

# Wave 4 (Age 17)
data_merged <- data_merged %>%
  mutate(W4Hous12HH = case_when(
    W4Hous12HH == -999 ~ -2,
    W4Hous12HH == -997 ~ -2,
    W4Hous12HH == -92 ~ -9,
    W4Hous12HH == -91 ~ -1,
    W4Hous12HH == -1 ~ -8,
    TRUE ~ W4Hous12HH
  )) %>%
  mutate(hownteen17 = W4Hous12HH) %>%
  mutate(hown17 = case_when(
    hownteen17 == 1 ~ 1,
    hownteen17 == 2 ~ 2,
    hownteen17 == 3 ~ 3,
    hownteen17 %in% c(4, 5, 6) ~ 4,
    hownteen17 == 7 ~ 5,
    hownteen17 == 8 ~ 6,
    TRUE ~ hownteen17
  ))

# Wave 5 (Age 18)
data_merged <- data_merged %>%
  mutate(W5Hous12HH = case_when(
    W5Hous12HH == -999 ~ -2,
    W5Hous12HH == -92 ~ -9,
    W5Hous12HH == -91 ~ -1,
    W5Hous12HH == -1 ~ -8,
    W5Hous12HH == 6 ~ -3,
    TRUE ~ W5Hous12HH
  )) %>%
  mutate(W5Hous12BHH = case_when(
    W5Hous12BHH == -999 ~ -2,
    W5Hous12BHH == -92 ~ -9,
    W5Hous12BHH == -91 ~ -1,
    W5Hous12BHH == -1 ~ -8,
    TRUE ~ W5Hous12BHH
  )) %>%
  mutate(W5Hous12CHH = case_when(
    W5Hous12CHH == -999 ~ -2,
    W5Hous12CHH == -92 ~ -9,
    W5Hous12CHH == -91 ~ -1,
    W5Hous12CHH == -1 ~ -8,
    TRUE ~ W5Hous12CHH
  )) %>%
  mutate(hownteen18 = case_when(
    W5Hous12HH == 1 & W5Hous12BHH == 1 ~ 1,
    W5Hous12HH == 1 & W5Hous12BHH == 2 ~ 2,
    W5Hous12HH == 1 & W5Hous12BHH == 3 ~ 3,
    W5Hous12HH == 1 & W5Hous12BHH == 4 ~ 8,
    W5Hous12HH == 2 & W5Hous12CHH == 1 ~ 4,
    W5Hous12HH == 2 & W5Hous12CHH == 2 ~ 5,
    W5Hous12HH == 2 & W5Hous12CHH == 3 ~ 6,
    W5Hous12HH == 2 & W5Hous12CHH == 4 ~ 7,
    W5Hous12HH == 2 & W5Hous12CHH == 5 ~ 8,
    W5Hous12HH == 3 ~ 8,
    W5Hous12HH == -3 ~ -3,
    W5Hous12HH == -9 ~ -9,
    W5Hous12HH == -1 ~ -1,
    W5Hous12HH == -8 ~ -8,
    W5Hous12HH == -2 ~ -2,
    TRUE ~ -3
  )) %>%
  mutate(hown18 = case_when(
    hownteen18 == 1 ~ 1,
    hownteen18 == 2 ~ 2,
    hownteen18 == 3 ~ 3,
    hownteen18 %in% c(4, 5, 6) ~ 4,
    hownteen18 == 7 ~ 5,
    hownteen18 == 8 ~ 6,
    TRUE ~ hownteen18
  ))

# Wave 6 (Age 19)
data_merged <- data_merged %>%
  mutate(W6Hous12YP = case_when(
    W6Hous12YP == -92 ~ -9,
    W6Hous12YP == -91 ~ -1,
    W6Hous12YP == -1 ~ -8,
    TRUE ~ W6Hous12YP
  )) %>%
  mutate(W6Hous12bYP = case_when(
    W6Hous12bYP == -92 ~ -9,
    W6Hous12bYP == -91 ~ -1,
    W6Hous12bYP == -1 ~ -8,
    TRUE ~ W6Hous12bYP
  )) %>%
  mutate(W6Hous12cYP = case_when(
    W6Hous12cYP == -92 ~ -9,
    W6Hous12cYP == -91 ~ -1,
    W6Hous12cYP == -1 ~ -8,
    TRUE ~ W6Hous12cYP
  )) %>%
  mutate(hownteen19 = case_when(
    W6Hous12YP == 1 & W6Hous12bYP == 1 ~ 1,
    W6Hous12YP == 1 & W6Hous12bYP == 2 ~ 2,
    W6Hous12YP == 1 & W6Hous12bYP == 3 ~ 3,
    W6Hous12YP == 1 & W6Hous12bYP == 4 ~ 8,
    W6Hous12YP == 2 & W6Hous12cYP == 1 ~ 4,
    W6Hous12YP == 2 & W6Hous12cYP == 2 ~ 5,
    W6Hous12YP == 2 & W6Hous12cYP == 3 ~ 6,
    W6Hous12YP == 2 & W6Hous12cYP == 4 ~ 7,
    W6Hous12YP == 2 & W6Hous12cYP == 5 ~ 8,
    W6Hous12YP == 3 ~ 8,
    W6Hous12YP == -9 ~ -9,
    W6Hous12YP == -1 ~ -1,
    W6Hous12YP == -8 ~ -8,
    TRUE ~ -3
  )) %>%
  mutate(hown19 = case_when(
    hownteen19 == 1 ~ 1,
    hownteen19 == 2 ~ 2,
    hownteen19 == 3 ~ 3,
    hownteen19 %in% c(4, 5, 6) ~ 4,
    hownteen19 == 7 ~ 5,
    hownteen19 == 8 ~ 6,
    TRUE ~ hownteen19
  ))

# Wave 7 (Age 20)
data_merged <- data_merged %>%
  mutate(W7Hous12YP = case_when(
    W7Hous12YP == -92 ~ -9,
    W7Hous12YP == -91 ~ -1,
    W7Hous12YP == -1 ~ -8,
    TRUE ~ W7Hous12YP
  )) %>%
  mutate(W7Hous12bYP = case_when(
    W7Hous12bYP == -92 ~ -9,
    W7Hous12bYP == -91 ~ -1,
    W7Hous12bYP == -1 ~ -8,
    TRUE ~ W7Hous12bYP
  )) %>%
  mutate(W7Hous12cYP = case_when(
    W7Hous12cYP == -92 ~ -9,
    W7Hous12cYP == -91 ~ -1,
    W7Hous12cYP == -1 ~ -8,
    TRUE ~ W7Hous12cYP
  )) %>%
  mutate(hownteen20 = case_when(
    W7Hous12YP == 1 & W7Hous12bYP == 1 ~ 1,
    W7Hous12YP == 1 & W7Hous12bYP == 2 ~ 2,
    W7Hous12YP == 1 & W7Hous12bYP == 3 ~ 3,
    W7Hous12YP == 1 & W7Hous12bYP == 4 ~ 8,
    W7Hous12YP == 2 & W7Hous12cYP == 1 ~ 4,
    W7Hous12YP == 2 & W7Hous12cYP == 2 ~ 5,
    W7Hous12YP == 2 & W7Hous12cYP == 3 ~ 6,
    W7Hous12YP == 2 & W7Hous12cYP == 4 ~ 7,
    W7Hous12YP == 2 & W7Hous12cYP == 5 ~ 8,
    W7Hous12YP == 3 ~ 8,
    W7Hous12YP == -9 ~ -9,
    W7Hous12YP == -1 ~ -1,
    W7Hous12YP == -8 ~ -8,
    TRUE ~ -3
  )) %>%
  mutate(hown20 = case_when(
    hownteen20 == 1 ~ 1,
    hownteen20 == 2 ~ 2,
    hownteen20 == 3 ~ 3,
    hownteen20 %in% c(4, 5, 6) ~ 4,
    hownteen20 == 7 ~ 5,
    hownteen20 == 8 ~ 6,
    TRUE ~ hownteen20
  ))

# Wave 8 (Age 25)
data_merged <- data_merged %>%
  mutate(W8TENURE = case_when(
    W8TENURE == -9 ~ -9,
    W8TENURE == -8 ~ -8,
    W8TENURE == -1 ~ -1,
    TRUE ~ W8TENURE
  )) %>%
  mutate(hown25 = case_when(
    W8TENURE == 1 ~ 1,
    W8TENURE == 2 ~ 2,
    W8TENURE == 3 ~ 3,
    W8TENURE == 4 ~ 4,
    W8TENURE == 5 ~ 5,
    W8TENURE == 6 ~ 6,
    W8TENURE == 7 ~ 6,
    TRUE ~ W8TENURE
  ))

# Wave 9 (Age 32)
data_merged <- data_merged %>%
  mutate(W9DTENURE = case_when(
    W9DTENURE == -8 ~ -8,
    TRUE ~ W9DTENURE
  )) %>%
  mutate(hown32 = case_when(
    W9DTENURE == 1 ~ 1,
    W9DTENURE == 2 ~ 2,
    W9DTENURE == 3 ~ 3,
    W9DTENURE == 4 ~ 4,
    W9DTENURE == 5 ~ 5,
    W9DTENURE == 6 ~ 6,
    W9DTENURE == 7 ~ 6,
    TRUE ~ W9DTENURE
  ))

cleaned_data <- data_merged %>%
  select(NSID, 
         hown14, hownteen14,
         hown15, hownteen15,
         hown16, hownteen16,
         hown17, hownteen17,
         hown18, hownteen18,
         hown19, hownteen19,
         hown20, hownteen20,
         hown25, hown32)

write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)
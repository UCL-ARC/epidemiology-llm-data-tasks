library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Read all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Process wave2
wave2_clean <- wave2 %>%
  select(NSID, urbind, gor) %>%
  rename(regub15 = urbind, regov15 = gor) %>%
  mutate(
    regub15 = case_when(
      regub15 == -94 ~ -8,
      regub15 < 0 ~ -3,
      is.na(regub15) ~ -3,
      TRUE ~ regub15
    ),
    regov15 = case_when(
      regov15 == -94 ~ -8,
      regov15 < 0 ~ -3,
      is.na(regov15) ~ -3,
      TRUE ~ regov15
    )
  ) %>%
  mutate(
    regub15 = labelled::labelled(regub15, labels = c(
      "Urban >= 10k - sparse" = 1,
      "Town & Fringe - sparse" = 2,
      "Village - sparse" = 3,
      "Hamlet and Isolated Dwelling - sparse" = 4,
      "Urban >= 10k - less sparse" = 5,
      "Town & Fringe - less sparse" = 6,
      "Village - less sparse" = 7,
      "Hamlet & Isolated Dwelling" = 8,
      "Don't know" = -8,
      "Not asked" = -3,
      "Not applicable" = -1,
      "Refused" = -9
    )),
    regov15 = labelled::labelled(regov15, labels = c(
      "North East" = 1,
      "North West" = 2,
      "Yorkshire and The Humber" = 3,
      "East Midlands" = 4,
      "West Midlands" = 5,
      "East of England" = 6,
      "London" = 7,
      "South East" = 8,
      "South West" = 9,
      "Don't know" = -8,
      "Not asked" = -3,
      "Not applicable" = -1,
      "Refused" = -9
    ))
  )

# Process wave3
wave3_clean <- wave3 %>%
  select(NSID, urbind, gor) %>%
  rename(regub16 = urbind, regov16 = gor) %>%
  mutate(
    regub16 = case_when(
      regub16 == -94 ~ -8,
      regub16 < 0 ~ -3,
      is.na(regub16) ~ -3,
      TRUE ~ regub16
    ),
    regov16 = case_when(
      regov16 == -94 ~ -8,
      regov16 < 0 ~ -3,
      is.na(regov16) ~ -3,
      TRUE ~ regov16
    )
  ) %>%
  mutate(
    regub16 = labelled::labelled(regub16, labels = c(
      "Urban >= 10k - sparse" = 1,
      "Town & Fringe - sparse" = 2,
      "Village - sparse" = 3,
      "Hamlet and Isolated Dwelling - sparse" = 4,
      "Urban >= 10k - less sparse" = 5,
      "Town & Fringe - less sparse" = 6,
      "Village - less sparse" = 7,
      "Hamlet & Isolated Dwelling" = 8,
      "Don't know" = -8,
      "Not asked" = -3,
      "Not applicable" = -1,
      "Refused" = -9
    )),
    regov16 = labelled::labelled(regov16, labels = c(
      "North East" = 1,
      "North West" = 2,
      "Yorkshire and the Humber" = 3,
      "East Midlands" = 4,
      "West Midlands" = 5,
      "East of England" = 6,
      "London" = 7,
      "South East" = 8,
      "South West" = 9,
      "Don't know" = -8,
      "Not asked" = -3,
      "Not applicable" = -1,
      "Refused" = -9
    ))
  )

# Process ns8
ns8_clean <- ns8 %>%
  select(NSID, W8DGOR) %>%
  rename(regov24 = W8DGOR) %>%
  mutate(
    regov24 = case_when(
      regov24 == -9 ~ -9,
      regov24 == -8 ~ -8,
      regov24 == -1 ~ -1,
      regov24 < 0 ~ -3,
      is.na(regov24) ~ -3,
      TRUE ~ regov24
    )
  ) %>%
  mutate(
    regov24 = labelled::labelled(regov24, labels = c(
      "North East" = 1,
      "North West" = 2,
      "Yorkshire and the Humber" = 3,
      "East Midlands" = 4,
      "West Midlands" = 5,
      "East of England" = 6,
      "London" = 7,
      "South East" = 8,
      "South West" = 9,
      "Wales" = 10,
      "Scotland" = 11,
      "Northern Ireland" = 12,
      "Unknown due to faulty/missing postcode" = 13,
      "Refused" = -9,
      "Don't know" = -8,
      "Not asked" = -3,
      "Not applicable" = -1
    ))
  )

# Process ns9_derived
ns9_derived_clean <- ns9_derived %>%
  select(NSID, W9DRGN) %>%
  rename(regov32 = W9DRGN) %>%
  mutate(
    regov32 = case_when(
      regov32 == -9 ~ -9,
      regov32 == -8 ~ -8,
      regov32 == -1 ~ -1,
      regov32 < 0 ~ -3,
      is.na(regov32) ~ -3,
      TRUE ~ regov32
    )
  ) %>%
  mutate(
    regov32 = labelled::labelled(regov32, labels = c(
      "North East" = 1,
      "North West" = 2,
      "Yorkshire and the Humber" = 3,
      "East Midlands" = 4,
      "West Midlands" = 5,
      "East of England" = 6,
      "London" = 7,
      "South East" = 8,
      "South West" = 9,
      "Wales" = 10,
      "Scotland" = 11,
      "Northern Ireland" = 12,
      "Unknown due to faulty/missing postcode" = 13,
      "Refused" = -9,
      "Don't know" = -8,
      "Not asked" = -3,
      "Not applicable" = -1
    ))
  )

# Process ns9_main
ns9_main_clean <- ns9_main %>%
  select(NSID, W9NATIONRES) %>%
  rename(regint32 = W9NATIONRES) %>%
  mutate(
    regint32 = case_when(
      regint32 == -9 ~ -9,
      regint32 == -8 ~ -8,
      regint32 == -3 ~ -3,
      regint32 == -1 ~ -1,
      regint32 < 0 ~ -3,
      is.na(regint32) ~ -3,
      TRUE ~ regint32
    )
  ) %>%
  mutate(
    regint32 = labelled::labelled(regint32, labels = c(
      "England" = 1,
      "Scotland" = 2,
      "Wales" = 3,
      "Northern Ireland" = 4,
      "Outside of UK or unknown" = 5,
      "Refused" = -9,
      "Don't know" = -8,
      "Not asked" = -3,
      "Not applicable" = -1
    ))
  )

# Wave1 and wave4 only have NSID, so just select NSID
wave1_clean <- wave1 %>% select(NSID)
wave4_clean <- wave4 %>% select(NSID)

# Merge all datasets
all_data <- wave1_clean %>%
  full_join(wave2_clean, by = "NSID") %>%
  full_join(wave3_clean, by = "NSID") %>%
  full_join(wave4_clean, by = "NSID") %>%
  full_join(ns8_clean, by = "NSID") %>%
  full_join(ns9_derived_clean, by = "NSID") %>%
  full_join(ns9_main_clean, by = "NSID")

# Write output
write_csv(all_data, "data/output/cleaned_data.csv")
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load only required columns to save memory
# We only need NSID and the specific marital status variables from the metadata
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_select = all_of("NSID"))
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_select = all_of("NSID"))
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", col_select = any_of(c("NSID", "W6MarStatYP")))
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_select = any_of(c("NSID", "W8DMARSTAT")))
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_select = any_of(c("NSID", "W9DMARSTAT")))

# Ensure NSID is unique in each wave to prevent many-to-many explosion
wave1 <- wave1 %>% distinct(NSID, .keep_all = TRUE)
wave4 <- wave4 %>% distinct(NSID, .keep_all = TRUE)
wave6 <- wave6 %>% distinct(NSID, .keep_all = TRUE)
wave8 <- wave8 %>% distinct(NSID, .keep_all = TRUE)
wave9 <- wave9 %>% distinct(NSID, .keep_all = TRUE)

# Merge datasets
data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Process Marital Status (Time-varying)
# Common categories: 1=Single, 2=Married, 3=Separated, 4=Divorced, 5=Widowed

# Wave 6 (Age 19)
data <- data %>%
  mutate(
    partnr19_collapsed = case_when(
      W6MarStatYP == 1 ~ 1,
      W6MarStatYP == 2 ~ 2,
      W6MarStatYP == 3 ~ 3,
      W6MarStatYP == 4 ~ 4,
      W6MarStatYP == 5 ~ 5,
      W6MarStatYP == -997 ~ -2,
      W6MarStatYP == -97 ~ -7,
      W6MarStatYP == -92 ~ -9,
      W6MarStatYP == -91 ~ -1,
      W6MarStatYP == -1 ~ -8,
      is.na(W6MarStatYP) ~ -3,
      TRUE ~ -3
    ),
    partnr19_adu = case_when(
      W6MarStatYP == 1 ~ 1,
      W6MarStatYP == 2 ~ 2,
      W6MarStatYP == 3 ~ 3,
      W6MarStatYP == 4 ~ 4,
      W6MarStatYP == 5 ~ 5,
      W6MarStatYP == -997 ~ -2,
      W6MarStatYP == -97 ~ -7,
      W6MarStatYP == -92 ~ -9,
      W6MarStatYP == -91 ~ -1,
      W6MarStatYP == -1 ~ -8,
      is.na(W6MarStatYP) ~ -3,
      TRUE ~ -3
    )
  )

# Wave 8
data <- data %>%
  mutate(
    partnradu_w8 = W8DMARSTAT,
    partnr_collapsed_w8 = case_when(
      W8DMARSTAT == 1 ~ 1,
      W8DMARSTAT == 2 ~ 2,
      W8DMARSTAT == 3 ~ 3,
      W8DMARSTAT == 7 ~ 3, 
      W8DMARSTAT == 4 ~ 4,
      W8DMARSTAT == 8 ~ 4, 
      W8DMARSTAT == 5 ~ 5,
      W8DMARSTAT == 9 ~ 5, 
      W8DMARSTAT == 6 ~ 2, 
      W8DMARSTAT == -9 ~ -9,
      W8DMARSTAT == -8 ~ -8,
      W8DMARSTAT == -1 ~ -1,
      is.na(W8DMARSTAT) ~ -3,
      TRUE ~ -3
    )
  )

# Wave 9
data <- data %>%
  mutate(
    partnradu_w9 = W9DMARSTAT,
    partnr_collapsed_w9 = case_when(
      W9DMARSTAT == 1 ~ 1,
      W9DMARSTAT == 2 ~ 2,
      W9DMARSTAT == 4 ~ 3, 
      W9DMARSTAT == 3 ~ 4, 
      W9DMARSTAT == 7 ~ 4, 
      W9DMARSTAT == 5 ~ 5,
      W9DMARSTAT == 8 ~ 5, 
      W9DMARSTAT == 6 ~ 2, 
      W9DMARSTAT == -9 ~ -9,
      W9DMARSTAT == -8 ~ -8,
      is.na(W9DMARSTAT) ~ -3,
      TRUE ~ -3
    )
  )

# Selection of derived variables
final_data <- data %>%
  select(NSID, partnr19_collapsed, partnr19_adu, partnr_collapsed_w8, partnradu_w8, partnr_collapsed_w9, partnradu_w9)

# Convert to factors
final_data <- final_data %>%
  mutate(across(-NSID, as.factor))

write_csv(final_data, "data/output/cleaned_data.csv")
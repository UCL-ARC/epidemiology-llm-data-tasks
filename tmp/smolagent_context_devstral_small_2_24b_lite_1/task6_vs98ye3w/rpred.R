library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID") %>%
  full_join(wave9_main, by = "NSID")

# Process wave2 variables (age 15)
merged_data <- merged_data %>%
  mutate(
    regub15 = case_when(
      urbind.y %in% c(1:8) ~ urbind.y,
      urbind.y == -94 ~ -8,
      TRUE ~ -3
    ),
    regov15 = case_when(
      gor.y %in% c(1:9) ~ gor.y,
      gor.y == -94 ~ -8,
      TRUE ~ -3
    )
  )

# Process wave3 variables (age 16)
merged_data <- merged_data %>%
  mutate(
    regub16 = case_when(
      urbind.x %in% c(1:8) ~ urbind.x,
      urbind.x == -94 ~ -8,
      TRUE ~ -3
    ),
    regov16 = case_when(
      gor.x %in% c(1:9) ~ gor.x,
      gor.x == -94 ~ -8,
      TRUE ~ -3
    )
  )

# Process wave8 variables (age 25)
merged_data <- merged_data %>%
  mutate(
    regor25 = case_when(
      W8DGOR %in% c(1:13) ~ W8DGOR,
      W8DGOR == -9 ~ -9,
      W8DGOR == -8 ~ -8,
      W8DGOR == -1 ~ -1,
      TRUE ~ -3
    )
  )

# Process wave9 variables (age 32)
merged_data <- merged_data %>%
  mutate(
    regor32 = case_when(
      W9DRGN %in% c(1:13) ~ W9DRGN,
      W9DRGN == -9 ~ -9,
      W9DRGN == -8 ~ -8,
      W9DRGN == -1 ~ -1,
      TRUE ~ -3
    ),
    regint32 = case_when(
      W9NATIONRES %in% c(1:5) ~ W9NATIONRES,
      W9NATIONRES == -9 ~ -9,
      W9NATIONRES == -8 ~ -8,
      W9NATIONRES == -3 ~ -3,
      W9NATIONRES == -1 ~ -1,
      TRUE ~ -3
    )
  )

# Select only the required variables
output_data <- merged_data %>%
  select(NSID, regub15, regov15, regub16, regov16, regor25, regor32, regint32)

# Write the output file
write_csv(output_data, "data/output/cleaned_data.csv")
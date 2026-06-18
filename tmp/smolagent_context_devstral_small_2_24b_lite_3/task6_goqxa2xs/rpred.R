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

# Function to standardize missing values
standardize_missing <- function(var, metadata) {
  if (is.numeric(var)) {
    var <- case_when(
      var %in% c(-999, -998, -997, -995) ~ -2,
      var %in% c(-94) ~ -8,
      var %in% c(-92) ~ -9,
      var %in% c(-91) ~ -1,
      var %in% c(-99) ~ -3,
      TRUE ~ var
    )
  }
  return(var)
}

# Process urban/rural indicator for wave2 and wave3
merged_data <- merged_data %>% 
  mutate(
    regub15 = case_when(
      !is.na(urbind.x) & urbind.x >= 1 & urbind.x <= 8 ~ urbind.x,
      urbind.x == -94 ~ -8,
      urbind.x %in% c(-999, -998, -997, -995) ~ -2,
      urbind.x %in% c(-92) ~ -9,
      urbind.x %in% c(-91) ~ -1,
      urbind.x %in% c(-99) ~ -3,
      TRUE ~ -3
    ),
    regub16 = case_when(
      !is.na(urbind.y) & urbind.y >= 1 & urbind.y <= 8 ~ urbind.y,
      urbind.y == -94 ~ -8,
      urbind.y %in% c(-999, -998, -997, -995) ~ -2,
      urbind.y %in% c(-92) ~ -9,
      urbind.y %in% c(-91) ~ -1,
      urbind.y %in% c(-99) ~ -3,
      TRUE ~ -3
    )
  )

# Process Government Office Region for wave2 and wave3
merged_data <- merged_data %>% 
  mutate(
    regov15 = case_when(
      !is.na(gor.x) & gor.x >= 1 & gor.x <= 9 ~ gor.x,
      gor.x == -94 ~ -8,
      gor.x %in% c(-999, -998, -997, -995) ~ -2,
      gor.x %in% c(-92) ~ -9,
      gor.x %in% c(-91) ~ -1,
      gor.x %in% c(-99) ~ -3,
      TRUE ~ -3
    ),
    regov16 = case_when(
      !is.na(gor.y) & gor.y >= 1 & gor.y <= 9 ~ gor.y,
      gor.y == -94 ~ -8,
      gor.y %in% c(-999, -998, -997, -995) ~ -2,
      gor.y %in% c(-92) ~ -9,
      gor.y %in% c(-91) ~ -1,
      gor.y %in% c(-99) ~ -3,
      TRUE ~ -3
    )
  )

# Process Government Office Region for wave8
merged_data <- merged_data %>% 
  mutate(
    regor25 = case_when(
      !is.na(W8DGOR) & W8DGOR >= 1 & W8DGOR <= 13 ~ W8DGOR,
      W8DGOR == -9 ~ -9,
      W8DGOR == -8 ~ -8,
      W8DGOR == -1 ~ -1,
      TRUE ~ -3
    )
  )

# Process Government Office Region for wave9
merged_data <- merged_data %>% 
  mutate(
    regor32 = case_when(
      !is.na(W9DRGN) & W9DRGN >= 1 & W9DRGN <= 13 ~ W9DRGN,
      W9DRGN == -9 ~ -9,
      W9DRGN == -8 ~ -8,
      W9DRGN == -1 ~ -1,
      TRUE ~ -3
    )
  )

# Process Nation of UK for wave9
merged_data <- merged_data %>% 
  mutate(
    regint32 = case_when(
      !is.na(W9NATIONRES) & W9NATIONRES >= 1 & W9NATIONRES <= 5 ~ W9NATIONRES,
      W9NATIONRES == -9 ~ -9,
      W9NATIONRES == -8 ~ -8,
      W9NATIONRES == -3 ~ -3,
      W9NATIONRES == -1 ~ -1,
      TRUE ~ -3
    )
  )

# Select only the required variables
cleaned_data <- merged_data %>% 
  select(NSID, regub15, regub16, regov15, regov16, regor25, regor32, regint32)

# Write the cleaned data to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return the path to the cleaned data
"data/output/cleaned_data.csv"
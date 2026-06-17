library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# Load each file
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets by NSID
full_df <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Harmonise missing codes for each ethnicity variable
full_df <- full_df %>%
  mutate(
    eth_w1 = case_when(
      W1ethnic2YP == -999 ~ -2,
      W1ethnic2YP == -94  ~ -8,
      W1ethnic2YP == -92  ~ -9,
      W1ethnic2YP == -91  ~ -1,
      W1ethnic2YP == -1   ~ -8,
      TRUE ~ W1ethnic2YP
    ),
    eth_w2 = case_when(
      W2ethnicYP == -998 ~ -2,
      W2ethnicYP == -997 ~ -2,
      W2ethnicYP == -995 ~ -2,
      W2ethnicYP == -99  ~ -3,
      W2ethnicYP == -92  ~ -9,
      W2ethnicYP == -91  ~ -1,
      W2ethnicYP == -1   ~ -8,
      TRUE ~ W2ethnicYP
    ),
    eth_w4 = case_when(
      w4ethnic2YP == -94 ~ -8,
      w4ethnic2YP == -1  ~ -8,
      TRUE ~ w4ethnic2YP
    ),
    eth_w8 = case_when(
      W8DETHN15 == -9  ~ -9,
      W8DETHN15 == -8  ~ -8,
      W8DETHN15 == -1  ~ -1,
      TRUE ~ W8DETHN15
    ),
    eth_w9 = case_when(
      W9DETHN15 == -8 ~ -8,
      TRUE ~ W9DETHN15
    )
  )

# Derive consolidated ethnicity variable using earliest valid response
full_df <- full_df %>%
  mutate(
    eth = coalesce(eth_w1, eth_w2, eth_w4, eth_w8, eth_w9),
    eth = ifelse(is.na(eth), -3, eth)
  )

# Keep only NSID and eth
output_df <- full_df %>% select(NSID, eth)

# Write cleaned data
write_csv(output_df, "data/output/cleaned_data.csv")

library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

eth14 <- wave1 %>% select(NSID, W1ethnic2YP) %>% rename(eth14 = W1ethnic2YP) %>% mutate(eth14 = case_when(
  eth14 == -999 ~ -3,
  eth14 == -94 ~ -8,
  eth14 == -92 ~ -9,
  eth14 == -91 ~ -1,
  eth14 == -1 ~ -8,
  TRUE ~ eth14
))

eth15 <- wave2 %>% select(NSID, W2ethnicYP) %>% rename(eth15 = W2ethnicYP) %>% mutate(eth15 = case_when(
  eth15 == -998 ~ -3,
  eth15 == -997 ~ -3,
  eth15 == -995 ~ -3,
  eth15 == -99 ~ -3,
  eth15 == -92 ~ -9,
  eth15 == -91 ~ -1,
  eth15 == -1 ~ -8,
  TRUE ~ eth15
))

eth17 <- wave4 %>% select(NSID, w4ethnic2YP) %>% rename(eth17 = w4ethnic2YP) %>% mutate(eth17 = case_when(
  eth17 == -94 ~ -8,
  eth17 == -1 ~ -8,
  eth17 < 0 ~ -3,
  TRUE ~ eth17
))

eth28 <- ns8 %>% select(NSID, W8DETHN15) %>% rename(eth28 = W8DETHN15) %>% mutate(eth28 = case_when(
  eth28 == -9 ~ -9,
  eth28 == -8 ~ -8,
  eth28 == -1 ~ -1,
  TRUE ~ eth28
))

eth32 <- ns9 %>% select(NSID, W9DETHN15) %>% rename(eth32 = W9DETHN15) %>% mutate(eth32 = case_when(
  eth32 == -8 ~ -8,
  TRUE ~ eth32
))

eth_data <- full_join(eth14, eth15, by = "NSID") %>%
  full_join(eth17, by = "NSID") %>%
  full_join(eth28, by = "NSID") %>%
  full_join(eth32, by = "NSID")

eth_data <- eth_data %>% mutate(eth = coalesce(
  ifelse(eth14 >= 1 & eth14 <= 16, eth14, NA),
  ifelse(eth15 >= 1 & eth15 <= 16, eth15, NA),
  ifelse(eth17 >= 1 & eth17 <= 16, eth17, NA),
  ifelse(eth28 >= 1 & eth28 <= 16, eth28, NA),
  ifelse(eth32 >= 1 & eth32 <= 16, eth32, NA)
)) %>% select(NSID, eth)

write_csv(eth_data, "data/output/cleaned_data.csv")
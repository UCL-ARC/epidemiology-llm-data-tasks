library(readr)
library(dplyr)

# Define file paths
file_paths <- list(
  w1 = "data/input/wave_one_lsype_young_person_2020.tab",
  w2 = "data/input/wave_two_lsype_young_person_2020.tab",
  w4 = "data/input/wave_four_lsype_young_person_2020.tab",
  w8 = "data/input/ns8_2015_derived.tab",
  w9 = "data/input/ns9_2022_derived_variables.tab"
)

# Load datasets
w1 <- read_delim(file_paths$w1, delim = "\t", col_types = cols())
w2 <- read_delim(file_paths$w2, delim = "\t", col_types = cols())
w4 <- read_delim(file_paths$w4, delim = "\t", col_types = cols())
w8 <- read_delim(file_paths$w8, delim = "\t", col_types = cols())
w9 <- read_delim(file_paths$w9, delim = "\t", col_types = cols())

# Clean ethnicity variables (keep only categories 1-16, others become NA)
w1_clean <- w1 %>% select(NSID, eth_w1 = W1ethnic2YP) %>%
  mutate(eth_w1 = ifelse(eth_w1 %in% 1:16, eth_w1, NA_real_))

w2_clean <- w2 %>% select(NSID, eth_w2 = W2ethnicYP) %>%
  mutate(eth_w2 = ifelse(eth_w2 %in% 1:16, eth_w2, NA_real_))

w4_clean <- w4 %>% select(NSID, eth_w4 = w4ethnic2YP) %>%
  mutate(eth_w4 = ifelse(eth_w4 %in% 1:16, eth_w4, NA_real_))

w8_clean <- w8 %>% select(NSID, eth_w8 = W8DETHN15) %>%
  mutate(eth_w8 = ifelse(eth_w8 %in% 1:16, eth_w8, NA_real_))

w9_clean <- w9 %>% select(NSID, eth_w9 = W9DETHN15) %>%
  mutate(eth_w9 = ifelse(eth_w9 %in% 1:16, eth_w9, NA_real_))

# Merge all datasets
merged <- w1_clean %>%
  full_join(w2_clean, by = "NSID") %>%
  full_join(w4_clean, by = "NSID") %>%
  full_join(w8_clean, by = "NSID") %>%
  full_join(w9_clean, by = "NSID")

# Consolidated ethnicity variable (earliest valid, then -3 if no valid)
merged <- merged %>%
  mutate(eth = coalesce(eth_w1, eth_w2, eth_w4, eth_w8, eth_w9)) %>%
  mutate(eth = ifelse(is.na(eth), -3, eth))

# Write cleaned data
write_csv(merged %>% select(NSID, eth), "data/output/cleaned_data.csv")

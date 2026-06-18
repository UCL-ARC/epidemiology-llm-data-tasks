library(readr)
library(dplyr)
library(haven)

# Load all files from the metadata
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Extract IMDRSCORE from wave2 and wave3 before merging
imd15 <- wave2 %>%
  select(NSID, IMDRSCORE) %>%
  mutate(imd15 = ifelse(IMDRSCORE == -94, -8, IMDRSCORE)) %>%
  select(NSID, imd15)

imd16 <- wave3 %>%
  select(NSID, IMDRSCORE) %>%
  mutate(imd16 = ifelse(IMDRSCORE == -94, -8, IMDRSCORE)) %>%
  select(NSID, imd16)

# Extract W9DIMDD from wave9
imd32 <- wave9 %>%
  select(NSID, W9DIMDD) %>%
  mutate(imd32 = ifelse(W9DIMDD == -8, -8, W9DIMDD)) %>%
  select(NSID, imd32)

# Merge the IMD variables into a single dataset
cleaned_data <- imd15 %>%
  full_join(imd16, by = "NSID") %>%
  full_join(imd32, by = "NSID")

# Write the cleaned data to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")
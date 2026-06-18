library(readr)
library(dplyr)

# Load all datasets from the metadata
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Rename IMDRSCORE columns to avoid conflicts during merge
wave2 <- wave2 %>% rename(IMDRSCORE_wave2 = IMDRSCORE)
wave3 <- wave3 %>% rename(IMDRSCORE_wave3 = IMDRSCORE)

# Merge datasets using NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Derive IMD variables
# For imd15 (Age 15), use IMDRSCORE from wave2
merged_data <- merged_data %>%
  mutate(imd15 = ifelse(IMDRSCORE_wave2 == -94, -8, IMDRSCORE_wave2))

# For imd16 (Age 16), use IMDRSCORE from wave3
merged_data <- merged_data %>%
  mutate(imd16 = ifelse(IMDRSCORE_wave3 == -94, -8, IMDRSCORE_wave3))

# For imd32 (Age 32), use W9DIMDD from wave9
# Convert decile to a continuous score by scaling
merged_data <- merged_data %>%
  mutate(imd32 = case_when(
    W9DIMDD == -8 ~ -8,
    TRUE ~ (W9DIMDD - 0.5) * 10
  ))

# Select only the ID variable and the derived IMD variables
output_data <- merged_data %>%
  select(NSID, imd15, imd16, imd32)

# Write the output to a CSV file
write_csv(output_data, "data/output/cleaned_data.csv")
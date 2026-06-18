library(readr)
library(dplyr)
library(haven)

# Load datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_nine <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Handle missing values for IMDRSCORE in wave_two and wave_three
# Replace -94 with -8 (Insufficient Information)
merged_data <- merged_data %>%
  mutate(
    IMDRSCORE.x = ifelse(IMDRSCORE.x == -94, -8, IMDRSCORE.x),
    IMDRSCORE.y = ifelse(IMDRSCORE.y == -94, -8, IMDRSCORE.y)
  )

# Replace other negative values with -3 (Not asked at the fieldwork stage / not interviewed)
merged_data <- merged_data %>%
  mutate(
    IMDRSCORE.x = ifelse(IMDRSCORE.x < -1 & IMDRSCORE.x != -8, -3, IMDRSCORE.x),
    IMDRSCORE.y = ifelse(IMDRSCORE.y < -1 & IMDRSCORE.y != -8, -3, IMDRSCORE.y)
  )

# Handle missing values for W9DIMDD in wave_nine
# Replace -8 with -8 (Insufficient Information)
merged_data <- merged_data %>%
  mutate(
    W9DIMDD = ifelse(W9DIMDD == -8, -8, W9DIMDD)
  )

# Replace other negative values with -3 (Not asked at the fieldwork stage / not interviewed)
merged_data <- merged_data %>%
  mutate(
    W9DIMDD = ifelse(W9DIMDD < -1 & W9DIMDD != -8, -3, W9DIMDD)
  )

# Rename variables to match the required output structure
cleaned_data <- merged_data %>%
  select(
    NSID,
    imd15 = IMDRSCORE.x,
    imd16 = IMDRSCORE.y,
    imd32 = W9DIMDD
  )

# Write the cleaned data to a CSV file
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return the path to the cleaned data file
"data/output/cleaned_data.csv"
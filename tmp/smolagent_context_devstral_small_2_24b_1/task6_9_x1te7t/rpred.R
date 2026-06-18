library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave_nine_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
wave_nine_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine_derived, by = "NSID") %>%
  full_join(wave_nine_main, by = "NSID")

# Define missing value mapping function
map_missing <- function(x, label_meaning = NULL) {
  if (is.numeric(x)) {
    x <- ifelse(x == -94, -8, x)
    x <- ifelse(x == -92, -9, x)
    x <- ifelse(x == -91, -1, x)
    x <- ifelse(x == -99, -3, x)
    x <- ifelse(x == -100 | x == -97, -2, x)
    x <- ifelse(x == -999 | x == -998 | x == -997 | x == -995, -2, x)
    x <- ifelse(x == 13, -2, x)  # Specific mapping for value 13
    x <- ifelse(is.na(x), -3, x)
  }
  x
}

# Derive regub15 and regov15 from wave 2 (age 15)
merged_data <- merged_data %>%
  mutate(
    regub15 = map_missing(urbind.x),
    regov15 = map_missing(gor.x)
  )

# Derive regub16 and regov16 from wave 3 (age 16)
merged_data <- merged_data %>%
  mutate(
    regub16 = map_missing(urbind.y),
    regov16 = map_missing(gor.y)
  )

# Derive regor25 from wave 8 (age 25)
merged_data <- merged_data %>%
  mutate(
    regor25 = map_missing(W8DGOR)
  )

# Derive regor32 from wave 9 (age 32)
merged_data <- merged_data %>%
  mutate(
    regor32 = map_missing(W9DRGN)
  )

# Derive regint32 from wave 9 (age 32)
merged_data <- merged_data %>%
  mutate(
    regint32 = case_when(
      W9NATIONRES %in% c(1, 2, 3, 4) ~ 1,
      W9NATIONRES %in% c(5, -9, -8, -3, -1, -2) ~ 2,
      TRUE ~ -3
    )
  )

# Select only the ID and derived variables
cleaned_data <- merged_data %>%
  select(NSID, regub15, regov15, regub16, regov16, regor25, regor32, regint32)

# Write the cleaned data to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return the path to the cleaned data
"data/output/cleaned_data.csv"
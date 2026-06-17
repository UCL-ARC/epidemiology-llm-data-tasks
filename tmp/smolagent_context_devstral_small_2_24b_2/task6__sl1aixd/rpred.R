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
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets using NSID
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID")

# Define missing value mappings
map_missing <- function(x, na_values = c(-999, -998, -997, -995, -94, -92, -91, -99, -100, -97)) {
  if (is.numeric(x)) {
    x[is.na(x)] <- -3
    x[x %in% na_values] <- -3
    x[x == -92] <- -9
    x[x == -91] <- -1
    x[x == -99] <- -3
    x[x == -94] <- -8
    x[x == -100 | x == -97] <- -7
    x[x == -999 | x == -998 | x == -997 | x == -995] <- -2
  }
  return(x)
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
      W9NATIONRES %in% c(5, -3, -1, -8, -9) ~ 2,
      TRUE ~ -3
    )
  )

# Select only the required variables
output_data <- merged_data %>%
  select(NSID, regub15, regov15, regub16, regov16, regor25, regor32, regint32)

# Write the output file
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"
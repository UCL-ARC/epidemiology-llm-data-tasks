library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all required datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave_nine_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
wave_nine_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets using NSID
merged_data <- wave_one %>% 
  full_join(wave_two, by = "NSID") %>% 
  full_join(wave_three, by = "NSID") %>% 
  full_join(wave_four, by = "NSID") %>% 
  full_join(wave_eight, by = "NSID") %>% 
  full_join(wave_nine_derived, by = "NSID") %>% 
  full_join(wave_nine_main, by = "NSID")

# Define missing value mapping function
map_missing <- function(x, wave = NULL) {
  if (is.numeric(x)) {
    case_when(
      x == -94 ~ -8,  # Insufficient information
      x == -92 ~ -9,  # Refused
      x == -91 ~ -1,  # Not applicable
      x == -99 ~ -3,  # Not asked at fieldwork stage
      x == -100 ~ -2, # Schedule not applicable / script error
      x == -97 ~ -2,  # Schedule not applicable / script error
      x == 13 & wave %in% c("W8DGOR", "W9DRGN") ~ -2,  # Unknown due to faulty/missing postcode
      TRUE ~ x
    )
  } else {
    x
  }
}

# Apply missing value mapping to relevant variables
merged_data <- merged_data %>% 
  mutate(across(c(urbind_w2 = urbind.x, gor_w2 = gor.x, urbind_w3 = urbind.y, gor_w3 = gor.y, 
                  W8DGOR, W9DRGN, W9NATIONRES), ~ map_missing(., wave = cur_column())))

# Derive regub15 and regov15 from wave 2 (age 15)
merged_data <- merged_data %>% 
  mutate(
    regub15 = case_when(
      !is.na(urbind_w2) & urbind_w2 >= 1 & urbind_w2 <= 8 ~ urbind_w2,
      urbind_w2 == -94 ~ -8,
      urbind_w2 == -92 ~ -9,
      urbind_w2 == -91 ~ -1,
      urbind_w2 == -99 ~ -3,
      urbind_w2 == -100 | urbind_w2 == -97 ~ -2,
      TRUE ~ -3
    ),
    regov15 = case_when(
      !is.na(gor_w2) & gor_w2 >= 1 & gor_w2 <= 9 ~ gor_w2,
      gor_w2 == -94 ~ -8,
      gor_w2 == -92 ~ -9,
      gor_w2 == -91 ~ -1,
      gor_w2 == -99 ~ -3,
      gor_w2 == -100 | gor_w2 == -97 ~ -2,
      TRUE ~ -3
    )
  )

# Derive regub16 and regov16 from wave 3 (age 16)
merged_data <- merged_data %>% 
  mutate(
    regub16 = case_when(
      !is.na(urbind_w3) & urbind_w3 >= 1 & urbind_w3 <= 8 ~ urbind_w3,
      urbind_w3 == -94 ~ -8,
      urbind_w3 == -92 ~ -9,
      urbind_w3 == -91 ~ -1,
      urbind_w3 == -99 ~ -3,
      urbind_w3 == -100 | urbind_w3 == -97 ~ -2,
      TRUE ~ -3
    ),
    regov16 = case_when(
      !is.na(gor_w3) & gor_w3 >= 1 & gor_w3 <= 9 ~ gor_w3,
      gor_w3 == -94 ~ -8,
      gor_w3 == -92 ~ -9,
      gor_w3 == -91 ~ -1,
      gor_w3 == -99 ~ -3,
      gor_w3 == -100 | gor_w3 == -97 ~ -2,
      TRUE ~ -3
    )
  )

# Derive regor25 from wave 8 (age 25)
merged_data <- merged_data %>% 
  mutate(
    regor25 = case_when(
      !is.na(W8DGOR) & W8DGOR >= 1 & W8DGOR <= 12 ~ W8DGOR,
      W8DGOR == -9 ~ -9,
      W8DGOR == -8 ~ -8,
      W8DGOR == -1 ~ -1,
      W8DGOR == 13 ~ -2,
      TRUE ~ -3
    )
  )

# Derive regor32 from wave 9 (age 32)
merged_data <- merged_data %>% 
  mutate(
    regor32 = case_when(
      !is.na(W9DRGN) & W9DRGN >= 1 & W9DRGN <= 12 ~ W9DRGN,
      W9DRGN == -9 ~ -9,
      W9DRGN == -8 ~ -8,
      W9DRGN == -1 ~ -1,
      W9DRGN == 13 ~ -2,
      TRUE ~ -3
    )
  )

# Derive regint32 from wave 9 (age 32)
merged_data <- merged_data %>% 
  mutate(
    regint32 = case_when(
      !is.na(W9NATIONRES) & W9NATIONRES >= 1 & W9NATIONRES <= 4 ~ 1,
      W9NATIONRES == 5 ~ 2,
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

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"
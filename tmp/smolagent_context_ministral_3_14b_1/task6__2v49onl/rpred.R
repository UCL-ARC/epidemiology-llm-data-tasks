
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files explicitly by name
wave_two_family <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three_family <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
ns9_main_interview <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Extract and rename variables from wave_two_family and wave_three_family
wave_two_family <- wave_two_family %>%
  select(NSID, urbind_w2 = urbind, gor_w2 = gor)

wave_three_family <- wave_three_family %>%
  select(NSID, urbind_w3 = urbind, gor_w3 = gor)

# Merge datasets step by step
merged_data <- wave_two_family %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID") %>%
  full_join(ns9_main_interview, by = "NSID") %>%
  mutate(
    regub15 = case_when(
      urbind_w2 %in% -999:-1 ~ -3,
      urbind_w2 == -94 ~ -2,
      is.na(urbind_w2) ~ -3,
      TRUE ~ as.numeric(urbind_w2)
    ),
    regov15 = case_when(
      gor_w2 %in% -999:-1 ~ -3,
      gor_w2 == -94 ~ -2,
      is.na(gor_w2) ~ -3,
      TRUE ~ as.numeric(gor_w2)
    )
  )

# Merge wave_three_family data
merged_data <- merged_data %>%
  full_join(wave_three_family, by = "NSID") %>%
  mutate(
    regub16 = case_when(
      urbind_w3 %in% -999:-1 ~ -3,
      urbind_w3 == -94 ~ -2,
      is.na(urbind_w3) ~ -3,
      TRUE ~ as.numeric(urbind_w3)
    ),
    regov16 = case_when(
      gor_w3 %in% -999:-1 ~ -3,
      gor_w3 == -94 ~ -2,
      is.na(gor_w3) ~ -3,
      TRUE ~ as.numeric(gor_w3)
    )
  )

# Derive regor25 from W8DGOR (age 25)
merged_data <- merged_data %>%
  mutate(
    regor25 = case_when(
      W8DGOR == -9 ~ -9,
      W8DGOR == -8 ~ -2,
      W8DGOR == -1 ~ -1,
      W8DGOR == 13 ~ -2,
      is.na(W8DGOR) ~ -3,
      TRUE ~ as.numeric(W8DGOR)
    )
  )

# Derive regor32 from W9DRGN (age 32)
merged_data <- merged_data %>%
  mutate(
    regor32 = case_when(
      W9DRGN == -9 ~ -9,
      W9DRGN == -8 ~ -2,
      W9DRGN == -1 ~ -1,
      W9DRGN == 13 ~ -2,
      is.na(W9DRGN) ~ -3,
      TRUE ~ as.numeric(W9DRGN)
    )
  )

# Derive regint32 from W9NATIONRES (age 32)
merged_data <- merged_data %>%
  mutate(
    regint32 = case_when(
      W9NATIONRES %in% c(1, 2, 3, 4) ~ 1,  # England, Scotland, Wales, Northern Ireland
      W9NATIONRES == 5 ~ 2,               # Outside of UK or unknown
      W9NATIONRES %in% c(-9, -8, -3, -1) ~ -3,
      TRUE ~ -3
    )
  )

# Select only the required columns
final_data <- merged_data %>%
  select(NSID, regub15, regub16, regov15, regov16, regor25, regor32, regint32)

# Create labeled factors for categorical variables
final_data <- final_data %>%
  mutate(
    regub15 = factor(regub15,
                    levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8),
                    labels = c("Refused", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Not applicable", "Urban >= 10k - sparse", "Town & Fringe - sparse", "Village - sparse", "Hamlet - sparse", "Urban >= 10k - less sparse", "Town & Fringe - less sparse", "Village - less sparse", "Hamlet & Isolated Dwelling")),
    regub16 = factor(regub16,
                    levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8),
                    labels = c("Refused", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Not applicable", "Urban >= 10k - sparse", "Town & Fringe - sparse", "Village - sparse", "Hamlet - sparse", "Urban >= 10k - less sparse", "Town & Fringe - less sparse", "Village - less sparse", "Hamlet & Isolated Dwelling")),
    regov15 = factor(regov15,
                    levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                    labels = c("Refused", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Not applicable", "North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West")),
    regov16 = factor(regov16,
                    levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                    labels = c("Refused", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Not applicable", "North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West")),
    regor25 = factor(regor25,
                    levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                    labels = c("Refused", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Not applicable", "North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland")),
    regor32 = factor(regor32,
                    levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                    labels = c("Refused", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Not applicable", "North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland")),
    regint32 = factor(regint32,
                    levels = c(-9, -8, -7, -3, -2, -1, 1, 2),
                    labels = c("Refused", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Not applicable", "In the UK", "Abroad"))
  )

# Write the cleaned data to CSV
write_csv(final_data, path = "data/output/cleaned_data.csv")

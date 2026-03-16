library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Process wave1 (age 14)
wave1 <- wave1 %>%
  mutate(
    hownteen14 = case_when(
      W1hous12HH == 1 ~ 1,
      W1hous12HH == 2 ~ 2,
      W1hous12HH == 3 ~ 3,
      W1hous12HH == 4 ~ 4,
      W1hous12HH == 5 ~ 5,
      W1hous12HH == 6 ~ 6,
      W1hous12HH == 7 ~ 7,
      W1hous12HH == 8 ~ 8,
      W1hous12HH == -999 ~ -3,
      W1hous12HH == -92 ~ -9,
      W1hous12HH == -91 ~ -1,
      W1hous12HH == -1 ~ -8,
      TRUE ~ -3
    ),
    hown14 = case_when(
      hownteen14 == 1 ~ 1,
      hownteen14 == 2 ~ 2,
      hownteen14 == 3 ~ 3,
      hownteen14 %in% c(4, 5, 6) ~ 4,
      hownteen14 == 7 ~ 5,
      hownteen14 == 8 ~ 6,
      hownteen14 == -9 ~ -9,
      hownteen14 == -8 ~ -8,
      hownteen14 == -1 ~ -1,
      hownteen14 == -3 ~ -3,
      hownteen14 == -2 ~ -2,
      TRUE ~ -3
    )
  )

# Process wave2 (age 15)
wave2 <- wave2 %>%
  mutate(
    hownteen15 = case_when(
      W2Hous12HH == 1 ~ 1,
      W2Hous12HH == 2 ~ 2,
      W2Hous12HH == 3 ~ 3,
      W2Hous12HH == 4 ~ 4,
      W2Hous12HH == 5 ~ 5,
      W2Hous12HH == 6 ~ 6,
      W2Hous12HH == 7 ~ 7,
      W2Hous12HH == 8 ~ 8,
      W2Hous12HH == -998 ~ -2,
      W2Hous12HH == -997 ~ -2,
      W2Hous12HH == -995 ~ -3,
      W2Hous12HH == -99 ~ -3,
      W2Hous12HH == -92 ~ -9,
      W2Hous12HH == -91 ~ -1,
      W2Hous12HH == -1 ~ -8,
      TRUE ~ -3
    ),
    hown15 = case_when(
      hownteen15 == 1 ~ 1,
      hownteen15 == 2 ~ 2,
      hownteen15 == 3 ~ 3,
      hownteen15 %in% c(4, 5, 6) ~ 4,
      hownteen15 == 7 ~ 5,
      hownteen15 == 8 ~ 6,
      hownteen15 == -9 ~ -9,
      hownteen15 == -8 ~ -8,
      hownteen15 == -1 ~ -1,
      hownteen15 == -3 ~ -3,
      hownteen15 == -2 ~ -2,
      TRUE ~ -3
    )
  )

# Process wave3 (age 16)
wave3 <- wave3 %>%
  mutate(
    hownteen16 = case_when(
      W3hous12HH == 1 ~ 1,
      W3hous12HH == 2 ~ 2,
      W3hous12HH == 3 ~ 3,
      W3hous12HH == 4 ~ 4,
      W3hous12HH == 5 ~ 5,
      W3hous12HH == 6 ~ 6,
      W3hous12HH == 7 ~ 7,
      W3hous12HH == 8 ~ 8,
      W3hous12HH == -999 ~ -3,
      W3hous12HH == -99 ~ -3,
      W3hous12HH == -92 ~ -9,
      W3hous12HH == -91 ~ -1,
      W3hous12HH == -1 ~ -8,
      TRUE ~ -3
    ),
    hown16 = case_when(
      hownteen16 == 1 ~ 1,
      hownteen16 == 2 ~ 2,
      hownteen16 == 3 ~ 3,
      hownteen16 %in% c(4, 5, 6) ~ 4,
      hownteen16 == 7 ~ 5,
      hownteen16 == 8 ~ 6,
      hownteen16 == -9 ~ -9,
      hownteen16 == -8 ~ -8,
      hownteen16 == -1 ~ -1,
      hownteen16 == -3 ~ -3,
      hownteen16 == -2 ~ -2,
      TRUE ~ -3
    )
  )

# Process wave4 (age 17)
wave4 <- wave4 %>%
  mutate(
    hownteen17 = case_when(
      W4Hous12HH == 1 ~ 1,
      W4Hous12HH == 2 ~ 2,
      W4Hous12HH == 3 ~ 3,
      W4Hous12HH == 4 ~ 4,
      W4Hous12HH == 5 ~ 5,
      W4Hous12HH == 6 ~ 6,
      W4Hous12HH == 7 ~ 7,
      W4Hous12HH == 8 ~ 8,
      W4Hous12HH == -999 ~ -3,
      W4Hous12HH == -997 ~ -2,
      W4Hous12HH == -92 ~ -9,
      W4Hous12HH == -91 ~ -1,
      W4Hous12HH == -1 ~ -8,
      TRUE ~ -3
    ),
    hown17 = case_when(
      hownteen17 == 1 ~ 1,
      hownteen17 == 2 ~ 2,
      hownteen17 == 3 ~ 3,
      hownteen17 %in% c(4, 5, 6) ~ 4,
      hownteen17 == 7 ~ 5,
      hownteen17 == 8 ~ 6,
      hownteen17 == -9 ~ -9,
      hownteen17 == -8 ~ -8,
      hownteen17 == -1 ~ -1,
      hownteen17 == -3 ~ -3,
      hownteen17 == -2 ~ -2,
      TRUE ~ -3
    )
  )

# Process wave5 (age 18)
wave5 <- wave5 %>%
  mutate(
    W5Hous12HH_mapped = case_when(
      W5Hous12HH == -92 ~ -9,
      W5Hous12HH == -91 ~ -1,
      W5Hous12HH == -1 ~ -8,
      W5Hous12HH == -999 ~ -3,
      TRUE ~ W5Hous12HH
    ),
    W5Hous12BHH_mapped = case_when(
      W5Hous12BHH == -92 ~ -9,
      W5Hous12BHH == -91 ~ -1,
      W5Hous12BHH == -1 ~ -8,
      W5Hous12BHH == -999 ~ -3,
      TRUE ~ W5Hous12BHH
    ),
    W5Hous12CHH_mapped = case_when(
      W5Hous12CHH == -92 ~ -9,
      W5Hous12CHH == -91 ~ -1,
      W5Hous12CHH == -1 ~ -8,
      W5Hous12CHH == -999 ~ -3,
      TRUE ~ W5Hous12CHH
    ),
    hownteen18 = case_when(
      W5Hous12HH_mapped == 1 ~ case_when(
        W5Hous12BHH_mapped == 1 ~ 1,
        W5Hous12BHH_mapped == 2 ~ 2,
        W5Hous12BHH_mapped == 3 ~ 3,
        W5Hous12BHH_mapped == 4 ~ 8,
        TRUE ~ -3
      ),
      W5Hous12HH_mapped == 2 ~ case_when(
        W5Hous12CHH_mapped == 1 ~ 4,
        W5Hous12CHH_mapped == 2 ~ 5,
        W5Hous12CHH_mapped == 3 ~ 6,
        W5Hous12CHH_mapped == 4 ~ 7,
        W5Hous12CHH_mapped == 5 ~ 8,
        TRUE ~ -3
      ),
      W5Hous12HH_mapped == 3 ~ 8,
      W5Hous12HH_mapped == -9 ~ -9,
      W5Hous12HH_mapped == -1 ~ -1,
      W5Hous12HH_mapped == -8 ~ -8,
      W5Hous12HH_mapped == -3 ~ -3,
      TRUE ~ -3
    ),
    hown18 = case_when(
      hownteen18 == 1 ~ 1,
      hownteen18 == 2 ~ 2,
      hownteen18 == 3 ~ 3,
      hownteen18 %in% c(4, 5, 6) ~ 4,
      hownteen18 == 7 ~ 5,
      hownteen18 == 8 ~ 6,
      hownteen18 == -9 ~ -9,
      hownteen18 == -8 ~ -8,
      hownteen18 == -1 ~ -1,
      hownteen18 == -3 ~ -3,
      hownteen18 == -2 ~ -2,
      TRUE ~ -3
    )
  )

# Process wave6 (age 19)
wave6 <- wave6 %>%
  mutate(
    W6Hous12YP_mapped = case_when(
      W6Hous12YP == -92 ~ -9,
      W6Hous12YP == -91 ~ -1,
      W6Hous12YP == -1 ~ -8,
      TRUE ~ W6Hous12YP
    ),
    W6Hous12bYP_mapped = case_when(
      W6Hous12bYP == -92 ~ -9,
      W6Hous12bYP == -91 ~ -1,
      W6Hous12bYP == -1 ~ -8,
      TRUE ~ W6Hous12bYP
    ),
    W6Hous12cYP_mapped = case_when(
      W6Hous12cYP == -92 ~ -9,
      W6Hous12cYP == -91 ~ -1,
      W6Hous12cYP == -1 ~ -8,
      TRUE ~ W6Hous12cYP
    ),
    hownteen19 = case_when(
      W6Hous12YP_mapped == 1 ~ case_when(
        W6Hous12bYP_mapped == 1 ~ 1,
        W6Hous12bYP_mapped == 2 ~ 2,
        W6Hous12bYP_mapped == 3 ~ 3,
        W6Hous12bYP_mapped == 4 ~ 8,
        TRUE ~ -3
      ),
      W6Hous12YP_mapped == 2 ~ case_when(
        W6Hous12cYP_mapped == 1 ~ 4,
        W6Hous12cYP_mapped == 2 ~ 5,
        W6Hous12cYP_mapped == 3 ~ 6,
        W6Hous12cYP_mapped == 4 ~ 7,
        W6Hous12cYP_mapped == 5 ~ 8,
        TRUE ~ -3
      ),
      W6Hous12YP_mapped == 3 ~ 8,
      W6Hous12YP_mapped == -9 ~ -9,
      W6Hous12YP_mapped == -1 ~ -1,
      W6Hous12YP_mapped == -8 ~ -8,
      TRUE ~ -3
    ),
    hown19 = case_when(
      hownteen19 == 1 ~ 1,
      hownteen19 == 2 ~ 2,
      hownteen19 == 3 ~ 3,
      hownteen19 %in% c(4, 5, 6) ~ 4,
      hownteen19 == 7 ~ 5,
      hownteen19 == 8 ~ 6,
      hownteen19 == -9 ~ -9,
      hownteen19 == -8 ~ -8,
      hownteen19 == -1 ~ -1,
      hownteen19 == -3 ~ -3,
      hownteen19 == -2 ~ -2,
      TRUE ~ -3
    )
  )

# Process wave7 (age 20)
wave7 <- wave7 %>%
  mutate(
    W7Hous12YP_mapped = case_when(
      W7Hous12YP == -92 ~ -9,
      W7Hous12YP == -91 ~ -1,
      W7Hous12YP == -1 ~ -8,
      TRUE ~ W7Hous12YP
    ),
    W7Hous12bYP_mapped = case_when(
      W7Hous12bYP == -92 ~ -9,
      W7Hous12bYP == -91 ~ -1,
      W7Hous12bYP == -1 ~ -8,
      TRUE ~ W7Hous12bYP
    ),
    W7Hous12cYP_mapped = case_when(
      W7Hous12cYP == -92 ~ -9,
      W7Hous12cYP == -91 ~ -1,
      W7Hous12cYP == -1 ~ -8,
      TRUE ~ W7Hous12cYP
    ),
    hownteen20 = case_when(
      W7Hous12YP_mapped == 1 ~ case_when(
        W7Hous12bYP_mapped == 1 ~ 1,
        W7Hous12bYP_mapped == 2 ~ 2,
        W7Hous12bYP_mapped == 3 ~ 3,
        W7Hous12bYP_mapped == 4 ~ 8,
        TRUE ~ -3
      ),
      W7Hous12YP_mapped == 2 ~ case_when(
        W7Hous12cYP_mapped == 1 ~ 4,
        W7Hous12cYP_mapped == 2 ~ 5,
        W7Hous12cYP_mapped == 3 ~ 6,
        W7Hous12cYP_mapped == 4 ~ 7,
        W7Hous12cYP_mapped == 5 ~ 8,
        TRUE ~ -3
      ),
      W7Hous12YP_mapped == 3 ~ 8,
      W7Hous12YP_mapped == -9 ~ -9,
      W7Hous12YP_mapped == -1 ~ -1,
      W7Hous12YP_mapped == -8 ~ -8,
      TRUE ~ -3
    ),
    hown20 = case_when(
      hownteen20 == 1 ~ 1,
      hownteen20 == 2 ~ 2,
      hownteen20 == 3 ~ 3,
      hownteen20 %in% c(4, 5, 6) ~ 4,
      hownteen20 == 7 ~ 5,
      hownteen20 == 8 ~ 6,
      hownteen20 == -9 ~ -9,
      hownteen20 == -8 ~ -8,
      hownteen20 == -1 ~ -1,
      hownteen20 == -3 ~ -3,
      hownteen20 == -2 ~ -2,
      TRUE ~ -3
    )
  )

# Process ns8 (age 25)
ns8 <- ns8 %>%
  mutate(
    hown25 = case_when(
      W8TENURE == 1 ~ 1,
      W8TENURE == 2 ~ 2,
      W8TENURE == 3 ~ 3,
      W8TENURE == 4 ~ 4,
      W8TENURE == 5 ~ 5,
      W8TENURE == 6 ~ 6,
      W8TENURE == 7 ~ 6,
      W8TENURE == -9 ~ -9,
      W8TENURE == -8 ~ -8,
      W8TENURE == -1 ~ -1,
      TRUE ~ -3
    )
  )

# Process ns9 (age 32)
ns9 <- ns9 %>%
  mutate(
    hown32 = case_when(
      W9DTENURE == 1 ~ 1,
      W9DTENURE == 2 ~ 2,
      W9DTENURE == 3 ~ 3,
      W9DTENURE == 4 ~ 4,
      W9DTENURE == 5 ~ 5,
      W9DTENURE == 6 ~ 6,
      W9DTENURE == 7 ~ 6,
      W9DTENURE == -8 ~ -8,
      TRUE ~ -3
    )
  )

# Merge all datasets by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Select only required variables
final_data <- merged_data %>%
  select(
    NSID,
    starts_with("hownteen"),
    starts_with("hown")
  )

# Write to CSV
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

file_names <- c(
  "ns9_2022_derived_variables.tab",
  "wave_four_lsype_family_background_2020.tab",
  "wave_three_lsype_family_background_2020.tab",
  "wave_two_lsype_family_background_2020.tab",
  "wave_one_lsype_family_background_2020.tab",
  "ns8_2015_main_interview.tab",
  "wave_five_lsype_family_background_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab"
)

data_list <- map(file_names, function(f) {
  read_delim(file.path("data/input", f), delim = "\t")
})

merged_data <- Reduce(function(x, y) full_join(x, y, by = "NSID"), data_list)

# Age 14 (Wave 1)
merged_data <- merged_data %>%
  mutate(
    hownteen14 = case_when(
      W1hous12HH == -999 ~ -2,
      W1hous12HH == -92 ~ -9,
      W1hous12HH == -91 ~ -1,
      W1hous12HH == -1 ~ -8,
      TRUE ~ W1hous12HH
    ),
    hown14 = case_when(
      hownteen14 == 1 ~ 1,
      hownteen14 == 2 ~ 2,
      hownteen14 == 3 ~ 3,
      hownteen14 == 4 ~ 4,
      hownteen14 == 5 ~ 4,
      hownteen14 == 6 ~ 4,
      hownteen14 == 7 ~ 5,
      hownteen14 == 8 ~ 6,
      hownteen14 == -9 ~ -9,
      hownteen14 == -8 ~ -8,
      hownteen14 == -1 ~ -1,
      hownteen14 == -2 ~ -2,
      hownteen14 == -3 ~ -3,
      TRUE ~ NA_real_
    )
  )

# Age 15 (Wave 2)
merged_data <- merged_data %>%
  mutate(
    hownteen15 = case_when(
      W2Hous12HH == -998 ~ -2,
      W2Hous12HH == -997 ~ -2,
      W2Hous12HH == -995 ~ -2,
      W2Hous12HH == -99 ~ -2,
      W2Hous12HH == -92 ~ -9,
      W2Hous12HH == -91 ~ -1,
      W2Hous12HH == -1 ~ -8,
      TRUE ~ W2Hous12HH
    ),
    hown15 = case_when(
      hownteen15 == 1 ~ 1,
      hownteen15 == 2 ~ 2,
      hownteen15 == 3 ~ 3,
      hownteen15 == 4 ~ 4,
      hownteen15 == 5 ~ 4,
      hownteen15 == 6 ~ 4,
      hownteen15 == 7 ~ 5,
      hownteen15 == 8 ~ 6,
      hownteen15 == -9 ~ -9,
      hownteen15 == -8 ~ -8,
      hownteen15 == -1 ~ -1,
      hownteen15 == -2 ~ -2,
      hownteen15 == -3 ~ -3,
      TRUE ~ NA_real_
    )
  )

# Age 16 (Wave 3)
merged_data <- merged_data %>%
  mutate(
    hownteen16 = case_when(
      W3hous12HH == -999 ~ -2,
      W3hous12HH == -99 ~ -2,
      W3hous12HH == -92 ~ -9,
      W3hous12HH == -91 ~ -1,
      W3hous12HH == -1 ~ -8,
      TRUE ~ W3hous12HH
    ),
    hown16 = case_when(
      hownteen16 == 1 ~ 1,
      hownteen16 == 2 ~ 2,
      hownteen16 == 3 ~ 3,
      hownteen16 == 4 ~ 4,
      hownteen16 == 5 ~ 4,
      hownteen16 == 6 ~ 4,
      hownteen16 == 7 ~ 5,
      hownteen16 == 8 ~ 6,
      hownteen16 == -9 ~ -9,
      hownteen16 == -8 ~ -8,
      hownteen16 == -1 ~ -1,
      hownteen16 == -2 ~ -2,
      hownteen16 == -3 ~ -3,
      TRUE ~ NA_real_
    )
  )

# Age 17 (Wave 4)
merged_data <- merged_data %>%
  mutate(
    hownteen17 = case_when(
      W4Hous12HH == -999 ~ -2,
      W4Hous12HH == -997 ~ -2,
      W4Hous12HH == -92 ~ -9,
      W4Hous12HH == -91 ~ -1,
      W4Hous12HH == -1 ~ -8,
      TRUE ~ W4Hous12HH
    ),
    hown17 = case_when(
      hownteen17 == 1 ~ 1,
      hownteen17 == 2 ~ 2,
      hownteen17 == 3 ~ 3,
      hownteen17 == 4 ~ 4,
      hownteen17 == 5 ~ 4,
      hownteen17 == 6 ~ 4,
      hownteen17 == 7 ~ 5,
      hownteen17 == 8 ~ 6,
      hownteen17 == -9 ~ -9,
      hownteen17 == -8 ~ -8,
      hownteen17 == -1 ~ -1,
      hownteen17 == -2 ~ -2,
      hownteen17 == -3 ~ -3,
      TRUE ~ NA_real_
    )
  )

# Age 18 (Wave 5)
merged_data <- merged_data %>%
  mutate(
    hownteen18 = case_when(
      W5Hous12HH == -999 ~ -2,
      W5Hous12HH == -92 ~ -9,
      W5Hous12HH == -91 ~ -1,
      W5Hous12HH == -1 ~ -8,
      W5Hous12HH == 6 ~ -3,
      W5Hous12HH == 1 & W5Hous12BHH == -999 ~ -2,
      W5Hous12HH == 1 & W5Hous12BHH == -92 ~ -9,
      W5Hous12HH == 1 & W5Hous12BHH == -91 ~ -1,
      W5Hous12HH == 1 & W5Hous12BHH == -1 ~ -8,
      W5Hous12HH == 1 & W5Hous12BHH == 1 ~ 1,
      W5Hous12HH == 1 & W5Hous12BHH == 2 ~ 2,
      W5Hous12HH == 1 & W5Hous12BHH == 3 ~ 3,
      W5Hous12HH == 1 & W5Hous12BHH == 4 ~ 4,
      W5Hous12HH == 2 & W5Hous12CHH == -999 ~ -2,
      W5Hous12HH == 2 & W5Hous12CHH == -92 ~ -9,
      W5Hous12HH == 2 & W5Hous12CHH == -91 ~ -1,
      W5Hous12HH == 2 & W5Hous12CHH == -1 ~ -8,
      W5Hous12HH == 2 & W5Hous12CHH == 1 ~ 4,
      W5Hous12HH == 2 & W5Hous12CHH == 2 ~ 5,
      W5Hous12HH == 2 & W5Hous12CHH == 3 ~ 6,
      W5Hous12HH == 2 & W5Hous12CHH == 4 ~ 7,
      W5Hous12HH == 2 & W5Hous12CHH == 5 ~ 8,
      W5Hous12HH == 3 ~ 8,
      TRUE ~ NA_real_
    ),
    hown18 = case_when(
      hownteen18 == 1 ~ 1,
      hownteen18 == 2 ~ 2,
      hownteen18 == 3 ~ 3,
      hownteen18 == 4 ~ 4,
      hownteen18 == 5 ~ 4,
      hownteen18 == 6 ~ 4,
      hownteen18 == 7 ~ 5,
      hownteen18 == 8 ~ 6,
      hownteen18 == -9 ~ -9,
      hownteen18 == -8 ~ -8,
      hownteen18 == -1 ~ -1,
      hownteen18 == -2 ~ -2,
      hownteen18 == -3 ~ -3,
      TRUE ~ NA_real_
    )
  )

# Age 19 (Wave 6)
merged_data <- merged_data %>%
  mutate(
    hownteen19 = case_when(
      W6Hous12YP == -92 ~ -9,
      W6Hous12YP == -91 ~ -1,
      W6Hous12YP == -1 ~ -8,
      W6Hous12YP == 1 & W6Hous12bYP == -92 ~ -9,
      W6Hous12YP == 1 & W6Hous12bYP == -91 ~ -1,
      W6Hous12YP == 1 & W6Hous12bYP == -1 ~ -8,
      W6Hous12YP == 1 & W6Hous12bYP == 1 ~ 1,
      W6Hous12YP == 1 & W6Hous12bYP == 2 ~ 2,
      W6Hous12YP == 1 & W6Hous12bYP == 3 ~ 3,
      W6Hous12YP == 1 & W6Hous12bYP == 4 ~ 4,
      W6Hous12YP == 2 & W6Hous12cYP == -92 ~ -9,
      W6Hous12YP == 2 & W6Hous12cYP == -91 ~ -1,
      W6Hous12YP == 2 & W6Hous12cYP == -1 ~ -8,
      W6Hous12YP == 2 & W6Hous12cYP == 1 ~ 4,
      W6Hous12YP == 2 & W6Hous12cYP == 2 ~ 5,
      W6Hous12YP == 2 & W6Hous12cYP == 3 ~ 6,
      W6Hous12YP == 2 & W6Hous12cYP == 4 ~ 7,
      W6Hous12YP == 2 & W6Hous12cYP == 5 ~ 8,
      W6Hous12YP == 3 ~ 8,
      TRUE ~ NA_real_
    ),
    hown19 = case_when(
      hownteen19 == 1 ~ 1,
      hownteen19 == 2 ~ 2,
      hownteen19 == 3 ~ 3,
      hownteen19 == 4 ~ 4,
      hownteen19 == 5 ~ 4,
      hownteen19 == 6 ~ 4,
      hownteen19 == 7 ~ 5,
      hownteen19 == 8 ~ 6,
      hownteen19 == -9 ~ -9,
      hownteen19 == -8 ~ -8,
      hownteen19 == -1 ~ -1,
      hownteen19 == -2 ~ -2,
      hownteen19 == -3 ~ -3,
      TRUE ~ NA_real_
    )
  )

# Age 20 (Wave 7)
merged_data <- merged_data %>%
  mutate(
    hownteen20 = case_when(
      W7Hous12YP == -92 ~ -9,
      W7Hous12YP == -91 ~ -1,
      W7Hous12YP == -1 ~ -8,
      W7Hous12YP == 1 & W7Hous12bYP == -92 ~ -9,
      W7Hous12YP == 1 & W7Hous12bYP == -91 ~ -1,
      W7Hous12YP == 1 & W7Hous12bYP == -1 ~ -8,
      W7Hous12YP == 1 & W7Hous12bYP == 1 ~ 1,
      W7Hous12YP == 1 & W7Hous12bYP == 2 ~ 2,
      W7Hous12YP == 1 & W7Hous12bYP == 3 ~ 3,
      W7Hous12YP == 1 & W7Hous12bYP == 4 ~ 4,
      W7Hous12YP == 2 & W7Hous12cYP == -92 ~ -9,
      W7Hous12YP == 2 & W7Hous12cYP == -91 ~ -1,
      W7Hous12YP == 2 & W7Hous12cYP == -1 ~ -8,
      W7Hous12YP == 2 & W7Hous12cYP == 1 ~ 4,
      W7Hous12YP == 2 & W7Hous12cYP == 2 ~ 5,
      W7Hous12YP == 2 & W7Hous12cYP == 3 ~ 6,
      W7Hous12YP == 2 & W7Hous12cYP == 4 ~ 7,
      W7Hous12YP == 2 & W7Hous12cYP == 5 ~ 8,
      W7Hous12YP == 3 ~ 8,
      TRUE ~ NA_real_
    ),
    hown20 = case_when(
      hownteen20 == 1 ~ 1,
      hownteen20 == 2 ~ 2,
      hownteen20 == 3 ~ 3,
      hownteen20 == 4 ~ 4,
      hownteen20 == 5 ~ 4,
      hownteen20 == 6 ~ 4,
      hownteen20 == 7 ~ 5,
      hownteen20 == 8 ~ 6,
      hownteen20 == -9 ~ -9,
      hownteen20 == -8 ~ -8,
      hownteen20 == -1 ~ -1,
      hownteen20 == -2 ~ -2,
      hownteen20 == -3 ~ -3,
      TRUE ~ NA_real_
    )
  )

# Age 25 (Wave 8)
merged_data <- merged_data %>%
  mutate(
    hown25 = case_when(
      W8TENURE == -9 ~ -9,
      W8TENURE == -8 ~ -8,
      W8TENURE == -1 ~ -1,
      W8TENURE == 1 ~ 1,
      W8TENURE == 2 ~ 2,
      W8TENURE == 3 ~ 3,
      W8TENURE == 4 ~ 4,
      W8TENURE == 5 ~ 5,
      W8TENURE == 6 | W8TENURE == 7 ~ 6,
      TRUE ~ NA_real_
    )
  )

# Age 32 (Wave 9)
merged_data <- merged_data %>%
  mutate(
    hown32 = case_when(
      W9DTENURE == -8 ~ -8,
      W9DTENURE == 1 ~ 1,
      W9DTENURE == 2 ~ 2,
      W9DTENURE == 3 ~ 3,
      W9DTENURE == 4 ~ 4,
      W9DTENURE == 5 ~ 5,
      W9DTENURE == 6 | W9DTENURE == 7 ~ 6,
      TRUE ~ NA_real_
    )
  )

# Select required columns
final_data <- merged_data %>%
  select(
    NSID,
    hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32,
    hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20
  )

# Write to CSV
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
ns9_2022_derived_variables <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
wave_four_lsype_family_background_2020 <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave_three_lsype_family_background_2020 <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_two_lsype_family_background_2020 <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_one_lsype_family_background_2020 <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
ns8_2015_main_interview <- readr::read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave_five_lsype_family_background_2020 <- readr::read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
wave_six_lsype_young_person_2020 <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven_lsype_young_person_2020 <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")

# Merge datasets
merged_data <- full_join(wave_one_lsype_family_background_2020, wave_two_lsype_family_background_2020, by = "NSID")
merged_data <- full_join(merged_data, wave_three_lsype_family_background_2020, by = "NSID")
merged_data <- full_join(merged_data, wave_four_lsype_family_background_2020, by = "NSID")
merged_data <- full_join(merged_data, wave_five_lsype_family_background_2020, by = "NSID")
merged_data <- full_join(merged_data, wave_six_lsype_young_person_2020, by = "NSID")
merged_data <- full_join(merged_data, wave_seven_lsype_young_person_2020, by = "NSID")
merged_data <- full_join(merged_data, ns8_2015_main_interview, by = "NSID")
merged_data <- full_join(merged_data, ns9_2022_derived_variables, by = "NSID")

# Recode missing values
merged_data <- merged_data %>% mutate(across(where(is.numeric), ~case_when(
  .x == -999.0 ~ -3,
  .x == -997.0 ~ -2,
  .x == -995.0 ~ -2,
  .x == -99.0 ~ -2,
  .x == -92.0 ~ -9,
  .x == -91.0 ~ -1,
  .x == -1.0 ~ -8,
  .x == -9.0 ~ -9,
  .x == -8.0 ~ -8,
  TRUE ~ .x
)))

# Create detailed adolescent variables
merged_data <- merged_data %>% mutate(
  hownteen14 = case_when(
    W1hous12HH == 1 ~ 1,
    W1hous12HH == 2 ~ 2,
    W1hous12HH == 3 ~ 3,
    W1hous12HH == 4 ~ 4,
    W1hous12HH == 5 ~ 5,
    W1hous12HH == 6 ~ 6,
    W1hous12HH == 7 ~ 7,
    W1hous12HH == 8 ~ 8,
    TRUE ~ NA_real_
  ),
  hownteen15 = case_when(
    W2Hous12HH == 1 ~ 1,
    W2Hous12HH == 2 ~ 2,
    W2Hous12HH == 3 ~ 3,
    W2Hous12HH == 4 ~ 4,
    W2Hous12HH == 5 ~ 5,
    W2Hous12HH == 6 ~ 6,
    W2Hous12HH == 7 ~ 7,
    W2Hous12HH == 8 ~ 8,
    TRUE ~ NA_real_
  ),
  hownteen16 = case_when(
    W3hous12HH == 1 ~ 1,
    W3hous12HH == 2 ~ 2,
    W3hous12HH == 3 ~ 3,
    W3hous12HH == 4 ~ 4,
    W3hous12HH == 5 ~ 5,
    W3hous12HH == 6 ~ 6,
    W3hous12HH == 7 ~ 7,
    W3hous12HH == 8 ~ 8,
    TRUE ~ NA_real_
  ),
  hownteen17 = case_when(
    W4Hous12HH == 1 ~ 1,
    W4Hous12HH == 2 ~ 2,
    W4Hous12HH == 3 ~ 3,
    W4Hous12HH == 4 ~ 4,
    W4Hous12HH == 5 ~ 5,
    W4Hous12HH == 6 ~ 6,
    W4Hous12HH == 7 ~ 7,
    W4Hous12HH == 8 ~ 8,
    TRUE ~ NA_real_
  ),
  hownteen18 = case_when(
    W5Hous12HH == 1 ~ 1,
    W5Hous12HH == 2 ~ 2,
    W5Hous12HH == 3 ~ 3,
    W5Hous12HH == 4 ~ 4,
    W5Hous12HH == 5 ~ 5,
    W5Hous12HH == 6 ~ 6,
    W5Hous12HH == 7 ~ 7,
    W5Hous12HH == 8 ~ 8,
    TRUE ~ NA_real_
  ),
  hownteen19 = case_when(
    W6Hous12YP == 1 ~ 1,
    W6Hous12YP == 2 ~ 2,
    W6Hous12YP == 3 ~ 3,
    W6Hous12bYP == 1 ~ 1,
    W6Hous12bYP == 2 ~ 2,
    W6Hous12bYP == 3 ~ 3,
    W6Hous12bYP == 4 ~ 8,
    W6Hous12cYP == 1 ~ 4,
    W6Hous12cYP == 2 ~ 5,
    W6Hous12cYP == 3 ~ 6,
    W6Hous12cYP == 4 ~ 7,
    W6Hous12cYP == 5 ~ 8,
    TRUE ~ NA_real_
  ),
  hownteen20 = case_when(
    W7Hous12YP == 1 ~ 1,
    W7Hous12YP == 2 ~ 2,
    W7Hous12YP == 3 ~ 3,
    W7Hous12bYP == 1 ~ 1,
    W7Hous12bYP == 2 ~ 2,
    W7Hous12bYP == 3 ~ 3,
    W7Hous12bYP == 4 ~ 8,
    W7Hous12cYP == 1 ~ 4,
    W7Hous12cYP == 2 ~ 5,
    W7Hous12cYP == 3 ~ 6,
    W7Hous12cYP == 4 ~ 7,
    W7Hous12cYP == 5 ~ 8,
    TRUE ~ NA_real_
  )
)

# Create collapsed variables
merged_data <- merged_data %>% mutate(
  hown14 = case_when(
    hownteen14 == 1 ~ 1,
    hownteen14 == 2 ~ 2,
    hownteen14 == 3 ~ 3,
    hownteen14 == 4 ~ 4,
    hownteen14 == 5 ~ 4,
    hownteen14 == 6 ~ 4,
    hownteen14 == 7 ~ 5,
    hownteen14 == 8 ~ 6,
    TRUE ~ NA_real_
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
    TRUE ~ NA_real_
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
    TRUE ~ NA_real_
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
    TRUE ~ NA_real_
  ),
  hown25 = case_when(
    W8TENURE == 1 ~ 1,
    W8TENURE == 2 ~ 2,
    W8TENURE == 3 ~ 3,
    W8TENURE == 4 ~ 4,
    W8TENURE == 5 ~ 5,
    W8TENURE == 6 ~ 6,
    W8TENURE == 7 ~ 6,
    TRUE ~ NA_real_
  ),
  hown32 = case_when(
    W9DTENURE == 1 ~ 1,
    W9DTENURE == 2 ~ 2,
    W9DTENURE == 3 ~ 3,
    W9DTENURE == 4 ~ 4,
    W9DTENURE == 5 ~ 5,
    W9DTENURE == 6 ~ 6,
    W9DTENURE == 7 ~ 6,
    TRUE ~ NA_real_
  )
)

# Add labels using attr
attr(merged_data$hownteen14, "label") <- "Detailed adolescent housing tenure at age 14"
attr(merged_data$hownteen15, "label") <- "Detailed adolescent housing tenure at age 15"
attr(merged_data$hownteen16, "label") <- "Detailed adolescent housing tenure at age 16"
attr(merged_data$hownteen17, "label") <- "Detailed adolescent housing tenure at age 17"
attr(merged_data$hownteen18, "label") <- "Detailed adolescent housing tenure at age 18"
attr(merged_data$hownteen19, "label") <- "Detailed adolescent housing tenure at age 19"
attr(merged_data$hownteen20, "label") <- "Detailed adolescent housing tenure at age 20"
attr(merged_data$hown14, "label") <- "Collapsed housing tenure at age 14"
attr(merged_data$hown15, "label") <- "Collapsed housing tenure at age 15"
attr(merged_data$hown16, "label") <- "Collapsed housing tenure at age 16"
attr(merged_data$hown17, "label") <- "Collapsed housing tenure at age 17"
attr(merged_data$hown18, "label") <- "Collapsed housing tenure at age 18"
attr(merged_data$hown19, "label") <- "Collapsed housing tenure at age 19"
attr(merged_data$hown20, "label") <- "Collapsed housing tenure at age 20"
attr(merged_data$hown25, "label") <- "Collapsed housing tenure at age 25"
attr(merged_data$hown32, "label") <- "Collapsed housing tenure at age 32"

# Select variables to keep
cleaned_data <- merged_data %>% select(NSID, hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20, hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32)

# Write to CSV
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)
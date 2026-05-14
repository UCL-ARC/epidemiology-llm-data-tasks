library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load each dataset into a separate object
ns9_2022_derived_variables <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
wave_four_lsype_family_background_2020 <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave_three_lsype_family_background_2020 <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_two_lsype_family_background_2020 <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_one_lsype_family_background_2020 <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
ns8_2015_main_interview <- readr::read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave_five_lsype_family_background_2020 <- readr::read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
wave_six_lsype_young_person_2020 <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven_lsype_young_person_2020 <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")

# Merge all datasets using full_join by NSID
merged_data <- full_join(ns9_2022_derived_variables, wave_four_lsype_family_background_2020, by = "NSID")
merged_data <- full_join(merged_data, wave_three_lsype_family_background_2020, by = "NSID")
merged_data <- full_join(merged_data, wave_two_lsype_family_background_2020, by = "NSID")
merged_data <- full_join(merged_data, wave_one_lsype_family_background_2020, by = "NSID")
merged_data <- full_join(merged_data, ns8_2015_main_interview, by = "NSID")
merged_data <- full_join(merged_data, wave_five_lsype_family_background_2020, by = "NSID")
merged_data <- full_join(merged_data, wave_six_lsype_young_person_2020, by = "NSID")
merged_data <- full_join(merged_data, wave_seven_lsype_young_person_2020, by = "NSID")

# Create detailed adolescent variables per age wave
merged_data <- merged_data %>% 
  mutate(hownteen14 = case_when(
    W1hous12HH == 1 ~ "Owned outright",
    W1hous12HH == 2 ~ "Being bought on a mortgage/bank loan",
    W1hous12HH == 3 ~ "Shared ownership (owns & rents property)",
    W1hous12HH == 4 ~ "Rented from a Council or New Town",
    W1hous12HH == 5 ~ "Rented from a Housing Association",
    W1hous12HH == 6 ~ "Rented privately",
    W1hous12HH == 7 ~ "Rent free",
    W1hous12HH == 8 ~ "Some other arrangement",
    W1hous12HH == -92 ~ "Refusal",
    W1hous12HH == -91 ~ "Item not applicable",
    W1hous12HH == -1 ~ "Don't know/insufficient information",
    W1hous12HH == -999 ~ "Script error/information lost",
    TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
  ))

merged_data <- merged_data %>% 
  mutate(hownteen15 = case_when(
    W2Hous12HH == 1 ~ "Owned outright",
    W2Hous12HH == 2 ~ "Being bought on a mortgage/bank loan",
    W2Hous12HH == 3 ~ "Shared ownership (owns & rents property)",
    W2Hous12HH == 4 ~ "Rented from a Council or New Town",
    W2Hous12HH == 5 ~ "Rented from a Housing Association",
    W2Hous12HH == 6 ~ "Rented privately",
    W2Hous12HH == 7 ~ "Rent free",
    W2Hous12HH == 8 ~ "Some other arrangement",
    W2Hous12HH == -92 ~ "Refusal",
    W2Hous12HH == -91 ~ "Item not applicable",
    W2Hous12HH == -1 ~ "Don't know/insufficient information",
    W2Hous12HH == -998 ~ "Script error/information lost",
    TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
  ))

merged_data <- merged_data %>% 
  mutate(hownteen16 = case_when(
    W3hous12HH == 1 ~ "Owned outright",
    W3hous12HH == 2 ~ "Being bought on a mortgage/ bank loan",
    W3hous12HH == 3 ~ "Shared ownership (owns & rents property)",
    W3hous12HH == 4 ~ "Rented from a Council or New Town",
    W3hous12HH == 5 ~ "Rented from a Housing Association",
    W3hous12HH == 6 ~ "Rented privately",
    W3hous12HH == 7 ~ "Rent free",
    W3hous12HH == 8 ~ "Some other arrangement",
    W3hous12HH == -92 ~ "Refusal",
    W3hous12HH == -91 ~ "Item not applicable",
    W3hous12HH == -1 ~ "Don't know/insufficient information",
    W3hous12HH == -999 ~ "Script error/information lost",
    TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
  ))

merged_data <- merged_data %>% 
  mutate(hownteen17 = case_when(
    W4Hous12HH == 1 ~ "Owned outright",
    W4Hous12HH == 2 ~ "Being bought on a mortgage or bank loan",
    W4Hous12HH == 3 ~ "Shared ownership (owns & rents property)",
    W4Hous12HH == 4 ~ "Rented from a Council or New Town",
    W4Hous12HH == 5 ~ "Rented from a Housing Association",
    W4Hous12HH == 6 ~ "Rented privately",
    W4Hous12HH == 7 ~ "Rent free",
    W4Hous12HH == 8 ~ "Some other arrangement",
    W4Hous12HH == -92 ~ "Refusal",
    W4Hous12HH == -91 ~ "Item not applicable",
    W4Hous12HH == -1 ~ "Don't know/insufficient information",
    W4Hous12HH == -999 ~ "Script error/information lost",
    TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
  ))

merged_data <- merged_data %>% 
  mutate(hownteen18 = case_when(
    W5Hous12BHH == 1 ~ "Owned outright",
    W5Hous12BHH == 2 ~ "Being bought on a mortgage/ bank loan",
    W5Hous12BHH == 3 ~ "Shared ownership (owns and rents the property)",
    W5Hous12BHH == 4 ~ "Some other arrangement",
    W5Hous12CHH == 1 ~ "Rented from a Council or New town",
    W5Hous12CHH == 2 ~ "Rented from a Housing Association",
    W5Hous12CHH == 3 ~ "Rented privately",
    W5Hous12CHH == 4 ~ "Rent free",
    W5Hous12CHH == 5 ~ "Some other arrangement",
    W5Hous12BHH == -92 ~ "Refusal",
    W5Hous12BHH == -91 ~ "Item not applicable",
    W5Hous12BHH == -1 ~ "Don't know/insufficient information",
    W5Hous12BHH == -999 ~ "Script error/information lost",
    TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
  ))

merged_data <- merged_data %>% 
  mutate(hownteen19 = case_when(
    W6Hous12bYP == 1 ~ "Owned outright",
    W6Hous12bYP == 2 ~ "Being bought on a mortgage/ bank loan",
    W6Hous12bYP == 3 ~ "Shared ownership",
    W6Hous12bYP == 4 ~ "Some other arrangement",
    W6Hous12cYP == 1 ~ "Rented from a Council or New town",
    W6Hous12cYP == 2 ~ "Rented from a Housing Association",
    W6Hous12cYP == 3 ~ "Rented privately",
    W6Hous12cYP == 4 ~ "Rent free",
    W6Hous12cYP == 5 ~ "Some other arrangement",
    W6Hous12bYP == -92 ~ "Refusal",
    W6Hous12bYP == -91 ~ "Item not applicable",
    W6Hous12bYP == -1 ~ "Don't know/insufficient information",
    W6Hous12bYP == -999 ~ "Script error/information lost",
    TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
  ))

merged_data <- merged_data %>% 
  mutate(hownteen20 = case_when(
    W7Hous12bYP == 1 ~ "Owned outright",
    W7Hous12bYP == 2 ~ "Being bought on a mortgage/ bank loan",
    W7Hous12bYP == 3 ~ "Shared ownership (you own and rent the property)",
    W7Hous12bYP == 4 ~ "Some other arrangement",
    W7Hous12cYP == 1 ~ "Rented from a Council or New town",
    W7Hous12cYP == 2 ~ "Rented from a Housing Association",
    W7Hous12cYP == 3 ~ "Rented privately",
    W7Hous12cYP == 4 ~ "Rent free",
    W7Hous12cYP == 5 ~ "Some other arrangement",
    W7Hous12bYP == -92 ~ "Refusal",
    W7Hous12bYP == -91 ~ "Item not applicable",
    W7Hous12bYP == -1 ~ "Don't know/insufficient information",
    W7Hous12bYP == -999 ~ "Script error/information lost",
    TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
  ))

# Create collapsed variables per age wave
merged_data <- merged_data %>% 
  mutate(hown14 = case_when(
    W1hous12HH == 1 ~ "Owned outright",
    W1hous12HH == 2 ~ "Owned, buying with help of mortgage/loan",
    W1hous12HH == 3 ~ "Part rent, part mortgage",
    W1hous12HH == 4 ~ "Rent it",
    W1hous12HH == 5 ~ "Rent it",
    W1hous12HH == 6 ~ "Rent it",
    W1hous12HH == 7 ~ "live rent-free",
    W1hous12HH == 8 ~ "Other",
    W1hous12HH == -92 ~ "Refusal",
    W1hous12HH == -91 ~ "Item not applicable",
    W1hous12HH == -1 ~ "Don't know/insufficient information",
    W1hous12HH == -999 ~ "Script error/information lost",
    TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
  ))

merged_data <- merged_data %>% 
  mutate(hown15 = case_when(
    W2Hous12HH == 1 ~ "Owned outright",
    W2Hous12HH == 2 ~ "Owned, buying with help of mortgage/loan",
    W2Hous12HH == 3 ~ "Part rent, part mortgage",
    W2Hous12HH == 4 ~ "Rent it",
    W2Hous12HH == 5 ~ "Rent it",
    W2Hous12HH == 6 ~ "Rent it",
    W2Hous12HH == 7 ~ "live rent-free",
    W2Hous12HH == 8 ~ "Other",
    W2Hous12HH == -92 ~ "Refusal",
    W2Hous12HH == -91 ~ "Item not applicable",
    W2Hous12HH == -1 ~ "Don't know/insufficient information",
    W2Hous12HH == -998 ~ "Script error/information lost",
    TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
  ))

merged_data <- merged_data %>% 
  mutate(hown16 = case_when(
    W3hous12HH == 1 ~ "Owned outright",
    W3hous12HH == 2 ~ "Owned, buying with help of mortgage/loan",
    W3hous12HH == 3 ~ "Part rent, part mortgage",
    W3hous12HH == 4 ~ "Rent it",
    W3hous12HH == 5 ~ "Rent it",
    W3hous12HH == 6 ~ "Rent it",
    W3hous12HH == 7 ~ "live rent-free",
    W3hous12HH == 8 ~ "Other",
    W3hous12HH == -92 ~ "Refusal",
    W3hous12HH == -91 ~ "Item not applicable",
    W3hous12HH == -1 ~ "Don't know/insufficient information",
    W3hous12HH == -999 ~ "Script error/information lost",
    TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
  ))

merged_data <- merged_data %>% 
  mutate(hown17 = case_when(
    W4Hous12HH == 1 ~ "Owned outright",
    W4Hous12HH == 2 ~ "Owned, buying with help of mortgage/loan",
    W4Hous12HH == 3 ~ "Part rent, part mortgage",
    W4Hous12HH == 4 ~ "Rent it",
    W4Hous12HH == 5 ~ "Rent it",
    W4Hous12HH == 6 ~ "Rent it",
    W4Hous12HH == 7 ~ "live rent-free",
    W4Hous12HH == 8 ~ "Other",
    W4Hous12HH == -92 ~ "Refusal",
    W4Hous12HH == -91 ~ "Item not applicable",
    W4Hous12HH == -1 ~ "Don't know/insufficient information",
    W4Hous12HH == -999 ~ "Script error/information lost",
    TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
  ))

merged_data <- merged_data %>% 
  mutate(hown18 = case_when(
    W5Hous12BHH == 1 ~ "Owned outright",
    W5Hous12BHH == 2 ~ "Owned, buying with help of mortgage/loan",
    W5Hous12BHH == 3 ~ "Part rent, part mortgage",
    W5Hous12BHH == 4 ~ "Other",
    W5Hous12CHH == 1 ~ "Rent it",
    W5Hous12CHH == 2 ~ "Rent it",
    W5Hous12CHH == 3 ~ "Rent it",
    W5Hous12CHH == 4 ~ "live rent-free",
    W5Hous12CHH == 5 ~ "Other",
    W5Hous12BHH == -92 ~ "Refusal",
    W5Hous12BHH == -91 ~ "Item not applicable",
    W5Hous12BHH == -1 ~ "Don't know/insufficient information",
    W5Hous12BHH == -999 ~ "Script error/information lost",
    TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
  ))

merged_data <- merged_data %>% 
  mutate(hown19 = case_when(
    W6Hous12bYP == 1 ~ "Owned outright",
    W6Hous12bYP == 2 ~ "Owned, buying with help of mortgage/loan",
    W6Hous12bYP == 3 ~ "Part rent, part mortgage",
    W6Hous12bYP == 4 ~ "Other",
    W6Hous12cYP == 1 ~ "Rent it",
    W6Hous12cYP == 2 ~ "Rent it",
    W6Hous12cYP == 3 ~ "Rent it",
    W6Hous12cYP == 4 ~ "live rent-free",
    W6Hous12cYP == 5 ~ "Other",
    W6Hous12bYP == -92 ~ "Refusal",
    W6Hous12bYP == -91 ~ "Item not applicable",
    W6Hous12bYP == -1 ~ "Don't know/insufficient information",
    W6Hous12bYP == -999 ~ "Script error/information lost",
    TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
  ))

merged_data <- merged_data %>% 
  mutate(hown20 = case_when(
    W7Hous12bYP == 1 ~ "Owned outright",
    W7Hous12bYP == 2 ~ "Owned, buying with help of mortgage/loan",
    W7Hous12bYP == 3 ~ "Part rent, part mortgage",
    W7Hous12bYP == 4 ~ "Other",
    W7Hous12cYP == 1 ~ "Rent it",
    W7Hous12cYP == 2 ~ "Rent it",
    W7Hous12cYP == 3 ~ "Rent it",
    W7Hous12cYP == 4 ~ "live rent-free",
    W7Hous12cYP == 5 ~ "Other",
    W7Hous12bYP == -92 ~ "Refusal",
    W7Hous12bYP == -91 ~ "Item not applicable",
    W7Hous12bYP == -1 ~ "Don't know/insufficient information",
    W7Hous12bYP == -999 ~ "Script error/information lost",
    TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
  ))

merged_data <- merged_data %>% 
  mutate(hown25 = case_when(
    W8TENURE == 1 ~ "Owned outright",
    W8TENURE == 2 ~ "Owned, buying with help of mortgage/loan",
    W8TENURE == 3 ~ "Part rent, part mortgage",
    W8TENURE == 4 ~ "Rent it",
    W8TENURE == 5 ~ "live rent-free",
    W8TENURE == 6 ~ "Other",
    W8TENURE == 7 ~ "Other",
    W8TENURE == -9 ~ "Refusal",
    W8TENURE == -8 ~ "Don't know/insufficient information",
    W8TENURE == -1 ~ "Item not applicable",
    TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
  ))

merged_data <- merged_data %>% 
  mutate(hown32 = case_when(
    W9DTENURE == 1 ~ "Owned outright",
    W9DTENURE == 2 ~ "Owned, buying with help of mortgage/loan",
    W9DTENURE == 3 ~ "Part rent, part mortgage",
    W9DTENURE == 4 ~ "Rent it",
    W9DTENURE == 5 ~ "live rent-free",
    W9DTENURE == 6 ~ "Other",
    W9DTENURE == 7 ~ "Other",
    W9DTENURE == -8 ~ "Refusal",
    W9DTENURE == -9 ~ "Don't know/insufficient information",
    W9DTENURE == -1 ~ "Item not applicable",
    TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
  ))

# Select only the required variables
cleaned_data <- merged_data %>% 
  select(NSID, hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20, hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32)

# Write the cleaned dataset to a single CSV
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)
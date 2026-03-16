library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Read all files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Process wave1 (age 14)
wave1 <- wave1 %>%
  mutate(W1hous12HH = case_when(
    W1hous12HH == -999 ~ -3,
    W1hous12HH == -92 ~ -9,
    W1hous12HH == -91 ~ -1,
    W1hous12HH == -1 ~ -8,
    TRUE ~ W1hous12HH
  )) %>%
  mutate(hownteen14 = case_when(
    W1hous12HH == 1 ~ "Owned outright",
    W1hous12HH == 2 ~ "Being bought on a mortgage/bank loan",
    W1hous12HH == 3 ~ "Shared ownership (owns & rents property)",
    W1hous12HH == 4 ~ "Rented from a Council or New Town",
    W1hous12HH == 5 ~ "Rented from a Housing Association",
    W1hous12HH == 6 ~ "Rented privately",
    W1hous12HH == 7 ~ "Rent free",
    W1hous12HH == 8 ~ "Some other arrangement",
    W1hous12HH == -9 ~ "Refusal",
    W1hous12HH == -8 ~ "Don't know/insufficient information",
    W1hous12HH == -1 ~ "Item not applicable",
    W1hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
    TRUE ~ NA_character_
  ))

# Process wave2 (age 15)
wave2 <- wave2 %>%
  mutate(W2Hous12HH = case_when(
    W2Hous12HH == -998 ~ -2,
    W2Hous12HH == -997 ~ -2,
    W2Hous12HH == -995 ~ -3,
    W2Hous12HH == -99 ~ -3,
    W2Hous12HH == -92 ~ -9,
    W2Hous12HH == -91 ~ -1,
    W2Hous12HH == -1 ~ -8,
    TRUE ~ W2Hous12HH
  )) %>%
  mutate(hownteen15 = case_when(
    W2Hous12HH == 1 ~ "Owned outright",
    W2Hous12HH == 2 ~ "Being bought on a mortgage/bank loan",
    W2Hous12HH == 3 ~ "Shared ownership (owns & rents property)",
    W2Hous12HH == 4 ~ "Rented from a Council or New Town",
    W2Hous12HH == 5 ~ "Rented from a Housing Association",
    W2Hous12HH == 6 ~ "Rented privately",
    W2Hous12HH == 7 ~ "Rent free",
    W2Hous12HH == 8 ~ "Some other arrangement",
    W2Hous12HH == -9 ~ "Refusal",
    W2Hous12HH == -8 ~ "Don't know/insufficient information",
    W2Hous12HH == -1 ~ "Item not applicable",
    W2Hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
    W2Hous12HH == -2 ~ "Script error/information lost",
    TRUE ~ NA_character_
  ))

# Process wave3 (age 16)
wave3 <- wave3 %>%
  mutate(W3hous12HH = case_when(
    W3hous12HH == -999 ~ -3,
    W3hous12HH == -99 ~ -3,
    W3hous12HH == -92 ~ -9,
    W3hous12HH == -91 ~ -1,
    W3hous12HH == -1 ~ -8,
    TRUE ~ W3hous12HH
  )) %>%
  mutate(hownteen16 = case_when(
    W3hous12HH == 1 ~ "Owned outright",
    W3hous12HH == 2 ~ "Being bought on a mortgage/bank loan",
    W3hous12HH == 3 ~ "Shared ownership (owns & rents property)",
    W3hous12HH == 4 ~ "Rented from a Council or New Town",
    W3hous12HH == 5 ~ "Rented from a Housing Association",
    W3hous12HH == 6 ~ "Rented privately",
    W3hous12HH == 7 ~ "Rent free",
    W3hous12HH == 8 ~ "Some other arrangement",
    W3hous12HH == -9 ~ "Refusal",
    W3hous12HH == -8 ~ "Don't know/insufficient information",
    W3hous12HH == -1 ~ "Item not applicable",
    W3hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
    TRUE ~ NA_character_
  ))

# Process wave4 (age 17)
wave4 <- wave4 %>%
  mutate(W4Hous12HH = case_when(
    W4Hous12HH == -999 ~ -3,
    W4Hous12HH == -997 ~ -2,
    W4Hous12HH == -92 ~ -9,
    W4Hous12HH == -91 ~ -1,
    W4Hous12HH == -1 ~ -8,
    TRUE ~ W4Hous12HH
  )) %>%
  mutate(hownteen17 = case_when(
    W4Hous12HH == 1 ~ "Owned outright",
    W4Hous12HH == 2 ~ "Being bought on a mortgage/bank loan",
    W4Hous12HH == 3 ~ "Shared ownership (owns & rents property)",
    W4Hous12HH == 4 ~ "Rented from a Council or New Town",
    W4Hous12HH == 5 ~ "Rented from a Housing Association",
    W4Hous12HH == 6 ~ "Rented privately",
    W4Hous12HH == 7 ~ "Rent free",
    W4Hous12HH == 8 ~ "Some other arrangement",
    W4Hous12HH == -9 ~ "Refusal",
    W4Hous12HH == -8 ~ "Don't know/insufficient information",
    W4Hous12HH == -1 ~ "Item not applicable",
    W4Hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
    W4Hous12HH == -2 ~ "Script error/information lost",
    TRUE ~ NA_character_
  ))

# Process wave5 (age 18)
wave5 <- wave5 %>%
  mutate(W5Hous12HH = case_when(
    W5Hous12HH == -999 ~ -3,
    W5Hous12HH == -92 ~ -9,
    W5Hous12HH == -91 ~ -1,
    W5Hous12HH == -1 ~ -8,
    TRUE ~ W5Hous12HH
  )) %>%
  mutate(W5Hous12BHH = case_when(
    W5Hous12BHH == -999 ~ -3,
    W5Hous12BHH == -92 ~ -9,
    W5Hous12BHH == -91 ~ -1,
    W5Hous12BHH == -1 ~ -8,
    TRUE ~ W5Hous12BHH
  )) %>%
  mutate(W5Hous12CHH = case_when(
    W5Hous12CHH == -999 ~ -3,
    W5Hous12CHH == -92 ~ -9,
    W5Hous12CHH == -91 ~ -1,
    W5Hous12CHH == -1 ~ -8,
    TRUE ~ W5Hous12CHH
  )) %>%
  mutate(hownteen18 = case_when(
    W5Hous12HH == 1 & W5Hous12BHH == 1 ~ "Owned outright",
    W5Hous12HH == 1 & W5Hous12BHH == 2 ~ "Being bought on a mortgage/bank loan",
    W5Hous12HH == 1 & W5Hous12BHH == 3 ~ "Shared ownership (owns & rents property)",
    W5Hous12HH == 1 & W5Hous12BHH == 4 ~ "Some other arrangement",
    W5Hous12HH == 2 & W5Hous12CHH == 1 ~ "Rented from a Council or New Town",
    W5Hous12HH == 2 & W5Hous12CHH == 2 ~ "Rented from a Housing Association",
    W5Hous12HH == 2 & W5Hous12CHH == 3 ~ "Rented privately",
    W5Hous12HH == 2 & W5Hous12CHH == 4 ~ "Rent free",
    W5Hous12HH == 2 & W5Hous12CHH == 5 ~ "Some other arrangement",
    W5Hous12HH == 3 ~ "Some other arrangement",
    W5Hous12HH == -9 ~ "Refusal",
    W5Hous12HH == -8 ~ "Don't know/insufficient information",
    W5Hous12HH == -1 ~ "Item not applicable",
    W5Hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
    TRUE ~ NA_character_
  ))

# Process wave6 (age 19)
wave6 <- wave6 %>%
  mutate(W6Hous12YP = case_when(
    W6Hous12YP == -92 ~ -9,
    W6Hous12YP == -91 ~ -1,
    W6Hous12YP == -1 ~ -8,
    TRUE ~ W6Hous12YP
  )) %>%
  mutate(W6Hous12bYP = case_when(
    W6Hous12bYP == -92 ~ -9,
    W6Hous12bYP == -91 ~ -1,
    W6Hous12bYP == -1 ~ -8,
    TRUE ~ W6Hous12bYP
  )) %>%
  mutate(W6Hous12cYP = case_when(
    W6Hous12cYP == -92 ~ -9,
    W6Hous12cYP == -91 ~ -1,
    W6Hous12cYP == -1 ~ -8,
    TRUE ~ W6Hous12cYP
  )) %>%
  mutate(hownteen19 = case_when(
    W6Hous12YP == 1 & W6Hous12bYP == 1 ~ "Owned outright",
    W6Hous12YP == 1 & W6Hous12bYP == 2 ~ "Being bought on a mortgage/bank loan",
    W6Hous12YP == 1 & W6Hous12bYP == 3 ~ "Shared ownership (owns & rents property)",
    W6Hous12YP == 1 & W6Hous12bYP == 4 ~ "Some other arrangement",
    W6Hous12YP == 2 & W6Hous12cYP == 1 ~ "Rented from a Council or New Town",
    W6Hous12YP == 2 & W6Hous12cYP == 2 ~ "Rented from a Housing Association",
    W6Hous12YP == 2 & W6Hous12cYP == 3 ~ "Rented privately",
    W6Hous12YP == 2 & W6Hous12cYP == 4 ~ "Rent free",
    W6Hous12YP == 2 & W6Hous12cYP == 5 ~ "Some other arrangement",
    W6Hous12YP == 3 ~ "Some other arrangement",
    W6Hous12YP == -9 ~ "Refusal",
    W6Hous12YP == -8 ~ "Don't know/insufficient information",
    W6Hous12YP == -1 ~ "Item not applicable",
    W6Hous12YP == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
    TRUE ~ NA_character_
  ))

# Process wave7 (age 20)
wave7 <- wave7 %>%
  mutate(W7Hous12YP = case_when(
    W7Hous12YP == -92 ~ -9,
    W7Hous12YP == -91 ~ -1,
    W7Hous12YP == -1 ~ -8,
    TRUE ~ W7Hous12YP
  )) %>%
  mutate(W7Hous12bYP = case_when(
    W7Hous12bYP == -92 ~ -9,
    W7Hous12bYP == -91 ~ -1,
    W7Hous12bYP == -1 ~ -8,
    TRUE ~ W7Hous12bYP
  )) %>%
  mutate(W7Hous12cYP = case_when(
    W7Hous12cYP == -92 ~ -9,
    W7Hous12cYP == -91 ~ -1,
    W7Hous12cYP == -1 ~ -8,
    TRUE ~ W7Hous12cYP
  )) %>%
  mutate(hownteen20 = case_when(
    W7Hous12YP == 1 & W7Hous12bYP == 1 ~ "Owned outright",
    W7Hous12YP == 1 & W7Hous12bYP == 2 ~ "Being bought on a mortgage/bank loan",
    W7Hous12YP == 1 & W7Hous12bYP == 3 ~ "Shared ownership (owns & rents property)",
    W7Hous12YP == 1 & W7Hous12bYP == 4 ~ "Some other arrangement",
    W7Hous12YP == 2 & W7Hous12cYP == 1 ~ "Rented from a Council or New Town",
    W7Hous12YP == 2 & W7Hous12cYP == 2 ~ "Rented from a Housing Association",
    W7Hous12YP == 2 & W7Hous12cYP == 3 ~ "Rented privately",
    W7Hous12YP == 2 & W7Hous12cYP == 4 ~ "Rent free",
    W7Hous12YP == 2 & W7Hous12cYP == 5 ~ "Some other arrangement",
    W7Hous12YP == 3 ~ "Some other arrangement",
    W7Hous12YP == -9 ~ "Refusal",
    W7Hous12YP == -8 ~ "Don't know/insufficient information",
    W7Hous12YP == -1 ~ "Item not applicable",
    W7Hous12YP == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
    TRUE ~ NA_character_
  ))

# Process wave8 (age 25)
wave8 <- wave8 %>%
  mutate(W8TENURE = case_when(
    W8TENURE == -9 ~ -9,
    W8TENURE == -8 ~ -8,
    W8TENURE == -1 ~ -1,
    TRUE ~ W8TENURE
  )) %>%
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
    TRUE ~ NA_character_
  ))

# Process wave9 (age 32)
wave9 <- wave9 %>%
  mutate(W9DTENURE = case_when(
    W9DTENURE == -8 ~ -8,
    TRUE ~ W9DTENURE
  )) %>%
  mutate(hown32 = case_when(
    W9DTENURE == 1 ~ "Owned outright",
    W9DTENURE == 2 ~ "Owned, buying with help of mortgage/loan",
    W9DTENURE == 3 ~ "Part rent, part mortgage",
    W9DTENURE == 4 ~ "Rent it",
    W9DTENURE == 5 ~ "live rent-free",
    W9DTENURE == 6 ~ "Other",
    W9DTENURE == 7 ~ "Other",
    W9DTENURE == -8 ~ "Don't know/insufficient information",
    TRUE ~ NA_character_
  ))

# Merge all data frames
merged_data <- wave1 %>%
  select(NSID, hownteen14) %>%
  full_join(wave2 %>% select(NSID, hownteen15), by = "NSID") %>%
  full_join(wave3 %>% select(NSID, hownteen16), by = "NSID") %>%
  full_join(wave4 %>% select(NSID, hownteen17), by = "NSID") %>%
  full_join(wave5 %>% select(NSID, hownteen18), by = "NSID") %>%
  full_join(wave6 %>% select(NSID, hownteen19), by = "NSID") %>%
  full_join(wave7 %>% select(NSID, hownteen20), by = "NSID") %>%
  full_join(wave8 %>% select(NSID, hown25), by = "NSID") %>%
  full_join(wave9 %>% select(NSID, hown32), by = "NSID")

# Create collapsed variables for ages 14-20
merged_data <- merged_data %>%
  mutate(hown14 = case_when(
    hownteen14 == "Owned outright" ~ "Owned outright",
    hownteen14 == "Being bought on a mortgage/bank loan" ~ "Owned, buying with help of mortgage/loan",
    hownteen14 == "Shared ownership (owns & rents property)" ~ "Part rent, part mortgage",
    hownteen14 == "Rented from a Council or New Town" ~ "Rent it",
    hownteen14 == "Rented from a Housing Association" ~ "Rent it",
    hownteen14 == "Rented privately" ~ "Rent it",
    hownteen14 == "Rent free" ~ "live rent-free",
    hownteen14 == "Some other arrangement" ~ "Other",
    hownteen14 == "Refusal" ~ "Refusal",
    hownteen14 == "Don't know/insufficient information" ~ "Don't know/insufficient information",
    hownteen14 == "Item not applicable" ~ "Item not applicable",
    hownteen14 == "Not asked at the fieldwork stage/participated/interviewed" ~ "Not asked at the fieldwork stage/participated/interviewed",
    hownteen14 == "Script error/information lost" ~ "Script error/information lost",
    TRUE ~ NA_character_
  )) %>%
  mutate(hown15 = case_when(
    hownteen15 == "Owned outright" ~ "Owned outright",
    hownteen15 == "Being bought on a mortgage/bank loan" ~ "Owned, buying with help of mortgage/loan",
    hownteen15 == "Shared ownership (owns & rents property)" ~ "Part rent, part mortgage",
    hownteen15 == "Rented from a Council or New Town" ~ "Rent it",
    hownteen15 == "Rented from a Housing Association" ~ "Rent it",
    hownteen15 == "Rented privately" ~ "Rent it",
    hownteen15 == "Rent free" ~ "live rent-free",
    hownteen15 == "Some other arrangement" ~ "Other",
    hownteen15 == "Refusal" ~ "Refusal",
    hownteen15 == "Don't know/insufficient information" ~ "Don't know/insufficient information",
    hownteen15 == "Item not applicable" ~ "Item not applicable",
    hownteen15 == "Not asked at the fieldwork stage/participated/interviewed" ~ "Not asked at the fieldwork stage/participated/interviewed",
    hownteen15 == "Script error/information lost" ~ "Script error/information lost",
    TRUE ~ NA_character_
  )) %>%
  mutate(hown16 = case_when(
    hownteen16 == "Owned outright" ~ "Owned outright",
    hownteen16 == "Being bought on a mortgage/bank loan" ~ "Owned, buying with help of mortgage/loan",
    hownteen16 == "Shared ownership (owns & rents property)" ~ "Part rent, part mortgage",
    hownteen16 == "Rented from a Council or New Town" ~ "Rent it",
    hownteen16 == "Rented from a Housing Association" ~ "Rent it",
    hownteen16 == "Rented privately" ~ "Rent it",
    hownteen16 == "Rent free" ~ "live rent-free",
    hownteen16 == "Some other arrangement" ~ "Other",
    hownteen16 == "Refusal" ~ "Refusal",
    hownteen16 == "Don't know/insufficient information" ~ "Don't know/insufficient information",
    hownteen16 == "Item not applicable" ~ "Item not applicable",
    hownteen16 == "Not asked at the fieldwork stage/participated/interviewed" ~ "Not asked at the fieldwork stage/participated/interviewed",
    TRUE ~ NA_character_
  )) %>%
  mutate(hown17 = case_when(
    hownteen17 == "Owned outright" ~ "Owned outright",
    hownteen17 == "Being bought on a mortgage/bank loan" ~ "Owned, buying with help of mortgage/loan",
    hownteen17 == "Shared ownership (owns & rents property)" ~ "Part rent, part mortgage",
    hownteen17 == "Rented from a Council or New Town" ~ "Rent it",
    hownteen17 == "Rented from a Housing Association" ~ "Rent it",
    hownteen17 == "Rented privately" ~ "Rent it",
    hownteen17 == "Rent free" ~ "live rent-free",
    hownteen17 == "Some other arrangement" ~ "Other",
    hownteen17 == "Refusal" ~ "Refusal",
    hownteen17 == "Don't know/insufficient information" ~ "Don't know/insufficient information",
    hownteen17 == "Item not applicable" ~ "Item not applicable",
    hownteen17 == "Not asked at the fieldwork stage/participated/interviewed" ~ "Not asked at the fieldwork stage/participated/interviewed",
    hownteen17 == "Script error/information lost" ~ "Script error/information lost",
    TRUE ~ NA_character_
  )) %>%
  mutate(hown18 = case_when(
    hownteen18 == "Owned outright" ~ "Owned outright",
    hownteen18 == "Being bought on a mortgage/bank loan" ~ "Owned, buying with help of mortgage/loan",
    hownteen18 == "Shared ownership (owns & rents property)" ~ "Part rent, part mortgage",
    hownteen18 == "Rented from a Council or New Town" ~ "Rent it",
    hownteen18 == "Rented from a Housing Association" ~ "Rent it",
    hownteen18 == "Rented privately" ~ "Rent it",
    hownteen18 == "Rent free" ~ "live rent-free",
    hownteen18 == "Some other arrangement" ~ "Other",
    hownteen18 == "Refusal" ~ "Refusal",
    hownteen18 == "Don't know/insufficient information" ~ "Don't know/insufficient information",
    hownteen18 == "Item not applicable" ~ "Item not applicable",
    hownteen18 == "Not asked at the fieldwork stage/participated/interviewed" ~ "Not asked at the fieldwork stage/participated/interviewed",
    TRUE ~ NA_character_
  )) %>%
  mutate(hown19 = case_when(
    hownteen19 == "Owned outright" ~ "Owned outright",
    hownteen19 == "Being bought on a mortgage/bank loan" ~ "Owned, buying with help of mortgage/loan",
    hownteen19 == "Shared ownership (owns & rents property)" ~ "Part rent, part mortgage",
    hownteen19 == "Rented from a Council or New Town" ~ "Rent it",
    hownteen19 == "Rented from a Housing Association" ~ "Rent it",
    hownteen19 == "Rented privately" ~ "Rent it",
    hownteen19 == "Rent free" ~ "live rent-free",
    hownteen19 == "Some other arrangement" ~ "Other",
    hownteen19 == "Refusal" ~ "Refusal",
    hownteen19 == "Don't know/insufficient information" ~ "Don't know/insufficient information",
    hownteen19 == "Item not applicable" ~ "Item not applicable",
    hownteen19 == "Not asked at the fieldwork stage/participated/interviewed" ~ "Not asked at the fieldwork stage/participated/interviewed",
    TRUE ~ NA_character_
  )) %>%
  mutate(hown20 = case_when(
    hownteen20 == "Owned outright" ~ "Owned outright",
    hownteen20 == "Being bought on a mortgage/bank loan" ~ "Owned, buying with help of mortgage/loan",
    hownteen20 == "Shared ownership (owns & rents property)" ~ "Part rent, part mortgage",
    hownteen20 == "Rented from a Council or New Town" ~ "Rent it",
    hownteen20 == "Rented from a Housing Association" ~ "Rent it",
    hownteen20 == "Rented privately" ~ "Rent it",
    hownteen20 == "Rent free" ~ "live rent-free",
    hownteen20 == "Some other arrangement" ~ "Other",
    hownteen20 == "Refusal" ~ "Refusal",
    hownteen20 == "Don't know/insufficient information" ~ "Don't know/insufficient information",
    hownteen20 == "Item not applicable" ~ "Item not applicable",
    hownteen20 == "Not asked at the fieldwork stage/participated/interviewed" ~ "Not asked at the fieldwork stage/participated/interviewed",
    TRUE ~ NA_character_
  ))

# Select only required columns
cleaned_data <- merged_data %>%
  select(NSID, hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32, hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20)

# Write to CSV
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)
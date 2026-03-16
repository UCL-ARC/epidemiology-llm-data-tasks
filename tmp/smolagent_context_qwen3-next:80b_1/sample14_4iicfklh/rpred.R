library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all data frames by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Process wave1 (age 14)
merged_data <- merged_data %>%
  mutate(
    W1hous12HH = case_when(
      W1hous12HH == -999 ~ -3,
      W1hous12HH == -92 ~ -9,
      W1hous12HH == -91 ~ -1,
      W1hous12HH == -1 ~ -8,
      TRUE ~ W1hous12HH
    ),
    hownteen14 = case_when(
      W1hous12HH == 1 ~ "Owned outright",
      W1hous12HH == 2 ~ "Being bought on a mortgage/bank loan",
      W1hous12HH == 3 ~ "Shared ownership (owns & rents property)",
      W1hous12HH == 4 ~ "Rented from a Council or New Town",
      W1hous12HH == 5 ~ "Rented from a Housing Association",
      W1hous12HH == 6 ~ "Rented privately",
      W1hous12HH == 7 ~ "Rent free",
      W1hous12HH == 8 ~ "Some other arrangement",
      W1hous12HH == -1 ~ "Item not applicable",
      W1hous12HH == -2 ~ "Script error/information lost",
      W1hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W1hous12HH == -8 ~ "Don't know/insufficient information",
      W1hous12HH == -9 ~ "Refusal",
      TRUE ~ NA_character_
    ),
    hown14 = case_when(
      W1hous12HH == 1 ~ "Owned outright",
      W1hous12HH == 2 ~ "Owned, buying with help of mortgage/loan",
      W1hous12HH == 3 ~ "Part rent, part mortgage",
      W1hous12HH == 4 ~ "Rent it",
      W1hous12HH == 5 ~ "Rent it",
      W1hous12HH == 6 ~ "Rent it",
      W1hous12HH == 7 ~ "live rent-free",
      W1hous12HH == 8 ~ "Other",
      W1hous12HH == -1 ~ "Item not applicable",
      W1hous12HH == -2 ~ "Script error/information lost",
      W1hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W1hous12HH == -8 ~ "Don't know/insufficient information",
      W1hous12HH == -9 ~ "Refusal",
      TRUE ~ NA_character_
    )
  )

# Process wave2 (age 15)
merged_data <- merged_data %>%
  mutate(
    W2Hous12HH = case_when(
      W2Hous12HH == -998 ~ -2,
      W2Hous12HH == -997 ~ -2,
      W2Hous12HH == -995 ~ -3,
      W2Hous12HH == -99 ~ -3,
      W2Hous12HH == -92 ~ -9,
      W2Hous12HH == -91 ~ -1,
      W2Hous12HH == -1 ~ -8,
      TRUE ~ W2Hous12HH
    ),
    hownteen15 = case_when(
      W2Hous12HH == 1 ~ "Owned outright",
      W2Hous12HH == 2 ~ "Being bought on a mortgage/bank loan",
      W2Hous12HH == 3 ~ "Shared ownership (owns & rents property)",
      W2Hous12HH == 4 ~ "Rented from a Council or New Town",
      W2Hous12HH == 5 ~ "Rented from a Housing Association",
      W2Hous12HH == 6 ~ "Rented privately",
      W2Hous12HH == 7 ~ "Rent free",
      W2Hous12HH == 8 ~ "Some other arrangement",
      W2Hous12HH == -1 ~ "Item not applicable",
      W2Hous12HH == -2 ~ "Script error/information lost",
      W2Hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W2Hous12HH == -8 ~ "Don't know/insufficient information",
      W2Hous12HH == -9 ~ "Refusal",
      TRUE ~ NA_character_
    ),
    hown15 = case_when(
      W2Hous12HH == 1 ~ "Owned outright",
      W2Hous12HH == 2 ~ "Owned, buying with help of mortgage/loan",
      W2Hous12HH == 3 ~ "Part rent, part mortgage",
      W2Hous12HH == 4 ~ "Rent it",
      W2Hous12HH == 5 ~ "Rent it",
      W2Hous12HH == 6 ~ "Rent it",
      W2Hous12HH == 7 ~ "live rent-free",
      W2Hous12HH == 8 ~ "Other",
      W2Hous12HH == -1 ~ "Item not applicable",
      W2Hous12HH == -2 ~ "Script error/information lost",
      W2Hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W2Hous12HH == -8 ~ "Don't know/insufficient information",
      W2Hous12HH == -9 ~ "Refusal",
      TRUE ~ NA_character_
    )
  )

# Process wave3 (age 16)
merged_data <- merged_data %>%
  mutate(
    W3hous12HH = case_when(
      W3hous12HH == -999 ~ -3,
      W3hous12HH == -99 ~ -2,
      W3hous12HH == -92 ~ -9,
      W3hous12HH == -91 ~ -1,
      W3hous12HH == -1 ~ -8,
      TRUE ~ W3hous12HH
    ),
    hownteen16 = case_when(
      W3hous12HH == 1 ~ "Owned outright",
      W3hous12HH == 2 ~ "Being bought on a mortgage/bank loan",
      W3hous12HH == 3 ~ "Shared ownership (owns & rents property)",
      W3hous12HH == 4 ~ "Rented from a Council or New Town",
      W3hous12HH == 5 ~ "Rented from a Housing Association",
      W3hous12HH == 6 ~ "Rented privately",
      W3hous12HH == 7 ~ "Rent free",
      W3hous12HH == 8 ~ "Some other arrangement",
      W3hous12HH == -1 ~ "Item not applicable",
      W3hous12HH == -2 ~ "Script error/information lost",
      W3hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W3hous12HH == -8 ~ "Don't know/insufficient information",
      W3hous12HH == -9 ~ "Refusal",
      TRUE ~ NA_character_
    ),
    hown16 = case_when(
      W3hous12HH == 1 ~ "Owned outright",
      W3hous12HH == 2 ~ "Owned, buying with help of mortgage/loan",
      W3hous12HH == 3 ~ "Part rent, part mortgage",
      W3hous12HH == 4 ~ "Rent it",
      W3hous12HH == 5 ~ "Rent it",
      W3hous12HH == 6 ~ "Rent it",
      W3hous12HH == 7 ~ "live rent-free",
      W3hous12HH == 8 ~ "Other",
      W3hous12HH == -1 ~ "Item not applicable",
      W3hous12HH == -2 ~ "Script error/information lost",
      W3hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W3hous12HH == -8 ~ "Don't know/insufficient information",
      W3hous12HH == -9 ~ "Refusal",
      TRUE ~ NA_character_
    )
  )

# Process wave4 (age 17)
merged_data <- merged_data %>%
  mutate(
    W4Hous12HH = case_when(
      W4Hous12HH == -999 ~ -3,
      W4Hous12HH == -997 ~ -2,
      W4Hous12HH == -92 ~ -9,
      W4Hous12HH == -91 ~ -1,
      W4Hous12HH == -1 ~ -8,
      TRUE ~ W4Hous12HH
    ),
    hownteen17 = case_when(
      W4Hous12HH == 1 ~ "Owned outright",
      W4Hous12HH == 2 ~ "Being bought on a mortgage/bank loan",
      W4Hous12HH == 3 ~ "Shared ownership (owns & rents property)",
      W4Hous12HH == 4 ~ "Rented from a Council or New Town",
      W4Hous12HH == 5 ~ "Rented from a Housing Association",
      W4Hous12HH == 6 ~ "Rented privately",
      W4Hous12HH == 7 ~ "Rent free",
      W4Hous12HH == 8 ~ "Some other arrangement",
      W4Hous12HH == -1 ~ "Item not applicable",
      W4Hous12HH == -2 ~ "Script error/information lost",
      W4Hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W4Hous12HH == -8 ~ "Don't know/insufficient information",
      W4Hous12HH == -9 ~ "Refusal",
      TRUE ~ NA_character_
    ),
    hown17 = case_when(
      W4Hous12HH == 1 ~ "Owned outright",
      W4Hous12HH == 2 ~ "Owned, buying with help of mortgage/loan",
      W4Hous12HH == 3 ~ "Part rent, part mortgage",
      W4Hous12HH == 4 ~ "Rent it",
      W4Hous12HH == 5 ~ "Rent it",
      W4Hous12HH == 6 ~ "Rent it",
      W4Hous12HH == 7 ~ "live rent-free",
      W4Hous12HH == 8 ~ "Other",
      W4Hous12HH == -1 ~ "Item not applicable",
      W4Hous12HH == -2 ~ "Script error/information lost",
      W4Hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W4Hous12HH == -8 ~ "Don't know/insufficient information",
      W4Hous12HH == -9 ~ "Refusal",
      TRUE ~ NA_character_
    )
  )

# Process wave5 (age 18)
merged_data <- merged_data %>%
  mutate(
    W5Hous12HH = case_when(
      W5Hous12HH == -999 ~ -3,
      W5Hous12HH == -92 ~ -9,
      W5Hous12HH == -91 ~ -1,
      W5Hous12HH == -1 ~ -8,
      TRUE ~ W5Hous12HH
    ),
    W5Hous12BHH = case_when(
      W5Hous12BHH == -999 ~ -3,
      W5Hous12BHH == -92 ~ -9,
      W5Hous12BHH == -91 ~ -1,
      W5Hous12BHH == -1 ~ -8,
      TRUE ~ W5Hous12BHH
    ),
    W5Hous12CHH = case_when(
      W5Hous12CHH == -999 ~ -3,
      W5Hous12CHH == -92 ~ -9,
      W5Hous12CHH == -91 ~ -1,
      W5Hous12CHH == -1 ~ -8,
      TRUE ~ W5Hous12CHH
    ),
    hownteen18 = case_when(
      W5Hous12HH == 1 & W5Hous12BHH == 1 ~ "Owned outright",
      W5Hous12HH == 1 & W5Hous12BHH == 2 ~ "Being bought on a mortgage/bank loan",
      W5Hous12HH == 1 & W5Hous12BHH == 3 ~ "Shared ownership (owns & rents property)",
      W5Hous12HH == 1 & W5Hous12BHH == 4 ~ "Some other arrangement",
      W5Hous12HH == 2 & W5Hous12CHH == 1 ~ "Rented from a Council or New Town",
      W5Hous12HH == 2 & W5Hous12CHH == 2 ~ "Rented from a Housing Association",
      W5Hous12HH == 2 & W5Hous12CHH == 3 ~ "Rented privately",
      W5Hous12HH == 2 & W5Hous12CHH == 4 ~ "Rent free",
      W5Hous12HH == 2 & W5Hous12CHH == 5 ~ "Some other arrangement",
      W5Hous12HH == 3 ~ "Something else",
      W5Hous12HH == -1 ~ "Item not applicable",
      W5Hous12HH == -2 ~ "Script error/information lost",
      W5Hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W5Hous12HH == -8 ~ "Don't know/insufficient information",
      W5Hous12HH == -9 ~ "Refusal",
      TRUE ~ NA_character_
    ),
    hown18 = case_when(
      W5Hous12HH == 1 & W5Hous12BHH == 1 ~ "Owned outright",
      W5Hous12HH == 1 & W5Hous12BHH == 2 ~ "Owned, buying with help of mortgage/loan",
      W5Hous12HH == 1 & W5Hous12BHH == 3 ~ "Part rent, part mortgage",
      W5Hous12HH == 1 & W5Hous12BHH == 4 ~ "Other",
      W5Hous12HH == 2 & W5Hous12CHH == 1 ~ "Rent it",
      W5Hous12HH == 2 & W5Hous12CHH == 2 ~ "Rent it",
      W5Hous12HH == 2 & W5Hous12CHH == 3 ~ "Rent it",
      W5Hous12HH == 2 & W5Hous12CHH == 4 ~ "live rent-free",
      W5Hous12HH == 2 & W5Hous12CHH == 5 ~ "Other",
      W5Hous12HH == 3 ~ "Other",
      W5Hous12HH == -1 ~ "Item not applicable",
      W5Hous12HH == -2 ~ "Script error/information lost",
      W5Hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W5Hous12HH == -8 ~ "Don't know/insufficient information",
      W5Hous12HH == -9 ~ "Refusal",
      TRUE ~ NA_character_
    )
  )

# Process wave6 (age 19)
merged_data <- merged_data %>%
  mutate(
    W6Hous12YP = case_when(
      W6Hous12YP == -92 ~ -9,
      W6Hous12YP == -91 ~ -1,
      W6Hous12YP == -1 ~ -8,
      TRUE ~ W6Hous12YP
    ),
    W6Hous12bYP = case_when(
      W6Hous12bYP == -92 ~ -9,
      W6Hous12bYP == -91 ~ -1,
      W6Hous12bYP == -1 ~ -8,
      TRUE ~ W6Hous12bYP
    ),
    W6Hous12cYP = case_when(
      W6Hous12cYP == -92 ~ -9,
      W6Hous12cYP == -91 ~ -1,
      W6Hous12cYP == -1 ~ -8,
      TRUE ~ W6Hous12cYP
    ),
    hownteen19 = case_when(
      W6Hous12YP == 1 & W6Hous12bYP == 1 ~ "Owned outright",
      W6Hous12YP == 1 & W6Hous12bYP == 2 ~ "Being bought on a mortgage/bank loan",
      W6Hous12YP == 1 & W6Hous12bYP == 3 ~ "Shared ownership",
      W6Hous12YP == 1 & W6Hous12bYP == 4 ~ "Some other arrangement",
      W6Hous12YP == 2 & W6Hous12cYP == 1 ~ "Rented from a Council or New town",
      W6Hous12YP == 2 & W6Hous12cYP == 2 ~ "Rented from a Housing Association",
      W6Hous12YP == 2 & W6Hous12cYP == 3 ~ "Rented privately",
      W6Hous12YP == 2 & W6Hous12cYP == 4 ~ "Rent free",
      W6Hous12YP == 2 & W6Hous12cYP == 5 ~ "Some other arrangement",
      W6Hous12YP == 3 ~ "Something else",
      W6Hous12YP == -1 ~ "Item not applicable",
      W6Hous12YP == -2 ~ "Script error/information lost",
      W6Hous12YP == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W6Hous12YP == -8 ~ "Don't know/insufficient information",
      W6Hous12YP == -9 ~ "Refusal",
      TRUE ~ NA_character_
    ),
    hown19 = case_when(
      W6Hous12YP == 1 & W6Hous12bYP == 1 ~ "Owned outright",
      W6Hous12YP == 1 & W6Hous12bYP == 2 ~ "Owned, buying with help of mortgage/loan",
      W6Hous12YP == 1 & W6Hous12bYP == 3 ~ "Part rent, part mortgage",
      W6Hous12YP == 1 & W6Hous12bYP == 4 ~ "Other",
      W6Hous12YP == 2 & W6Hous12cYP == 1 ~ "Rent it",
      W6Hous12YP == 2 & W6Hous12cYP == 2 ~ "Rent it",
      W6Hous12YP == 2 & W6Hous12cYP == 3 ~ "Rent it",
      W6Hous12YP == 2 & W6Hous12cYP == 4 ~ "live rent-free",
      W6Hous12YP == 2 & W6Hous12cYP == 5 ~ "Other",
      W6Hous12YP == 3 ~ "Other",
      W6Hous12YP == -1 ~ "Item not applicable",
      W6Hous12YP == -2 ~ "Script error/information lost",
      W6Hous12YP == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W6Hous12YP == -8 ~ "Don't know/insufficient information",
      W6Hous12YP == -9 ~ "Refusal",
      TRUE ~ NA_character_
    )
  )

# Process wave7 (age 20)
merged_data <- merged_data %>%
  mutate(
    W7Hous12YP = case_when(
      W7Hous12YP == -92 ~ -9,
      W7Hous12YP == -91 ~ -1,
      W7Hous12YP == -1 ~ -8,
      TRUE ~ W7Hous12YP
    ),
    W7Hous12bYP = case_when(
      W7Hous12bYP == -92 ~ -9,
      W7Hous12bYP == -91 ~ -1,
      W7Hous12bYP == -1 ~ -8,
      TRUE ~ W7Hous12bYP
    ),
    W7Hous12cYP = case_when(
      W7Hous12cYP == -92 ~ -9,
      W7Hous12cYP == -91 ~ -1,
      W7Hous12cYP == -1 ~ -8,
      TRUE ~ W7Hous12cYP
    ),
    hownteen20 = case_when(
      W7Hous12YP == 1 & W7Hous12bYP == 1 ~ "Owned outright",
      W7Hous12YP == 1 & W7Hous12bYP == 2 ~ "Being bought on a mortgage/bank loan",
      W7Hous12YP == 1 & W7Hous12bYP == 3 ~ "Shared ownership (you own and rent the property)",
      W7Hous12YP == 1 & W7Hous12bYP == 4 ~ "Some other arrangement",
      W7Hous12YP == 2 & W7Hous12cYP == 1 ~ "Rented from a Council or New town",
      W7Hous12YP == 2 & W7Hous12cYP == 2 ~ "Rented from a Housing Association",
      W7Hous12YP == 2 & W7Hous12cYP == 3 ~ "Rented privately",
      W7Hous12YP == 2 & W7Hous12cYP == 4 ~ "Rent free",
      W7Hous12YP == 2 & W7Hous12cYP == 5 ~ "Some other arrangement",
      W7Hous12YP == 3 ~ "Something else",
      W7Hous12YP == -1 ~ "Item not applicable",
      W7Hous12YP == -2 ~ "Script error/information lost",
      W7Hous12YP == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W7Hous12YP == -8 ~ "Don't know/insufficient information",
      W7Hous12YP == -9 ~ "Refusal",
      TRUE ~ NA_character_
    ),
    hown20 = case_when(
      W7Hous12YP == 1 & W7Hous12bYP == 1 ~ "Owned outright",
      W7Hous12YP == 1 & W7Hous12bYP == 2 ~ "Owned, buying with help of mortgage/loan",
      W7Hous12YP == 1 & W7Hous12bYP == 3 ~ "Part rent, part mortgage",
      W7Hous12YP == 1 & W7Hous12bYP == 4 ~ "Other",
      W7Hous12YP == 2 & W7Hous12cYP == 1 ~ "Rent it",
      W7Hous12YP == 2 & W7Hous12cYP == 2 ~ "Rent it",
      W7Hous12YP == 2 & W7Hous12cYP == 3 ~ "Rent it",
      W7Hous12YP == 2 & W7Hous12cYP == 4 ~ "live rent-free",
      W7Hous12YP == 2 & W7Hous12cYP == 5 ~ "Other",
      W7Hous12YP == 3 ~ "Other",
      W7Hous12YP == -1 ~ "Item not applicable",
      W7Hous12YP == -2 ~ "Script error/information lost",
      W7Hous12YP == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W7Hous12YP == -8 ~ "Don't know/insufficient information",
      W7Hous12YP == -9 ~ "Refusal",
      TRUE ~ NA_character_
    )
  )

# Process wave8 (age 25)
merged_data <- merged_data %>%
  mutate(
    W8TENURE = case_when(
      W8TENURE == -9 ~ -9,
      W8TENURE == -8 ~ -8,
      W8TENURE == -1 ~ -1,
      TRUE ~ W8TENURE
    ),
    hownteen25 = case_when(
      W8TENURE == 1 ~ "Owned outright",
      W8TENURE == 2 ~ "Owned, buying with help of mortgage/loan",
      W8TENURE == 3 ~ "Part rent, part mortgage",
      W8TENURE == 4 ~ "Rent it",
      W8TENURE == 5 ~ "live rent-free",
      W8TENURE == 6 ~ "Squatting",
      W8TENURE == 7 ~ "Other arrangement",
      W8TENURE == -1 ~ "Item not applicable",
      W8TENURE == -8 ~ "Don't know/insufficient information",
      W8TENURE == -9 ~ "Refusal",
      TRUE ~ NA_character_
    ),
    hown25 = case_when(
      W8TENURE == 1 ~ "Owned outright",
      W8TENURE == 2 ~ "Owned, buying with help of mortgage/loan",
      W8TENURE == 3 ~ "Part rent, part mortgage",
      W8TENURE == 4 ~ "Rent it",
      W8TENURE == 5 ~ "live rent-free",
      W8TENURE == 6 ~ "Other",
      W8TENURE == 7 ~ "Other",
      W8TENURE == -1 ~ "Item not applicable",
      W8TENURE == -8 ~ "Don't know/insufficient information",
      W8TENURE == -9 ~ "Refusal",
      TRUE ~ NA_character_
    )
  )

# Process wave9 (age 32)
merged_data <- merged_data %>%
  mutate(
    W9DTENURE = case_when(
      W9DTENURE == -8 ~ -8,
      TRUE ~ W9DTENURE
    ),
    hownteen32 = case_when(
      W9DTENURE == 1 ~ "Owned outright",
      W9DTENURE == 2 ~ "Owned, buying with help of mortgage/loan",
      W9DTENURE == 3 ~ "Part rent, part mortgage",
      W9DTENURE == 4 ~ "Rent it",
      W9DTENURE == 5 ~ "live rent-free",
      W9DTENURE == 6 ~ "Squatting",
      W9DTENURE == 7 ~ "Other arrangement",
      W9DTENURE == -8 ~ "Don't know/insufficient information",
      TRUE ~ NA_character_
    ),
    hown32 = case_when(
      W9DTENURE == 1 ~ "Owned outright",
      W9DTENURE == 2 ~ "Owned, buying with help of mortgage/loan",
      W9DTENURE == 3 ~ "Part rent, part mortgage",
      W9DTENURE == 4 ~ "Rent it",
      W9DTENURE == 5 ~ "live rent-free",
      W9DTENURE == 6 ~ "Other",
      W9DTENURE == 7 ~ "Other",
      W9DTENURE == -8 ~ "Don't know/insufficient information",
      TRUE ~ NA_character_
    )
  )

# Select only required columns
cleaned_data <- merged_data %>%
  select(NSID, 
         starts_with("hownteen"), 
         starts_with("hown"))

# Write to CSV
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)
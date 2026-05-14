library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load files from data/input/
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
merged_data <- list(wave1, wave2, wave3, wave4, wave5, wave6, wave7, wave8, wave9) %>%
  reduce(full_join, by = "NSID")

# Process age 14 (wave1)
merged_data <- merged_data %>%
  mutate(
    W1hous12HH = case_when(
      is.na(W1hous12HH) ~ -3,
      W1hous12HH == -999 ~ -2,
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
      W1hous12HH == -9 ~ "Refusal",
      W1hous12HH == -8 ~ "Don't know/insufficient information",
      W1hous12HH == -2 ~ "Script error/information lost",
      W1hous12HH == -1 ~ "Item not applicable",
      W1hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed"
    ),
    hown14 = case_when(
      W1hous12HH == 1 ~ "Owned outright",
      W1hous12HH == 2 ~ "Owned, buying with help of mortgage/loan",
      W1hous12HH == 3 ~ "Part rent, part mortgage",
      W1hous12HH %in% c(4,5,6) ~ "Rent it",
      W1hous12HH == 7 ~ "live rent-free",
      W1hous12HH == 8 ~ "Other",
      W1hous12HH == -9 ~ "Refusal",
      W1hous12HH == -8 ~ "Don't know/insufficient information",
      W1hous12HH == -2 ~ "Script error/information lost",
      W1hous12HH == -1 ~ "Item not applicable",
      W1hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed"
    )
  )

# Process age 15 (wave2)
merged_data <- merged_data %>%
  mutate(
    W2Hous12HH = case_when(
      is.na(W2Hous12HH) ~ -3,
      W2Hous12HH == -999 ~ -2,
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
      W2Hous12HH == -9 ~ "Refusal",
      W2Hous12HH == -8 ~ "Don't know/insufficient information",
      W2Hous12HH == -2 ~ "Script error/information lost",
      W2Hous12HH == -1 ~ "Item not applicable",
      W2Hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed"
    ),
    hown15 = case_when(
      W2Hous12HH == 1 ~ "Owned outright",
      W2Hous12HH == 2 ~ "Owned, buying with help of mortgage/loan",
      W2Hous12HH == 3 ~ "Part rent, part mortgage",
      W2Hous12HH %in% c(4,5,6) ~ "Rent it",
      W2Hous12HH == 7 ~ "live rent-free",
      W2Hous12HH == 8 ~ "Other",
      W2Hous12HH == -9 ~ "Refusal",
      W2Hous12HH == -8 ~ "Don't know/insufficient information",
      W2Hous12HH == -2 ~ "Script error/information lost",
      W2Hous12HH == -1 ~ "Item not applicable",
      W2Hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed"
    )
  )

# Age 16 (wave3)
merged_data <- merged_data %>%
  mutate(
    W3hous12HH = case_when(
      is.na(W3hous12HH) ~ -3,
      W3hous12HH == -999 ~ -2,
      W3hous12HH == -99 ~ -2,
      W3hous12HH == -92 ~ -9,
      W3hous12HH == -91 ~ -1,
      W3hous12HH == -1 ~ -8,
      TRUE ~ W3hous12HH
    ),
    hownteen16 = case_when(
      W3hous12HH == 1 ~ "Owned outright",
      W3hous12HH == 2 ~ "Being bought on a mortgage/ bank loan",
      W3hous12HH == 3 ~ "Shared ownership (owns & rents property)",
      W3hous12HH == 4 ~ "Rented from a Council or New Town",
      W3hous12HH == 5 ~ "Rented from a Housing Association",
      W3hous12HH == 6 ~ "Rented privately",
      W3hous12HH == 7 ~ "Rent free",
      W3hous12HH == 8 ~ "Some other arrangement",
      W3hous12HH == -9 ~ "Refusal",
      W3hous12HH == -8 ~ "Don't know/insufficient information",
      W3hous12HH == -2 ~ "Script error/information lost",
      W3hous12HH == -1 ~ "Item not applicable",
      W3hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed"
    ),
    hown16 = case_when(
      W3hous12HH == 1 ~ "Owned outright",
      W3hous12HH == 2 ~ "Owned, buying with help of mortgage/loan",
      W3hous12HH == 3 ~ "Part rent, part mortgage",
      W3hous12HH %in% c(4,5,6) ~ "Rent it",
      W3hous12HH == 7 ~ "live rent-free",
      W3hous12HH == 8 ~ "Other",
      W3hous12HH == -9 ~ "Refusal",
      W3hous12HH == -8 ~ "Don't know/insufficient information",
      W3hous12HH == -2 ~ "Script error/information lost",
      W3hous12HH == -1 ~ "Item not applicable",
      W3hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed"
    )
  )

# Age 17 (wave4)
merged_data <- merged_data %>%
  mutate(
    W4Hous12HH = case_when(
      is.na(W4Hous12HH) ~ -3,
      W4Hous12HH == -999 ~ -2,
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
      W4Hous12HH == -9 ~ "Refusal",
      W4Hous12HH == -8 ~ "Don't know/insufficient information",
      W4Hous12HH == -2 ~ "Script error/information lost",
      W4Hous12HH == -1 ~ "Item not applicable",
      W4Hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed"
    ),
    hown17 = case_when(
      W4Hous12HH == 1 ~ "Owned outright",
      W4Hous12HH == 2 ~ "Owned, buying with help of mortgage/loan",
      W4Hous12HH == 3 ~ "Part rent, part mortgage",
      W4Hous12HH %in% c(4,5,6) ~ "Rent it",
      W4Hous12HH == 7 ~ "live rent-free",
      W4Hous12HH == 8 ~ "Other",
      W4Hous12HH == -9 ~ "Refusal",
      W4Hous12HH == -8 ~ "Don't know/insufficient information",
      W4Hous12HH == -2 ~ "Script error/information lost",
      W4Hous12HH == -1 ~ "Item not applicable",
      W4Hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed"
    )
  )

# Age 18 (wave5)
merged_data <- merged_data %>%
  mutate(
    W5Hous12HH = case_when(
      is.na(W5Hous12HH) ~ -3,
      W5Hous12HH == -999 ~ -2,
      W5Hous12HH == -92 ~ -9,
      W5Hous12HH == -91 ~ -1,
      W5Hous12HH == -1 ~ -8,
      TRUE ~ W5Hous12HH
    ),
    W5Hous12BHH = case_when(
      is.na(W5Hous12BHH) ~ -3,
      W5Hous12BHH == -999 ~ -2,
      W5Hous12BHH == -92 ~ -9,
      W5Hous12BHH == -91 ~ -1,
      W5Hous12BHH == -1 ~ -8,
      TRUE ~ W5Hous12BHH
    ),
    W5Hous12CHH = case_when(
      is.na(W5Hous12CHH) ~ -3,
      W5Hous12CHH == -999 ~ -2,
      W5Hous12CHH == -92 ~ -9,
      W5Hous12CHH == -91 ~ -1,
      W5Hous12CHH == -1 ~ -8,
      TRUE ~ W5Hous12CHH
    ),
    hownteen18 = case_when(
      W5Hous12HH == -9 ~ "Refusal",
      W5Hous12HH == -8 ~ "Don't know/insufficient information",
      W5Hous12HH == -2 ~ "Script error/information lost",
      W5Hous12HH == -1 ~ "Item not applicable",
      W5Hous12HH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W5Hous12HH == 1 & W5Hous12BHH == 1 ~ "Owned outright",
      W5Hous12HH == 1 & W5Hous12BHH == 2 ~ "Being bought on a mortgage/bank loan",
      W5Hous12HH == 1 & W5Hous12BHH == 3 ~ "Shared ownership (owns & rents property)",
      W5Hous12HH == 1 & W5Hous12BHH == 4 ~ "Some other arrangement",
      W5Hous12HH == 1 & W5Hous12BHH %in% c(-9, -8, -2, -1, -3) ~ case_when(
        W5Hous12BHH == -9 ~ "Refusal",
        W5Hous12BHH == -8 ~ "Don't know/insufficient information",
        W5Hous12BHH == -2 ~ "Script error/information lost",
        W5Hous12BHH == -1 ~ "Item not applicable",
        W5Hous12BHH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed"
      ),
      W5Hous12HH == 2 & W5Hous12CHH == 1 ~ "Rented from a Council or New Town",
      W5Hous12HH == 2 & W5Hous12CHH == 2 ~ "Rented from a Housing Association",
      W5Hous12HH == 2 & W5Hous12CHH == 3 ~ "Rented privately",
      W5Hous12HH == 2 & W5Hous12CHH == 4 ~ "Rent free",
      W5Hous12HH == 2 & W5Hous12CHH == 5 ~ "Some other arrangement",
      W5Hous12HH == 2 & W5Hous12CHH %in% c(-9, -8, -2, -1, -3) ~ case_when(
        W5Hous12CHH == -9 ~ "Refusal",
        W5Hous12CHH == -8 ~ "Don't know/insufficient information",
        W5Hous12CHH == -2 ~ "Script error/information lost",
        W5Hous12CHH == -1 ~ "Item not applicable",
        W5Hous12CHH == -3 ~ "Not asked at the fieldwork stage/participated/interviewed"
      ),
      W5Hous12HH == 3 ~ "Some other arrangement",
      TRUE ~ "NA"
    ),
    hown18 = case_when(
      hownteen18 == "Owned outright" ~ "Owned outright",
      hownteen18 == "Being bought on a mortgage/bank loan" ~ "Owned, buying with help of mortgage/loan",
      hownteen18 == "Shared ownership (owns & rents property)" ~ "Part rent, part mortgage",
      hownteen18 %in% c("Rented from a Council or New Town", "Rented from a Housing Association", "Rented privately") ~ "Rent it",
      hownteen18 == "Rent free" ~ "live rent-free",
      hownteen18 %in% c("Some other arrangement", "NA") ~ "Other",
      hownteen18 == "Refusal" ~ "Refusal",
      hownteen18 == "Don't know/insufficient information" ~ "Don't know/insufficient information",
      hownteen18 == "Script error/information lost" ~ "Script error/information lost",
      hownteen18 == "Item not applicable" ~ "Item not applicable",
      hownteen18 == "Not asked at the fieldwork stage/participated/interviewed" ~ "Not asked at the fieldwork stage/participated/interviewed"
    )
  )

# Age 19 (wave6)
merged_data <- merged_data %>%
  mutate(
    W6Hous12YP = case_when(
      is.na(W6Hous12YP) ~ -3,
      W6Hous12YP == -92 ~ -9,
      W6Hous12YP == -91 ~ -1,
      W6Hous12YP == -1 ~ -8,
      TRUE ~ W6Hous12YP
    ),
    W6Hous12bYP = case_when(
      is.na(W6Hous12bYP) ~ -3,
      W6Hous12bYP == -92 ~ -9,
      W6Hous12bYP == -91 ~ -1,
      W6Hous12bYP == -1 ~ -8,
      TRUE ~ W6Hous12bYP
    ),
    W6Hous12cYP = case_when(
      is.na(W6Hous12cYP) ~ -3,
      W6Hous12cYP == -92 ~ -9,
      W6Hous12cYP == -91 ~ -1,
      W6Hous12cYP == -1 ~ -8,
      TRUE ~ W6Hous12cYP
    ),
    hownteen19 = case_when(
      W6Hous12YP == -9 ~ "Refusal",
      W6Hous12YP == -8 ~ "Don't know/insufficient information",
      W6Hous12YP == -2 ~ "Script error/information lost",
      W6Hous12YP == -1 ~ "Item not applicable",
      W6Hous12YP == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W6Hous12YP == 1 & W6Hous12bYP == 1 ~ "Owned outright",
      W6Hous12YP == 1 & W6Hous12bYP == 2 ~ "Being bought on a mortgage/ bank loan",
      W6Hous12YP == 1 & W6Hous12bYP == 3 ~ "Shared ownership",
      W6Hous12YP == 1 & W6Hous12bYP == 4 ~ "Some other arrangement",
      W6Hous12YP == 1 & W6Hous12bYP %in% c(-9, -8, -2, -1, -3) ~ case_when(
        W6Hous12bYP == -9 ~ "Refusal",
        W6Hous12bYP == -8 ~ "Don't know/insufficient information",
        W6Hous12bYP == -2 ~ "Script error/information lost",
        W6Hous12bYP == -1 ~ "Item not applicable",
        W6Hous12bYP == -3 ~ "Not asked at the fieldwork stage/participated/interviewed"
      ),
      W6Hous12YP == 2 & W6Hous12cYP == 1 ~ "Rented from a Council or New town",
      W6Hous12YP == 2 & W6Hous12cYP == 2 ~ "Rented from a Housing Association",
      W6Hous12YP == 2 & W6Hous12cYP == 3 ~ "Rented privately",
      W6Hous12YP == 2 & W6Hous12cYP == 4 ~ "Rent free",
      W6Hous12YP == 2 & W6Hous12cYP == 5 ~ "Some other arrangement",
      W6Hous12YP == 2 & W6Hous12cYP %in% c(-9, -8, -2, -1, -3) ~ case_when(
        W6Hous12cYP == -9 ~ "Refusal",
        W6Hous12cYP == -8 ~ "Don't know/insufficient information",
        W6Hous12cYP == -2 ~ "Script error/information lost",
        W6Hous12cYP == -1 ~ "Item not applicable",
        W6Hous12cYP == -3 ~ "Not asked at the fieldwork stage/participated/interviewed"
      ),
      W6Hous12YP == 3 ~ "Some other arrangement",
      TRUE ~ "NA"
    ),
    hown19 = case_when(
      hownteen19 == "Owned outright" ~ "Owned outright",
      hownteen19 == "Being bought on a mortgage/ bank loan" ~ "Owned, buying with help of mortgage/loan",
      hownteen19 == "Shared ownership" ~ "Part rent, part mortgage",
      hownteen19 %in% c("Rented from a Council or New town", "Rented from a Housing Association", "Rented privately") ~ "Rent it",
      hownteen19 == "Rent free" ~ "live rent-free",
      hownteen19 %in% c("Some other arrangement", "NA") ~ "Other",
      hownteen19 == "Refusal" ~ "Refusal",
      hownteen19 == "Don't know/insufficient information" ~ "Don't know/insufficient information",
      hownteen19 == "Script error/information lost" ~ "Script error/information lost",
      hownteen19 == "Item not applicable" ~ "Item not applicable",
      hownteen19 == "Not asked at the fieldwork stage/participated/interviewed" ~ "Not asked at the fieldwork stage/participated/interviewed"
    )
  )

# Age 20 (wave7)
merged_data <- merged_data %>%
  mutate(
    W7Hous12YP = case_when(
      is.na(W7Hous12YP) ~ -3,
      W7Hous12YP == -92 ~ -9,
      W7Hous12YP == -91 ~ -1,
      W7Hous12YP == -1 ~ -8,
      TRUE ~ W7Hous12YP
    ),
    W7Hous12bYP = case_when(
      is.na(W7Hous12bYP) ~ -3,
      W7Hous12bYP == -92 ~ -9,
      W7Hous12bYP == -91 ~ -1,
      W7Hous12bYP == -1 ~ -8,
      TRUE ~ W7Hous12bYP
    ),
    W7Hous12cYP = case_when(
      is.na(W7Hous12cYP) ~ -3,
      W7Hous12cYP == -92 ~ -9,
      W7Hous12cYP == -91 ~ -1,
      W7Hous12cYP == -1 ~ -8,
      TRUE ~ W7Hous12cYP
    ),
    hownteen20 = case_when(
      W7Hous12YP == -9 ~ "Refusal",
      W7Hous12YP == -8 ~ "Don't know/insufficient information",
      W7Hous12YP == -2 ~ "Script error/information lost",
      W7Hous12YP == -1 ~ "Item not applicable",
      W7Hous12YP == -3 ~ "Not asked at the fieldwork stage/participated/interviewed",
      W7Hous12YP == 1 & W7Hous12bYP == 1 ~ "Owned outright",
      W7Hous12YP == 1 & W7Hous12bYP == 2 ~ "Being bought on a mortgage/ bank loan",
      W7Hous12YP == 1 & W7Hous12bYP == 3 ~ "Shared ownership (you own and rent the property)",
      W7Hous12YP == 1 & W7Hous12bYP == 4 ~ "Some other arrangement",
      W7Hous12YP == 1 & W7Hous12bYP %in% c(-9, -8, -2, -1, -3) ~ case_when(
        W7Hous12bYP == -9 ~ "Refusal",
        W7Hous12bYP == -8 ~ "Don't know/insufficient information",
        W7Hous12bYP == -2 ~ "Script error/information lost",
        W7Hous12bYP == -1 ~ "Item not applicable",
        W7Hous12bYP == -3 ~ "Not asked at the fieldwork stage/participated/interviewed"
      ),
      W7Hous12YP == 2 & W7Hous12cYP == 1 ~ "Rented from a Council or New town",
      W7Hous12YP == 2 & W7Hous12cYP == 2 ~ "Rented from a Housing Association",
      W7Hous12YP == 2 & W7Hous12cYP == 3 ~ "Rented privately",
      W7Hous12YP == 2 & W7Hous12cYP == 4 ~ "Rent free",
      W7Hous12YP == 2 & W7Hous12cYP == 5 ~ "Some other arrangement",
      W7Hous12YP == 2 & W7Hous12cYP %in% c(-9, -8, -2, -1, -3) ~ case_when(
        W7Hous12cYP == -9 ~ "Refusal",
        W7Hous12cYP == -8 ~ "Don't know/insufficient information",
        W7Hous12cYP == -2 ~ "Script error/information lost",
        W7Hous12cYP == -1 ~ "Item not applicable",
        W7Hous12cYP == -3 ~ "Not asked at the fieldwork stage/participated/interviewed"
      ),
      W7Hous12YP == 3 ~ "Some other arrangement",
      TRUE ~ "NA"
    ),
    hown20 = case_when(
      hownteen20 == "Owned outright" ~ "Owned outright",
      hownteen20 == "Being bought on a mortgage/ bank loan" ~ "Owned, buying with help of mortgage/loan",
      hownteen20 == "Shared ownership (you own and rent the property)" ~ "Part rent, part mortgage",
      hownteen20 %in% c("Rented from a Council or New town", "Rented from a Housing Association", "Rented privately") ~ "Rent it",
      hownteen20 == "Rent free" ~ "live rent-free",
      hownteen20 %in% c("Some other arrangement", "NA") ~ "Other",
      hownteen20 == "Refusal" ~ "Refusal",
      hownteen20 == "Don't know/insufficient information" ~ "Don't know/insufficient information",
      hownteen20 == "Script error/information lost" ~ "Script error/information lost",
      hownteen20 == "Item not applicable" ~ "Item not applicable",
      hownteen20 == "Not asked at the fieldwork stage/participated/interviewed" ~ "Not asked at the fieldwork stage/participated/interviewed"
    )
  )

# Age 25 (wave8)
merged_data <- merged_data %>%
  mutate(
    W8TENURE = case_when(
      is.na(W8TENURE) ~ -3,
      W8TENURE == -9 ~ -9,
      W8TENURE == -8 ~ -8,
      W8TENURE == -1 ~ -1,
      TRUE ~ W8TENURE
    ),
    hown25 = case_when(
      W8TENURE == 1 ~ "Owned outright",
      W8TENURE == 2 ~ "Owned, buying with help of mortgage/loan",
      W8TENURE == 3 ~ "Part rent, part mortgage",
      W8TENURE == 4 ~ "Rent it",
      W8TENURE == 5 ~ "live rent-free",
      W8TENURE == 6 ~ "Squatting",
      W8TENURE == 7 ~ "Other",
      W8TENURE == -9 ~ "Refusal",
      W8TENURE == -8 ~ "Don't know/insufficient information",
      W8TENURE == -2 ~ "Script error/information lost",
      W8TENURE == -1 ~ "Item not applicable",
      W8TENURE == -3 ~ "Not asked at the fieldwork stage/participated/interviewed"
    )
  )

# Age 32 (wave9)
merged_data <- merged_data %>%
  mutate(
    W9DTENURE = case_when(
      is.na(W9DTENURE) ~ -3,
      W9DTENURE == -8 ~ -8,
      TRUE ~ W9DTENURE
    ),
    hown32 = case_when(
      W9DTENURE == 1 ~ "Owned outright",
      W9DTENURE == 2 ~ "Owned, buying with help of mortgage/loan",
      W9DTENURE == 3 ~ "Part rent, part mortgage",
      W9DTENURE == 4 ~ "Rent it",
      W9DTENURE == 5 ~ "live rent-free",
      W9DTENURE == 6 ~ "Squatting",
      W9DTENURE == 7 ~ "Other",
      W9DTENURE == -8 ~ "Don't know/insufficient information",
      W9DTENURE == -2 ~ "Script error/information lost",
      W9DTENURE == -1 ~ "Item not applicable",
      W9DTENURE == -3 ~ "Not asked at the fieldwork stage/participated/interviewed"
    )
  )

# Select required columns
final_data <- merged_data %>%
  select(
    NSID,
    starts_with("hown"),
    starts_with("hownteen")
  )

# Write to CSV
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)
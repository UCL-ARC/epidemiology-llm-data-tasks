# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# ============================================================================
# 1. FILE LOADING
# ============================================================================

# Load all data files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# ============================================================================
# 2. MERGE ALL WAVES BY NSID
# ============================================================================

# Start with wave1 and progressively full_join
merged <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# ============================================================================
# 3. STANDARDIZE MISSING VALUES
# ============================================================================

# Standard codes:
# -9 = Refusal
# -8 = Don't know/insufficient information
# -1 = Item not applicable
# -3 = Not asked/participated/interviewed
# -2 = Script error/information lost

# Recode function for waves 1-4 (ages 14-17)
recode_1_4 <- function(x) {
  x %>%
    replace(x == -999, -3) %>%
    replace(x == -998, -2) %>%
    replace(x == -997, -2) %>%
    replace(x == -995, -3) %>%
    replace(x == -99, -3) %>%
    replace(x == -92, -9) %>%
    replace(x == -91, -1) %>%
    replace(x == -1, -8)
}

# Recode function for waves 5-7 (ages 18-20)
recode_5_7 <- function(x) {
  x %>%
    replace(x == -999, -3) %>%
    replace(x == -92, -9) %>%
    replace(x == -91, -1) %>%
    replace(x == -1, -8)
}

# Recode function for wave 8 (age 25)
recode_8 <- function(x) {
  x %>%
    replace(x == -9, -9) %>%
    replace(x == -8, -8) %>%
    replace(x == -1, -1)
}

# Recode function for wave 9 (age 32)
recode_9 <- function(x) {
  x %>%
    replace(x == -8, -8) %>%
    replace(x == -1, -1)
}

# Apply recoding to all variables
merged <- merged %>%
  mutate(
    W1hous12HH = recode_1_4(W1hous12HH),
    W2Hous12HH = recode_1_4(W2Hous12HH),
    W3hous12HH = recode_1_4(W3hous12HH),
    W4Hous12HH = recode_1_4(W4Hous12HH),
    W5Hous12HH = recode_5_7(W5Hous12HH),
    W5Hous12BHH = recode_5_7(W5Hous12BHH),
    W5Hous12CHH = recode_5_7(W5Hous12CHH),
    W6Hous12YP = recode_5_7(W6Hous12YP),
    W6Hous12bYP = recode_5_7(W6Hous12bYP),
    W6Hous12cYP = recode_5_7(W6Hous12cYP),
    W7Hous12YP = recode_5_7(W7Hous12YP),
    W7Hous12bYP = recode_5_7(W7Hous12bYP),
    W7Hous12cYP = recode_5_7(W7Hous12cYP),
    W8TENURE = recode_8(W8TENURE),
    W9DTENURE = recode_9(W9DTENURE)
  )

# ============================================================================
# 4. CREATE DETAILED ADOLESCENT VARIABLES (hownteen14-20)
# ============================================================================

# Ages 14-17: Direct mapping (no split)
merged <- merged %>%
  mutate(
    hownteen14 = W1hous12HH,
    hownteen15 = W2Hous12HH,
    hownteen16 = W3hous12HH,
    hownteen17 = W4Hous12HH
  )

# Age 18: Combine W5Hous12HH (type) with sub-type variables
merged <- merged %>%
  mutate(
    hownteen18 = case_when(
      W5Hous12HH == 1 ~ W5Hous12BHH,
      W5Hous12HH == 2 ~ W5Hous12CHH,
      TRUE ~ NA_integer_
    )
  )

# Age 19: Combine W6Hous12YP (type) with sub-type variables
merged <- merged %>%
  mutate(
    hownteen19 = case_when(
      W6Hous12YP == 1 ~ W6Hous12bYP,
      W6Hous12YP == 2 ~ W6Hous12cYP,
      TRUE ~ NA_integer_
    )
  )

# Age 20: Combine W7Hous12YP (type) with sub-type variables
merged <- merged %>%
  mutate(
    hownteen20 = case_when(
      W7Hous12YP == 1 ~ W7Hous12bYP,
      W7Hous12YP == 2 ~ W7Hous12cYP,
      TRUE ~ NA_integer_
    )
  )

# ============================================================================
# 5. CREATE COLLAPSED VARIABLES (hown14-20, hown25, hown32)
# ============================================================================

# For ages 14-17, collapsed = detailed (since no split)
merged <- merged %>%
  mutate(
    hown14 = W1hous12HH,
    hown15 = W2Hous12HH,
    hown16 = W3hous12HH,
    hown17 = W4Hous12HH,
    hown18 = case_when(
      W5Hous12HH == 1 ~ W5Hous12BHH,
      W5Hous12HH == 2 ~ W5Hous12CHH,
      TRUE ~ NA_integer_
    ),
    hown19 = case_when(
      W6Hous12YP == 1 ~ W6Hous12bYP,
      W6Hous12YP == 2 ~ W6Hous12cYP,
      TRUE ~ NA_integer_
    ),
    hown20 = case_when(
      W7Hous12YP == 1 ~ W7Hous12bYP,
      W7Hous12YP == 2 ~ W7Hous12cYP,
      TRUE ~ NA_integer_
    )
  )

# Age 25
merged <- merged %>%
  mutate(
    hown25 = W8TENURE
  )

# Age 32
merged <- merged %>%
  mutate(
    hown32 = W9DTENURE
  )

# ============================================================================
# 6. MAP COLLAPSED VALUES TO STANDARD 6-CATEGORY SCHEME
# ============================================================================

# Collapsed scheme:
# 1 = Owned outright
# 2 = Owned, buying with help of mortgage/loan
# 3 = Part rent, part mortgage
# 4 = Rent it
# 5 = live rent-free
# 6 = Other
# -1 = Item not applicable
# -2 = Script error/information lost
# -3 = Not asked at the fieldwork stage/participated/interviewed
# -8 = Don't know/insufficient information
# -9 = Refusal

# For ages 14-17 (waves 1-4), map to collapsed scheme
merged <- merged %>%
  mutate(
    hown14_cat = case_when(
      hown14 == 1 ~ 1,
      hown14 == 2 ~ 2,
      hown14 == 3 ~ 3,
      hown14 == 4 ~ 4,
      hown14 == 5 ~ 4,
      hown14 == 6 ~ 4,
      hown14 == 7 ~ 5,
      hown14 == 8 ~ 6,
      hown14 == -1 ~ -1,
      hown14 == -2 ~ -2,
      hown14 == -3 ~ -3,
      hown14 == -8 ~ -8,
      hown14 == -9 ~ -9,
      TRUE ~ NA_integer_
    ),
    hown15_cat = case_when(
      hown15 == 1 ~ 1,
      hown15 == 2 ~ 2,
      hown15 == 3 ~ 3,
      hown15 == 4 ~ 4,
      hown15 == 5 ~ 4,
      hown15 == 6 ~ 4,
      hown15 == 7 ~ 5,
      hown15 == 8 ~ 6,
      hown15 == -1 ~ -1,
      hown15 == -2 ~ -2,
      hown15 == -3 ~ -3,
      hown15 == -8 ~ -8,
      hown15 == -9 ~ -9,
      TRUE ~ NA_integer_
    ),
    hown16_cat = case_when(
      hown16 == 1 ~ 1,
      hown16 == 2 ~ 2,
      hown16 == 3 ~ 3,
      hown16 == 4 ~ 4,
      hown16 == 5 ~ 4,
      hown16 == 6 ~ 4,
      hown16 == 7 ~ 5,
      hown16 == 8 ~ 6,
      hown16 == -1 ~ -1,
      hown16 == -2 ~ -2,
      hown16 == -3 ~ -3,
      hown16 == -8 ~ -8,
      hown16 == -9 ~ -9,
      TRUE ~ NA_integer_
    ),
    hown17_cat = case_when(
      hown17 == 1 ~ 1,
      hown17 == 2 ~ 2,
      hown17 == 3 ~ 3,
      hown17 == 4 ~ 4,
      hown17 == 5 ~ 4,
      hown17 == 6 ~ 4,
      hown17 == 7 ~ 5,
      hown17 == 8 ~ 6,
      hown17 == -1 ~ -1,
      hown17 == -2 ~ -2,
      hown17 == -3 ~ -3,
      hown17 == -8 ~ -8,
      hown17 == -9 ~ -9,
      TRUE ~ NA_integer_
    )
  )

# For ages 18-20 (waves 5-7), map to collapsed scheme
merged <- merged %>%
  mutate(
    hown18_cat = case_when(
      hown18 == 1 ~ 1,
      hown18 == 2 ~ 2,
      hown18 == 3 ~ 3,
      hown18 == 4 ~ 4,
      hown18 == 5 ~ 5,
      hown18 == 6 ~ 6,
      hown18 == -1 ~ -1,
      hown18 == -2 ~ -2,
      hown18 == -3 ~ -3,
      hown18 == -8 ~ -8,
      hown18 == -9 ~ -9,
      TRUE ~ NA_integer_
    ),
    hown19_cat = case_when(
      hown19 == 1 ~ 1,
      hown19 == 2 ~ 2,
      hown19 == 3 ~ 3,
      hown19 == 4 ~ 4,
      hown19 == 5 ~ 5,
      hown19 == 6 ~ 6,
      hown19 == -1 ~ -1,
      hown19 == -2 ~ -2,
      hown19 == -3 ~ -3,
      hown19 == -8 ~ -8,
      hown19 == -9 ~ -9,
      TRUE ~ NA_integer_
    ),
    hown20_cat = case_when(
      hown20 == 1 ~ 1,
      hown20 == 2 ~ 2,
      hown20 == 3 ~ 3,
      hown20 == 4 ~ 4,
      hown20 == 5 ~ 5,
      hown20 == 6 ~ 6,
      hown20 == -1 ~ -1,
      hown20 == -2 ~ -2,
      hown20 == -3 ~ -3,
      hown20 == -8 ~ -8,
      hown20 == -9 ~ -9,
      TRUE ~ NA_integer_
    )
  )

# For age 25
merged <- merged %>%
  mutate(
    hown25_cat = case_when(
      hown25 == 1 ~ 1,
      hown25 == 2 ~ 2,
      hown25 == 3 ~ 3,
      hown25 == 4 ~ 4,
      hown25 == 5 ~ 5,
      hown25 == 6 ~ 6,
      hown25 == 7 ~ 6,
      hown25 == -1 ~ -1,
      hown25 == -2 ~ -2,
      hown25 == -3 ~ -3,
      hown25 == -8 ~ -8,
      hown25 == -9 ~ -9,
      TRUE ~ NA_integer_
    )
  )

# For age 32
merged <- merged %>%
  mutate(
    hown32_cat = case_when(
      hown32 == 1 ~ 1,
      hown32 == 2 ~ 2,
      hown32 == 3 ~ 3,
      hown32 == 4 ~ 4,
      hown32 == 5 ~ 5,
      hown32 == 6 ~ 6,
      hown32 == 7 ~ 6,
      hown32 == -1 ~ -1,
      hown32 == -2 ~ -2,
      hown32 == -3 ~ -3,
      hown32 == -8 ~ -8,
      hown32 == -9 ~ -9,
      TRUE ~ NA_integer_
    )
  )

# ============================================================================
# 7. CREATE FACTOR LABELS
# ============================================================================

# Labels for detailed adolescent variables (hownteen14-20)
merged <- merged %>%
  mutate(
    hownteen14 = factor(hownteen14,
                        levels = c(1, 2, 3, 4, 5, 6, 7, 8, -1, -2, -3, -8, -9),
                        labels = c("Owned outright",
                                  "Being bought on a mortgage/bank loan",
                                  "Shared ownership (owns & rents property)",
                                  "Rented from a Council or New Town",
                                  "Rented from a Housing Association",
                                  "Rented privately",
                                  "Rent free",
                                  "Some other arrangement",
                                  "Item not applicable",
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed",
                                  "Don't know/insufficient information",
                                  "Refusal")),
    hownteen15 = factor(hownteen15,
                        levels = c(1, 2, 3, 4, 5, 6, 7, 8, -1, -2, -3, -8, -9),
                        labels = c("Owned outright",
                                  "Being bought on a mortgage/bank loan",
                                  "Shared ownership (owns & rents property)",
                                  "Rented from a Council or New Town",
                                  "Rented from a Housing Association",
                                  "Rented privately",
                                  "Rent free",
                                  "Some other arrangement",
                                  "Item not applicable",
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed",
                                  "Don't know/insufficient information",
                                  "Refusal")),
    hownteen16 = factor(hownteen16,
                        levels = c(1, 2, 3, 4, 5, 6, 7, 8, -1, -2, -3, -8, -9),
                        labels = c("Owned outright",
                                  "Being bought on a mortgage/bank loan",
                                  "Shared ownership (owns & rents property)",
                                  "Rented from a Council or New Town",
                                  "Rented from a Housing Association",
                                  "Rented privately",
                                  "Rent free",
                                  "Some other arrangement",
                                  "Item not applicable",
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed",
                                  "Don't know/insufficient information",
                                  "Refusal")),
    hownteen17 = factor(hownteen17,
                        levels = c(1, 2, 3, 4, 5, 6, 7, 8, -1, -2, -3, -8, -9),
                        labels = c("Owned outright",
                                  "Being bought on a mortgage/bank loan",
                                  "Shared ownership (owns & rents property)",
                                  "Rented from a Council or New Town",
                                  "Rented from a Housing Association",
                                  "Rented privately",
                                  "Rent free",
                                  "Some other arrangement",
                                  "Item not applicable",
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed",
                                  "Don't know/insufficient information",
                                  "Refusal")),
    hownteen18 = factor(hownteen18,
                        levels = c(1, 2, 3, 4, 5, 6, 7, 8, -1, -2, -3, -8, -9),
                        labels = c("Owned outright",
                                  "Being bought on a mortgage/bank loan",
                                  "Shared ownership (owns & rents property)",
                                  "Rented from a Council or New Town",
                                  "Rented from a Housing Association",
                                  "Rented privately",
                                  "Rent free",
                                  "Some other arrangement",
                                  "Item not applicable",
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed",
                                  "Don't know/insufficient information",
                                  "Refusal")),
    hownteen19 = factor(hownteen19,
                        levels = c(1, 2, 3, 4, 5, 6, 7, 8, -1, -2, -3, -8, -9),
                        labels = c("Owned outright",
                                  "Being bought on a mortgage/bank loan",
                                  "Shared ownership (owns & rents property)",
                                  "Rented from a Council or New Town",
                                  "Rented from a Housing Association",
                                  "Rented privately",
                                  "Rent free",
                                  "Some other arrangement",
                                  "Item not applicable",
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed",
                                  "Don't know/insufficient information",
                                  "Refusal")),
    hownteen20 = factor(hownteen20,
                        levels = c(1, 2, 3, 4, 5, 6, 7, 8, -1, -2, -3, -8, -9),
                        labels = c("Owned outright",
                                  "Being bought on a mortgage/bank loan",
                                  "Shared ownership (owns & rents property)",
                                  "Rented from a Council or New Town",
                                  "Rented from a Housing Association",
                                  "Rented privately",
                                  "Rent free",
                                  "Some other arrangement",
                                  "Item not applicable",
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed",
                                  "Don't know/insufficient information",
                                  "Refusal"))
  )

# Labels for collapsed variables (hown14-20, hown25, hown32)
merged <- merged %>%
  mutate(
    hown14_cat = factor(hown14_cat,
                        levels = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -8, -9),
                        labels = c("Owned outright",
                                  "Owned, buying with help of mortgage/loan",
                                  "Part rent, part mortgage",
                                  "Rent it",
                                  "live rent-free",
                                  "Other",
                                  "Item not applicable",
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed",
                                  "Don't know/insufficient information",
                                  "Refusal")),
    hown15_cat = factor(hown15_cat,
                        levels = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -8, -9),
                        labels = c("Owned outright",
                                  "Owned, buying with help of mortgage/loan",
                                  "Part rent, part mortgage",
                                  "Rent it",
                                  "live rent-free",
                                  "Other",
                                  "Item not applicable",
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed",
                                  "Don't know/insufficient information",
                                  "Refusal")),
    hown16_cat = factor(hown16_cat,
                        levels = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -8, -9),
                        labels = c("Owned outright",
                                  "Owned, buying with help of mortgage/loan",
                                  "Part rent, part mortgage",
                                  "Rent it",
                                  "live rent-free",
                                  "Other",
                                  "Item not applicable",
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed",
                                  "Don't know/insufficient information",
                                  "Refusal")),
    hown17_cat = factor(hown17_cat,
                        levels = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -8, -9),
                        labels = c("Owned outright",
                                  "Owned, buying with help of mortgage/loan",
                                  "Part rent, part mortgage",
                                  "Rent it",
                                  "live rent-free",
                                  "Other",
                                  "Item not applicable",
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed",
                                  "Don't know/insufficient information",
                                  "Refusal")),
    hown18_cat = factor(hown18_cat,
                        levels = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -8, -9),
                        labels = c("Owned outright",
                                  "Owned, buying with help of mortgage/loan",
                                  "Part rent, part mortgage",
                                  "Rent it",
                                  "live rent-free",
                                  "Other",
                                  "Item not applicable",
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed",
                                  "Don't know/insufficient information",
                                  "Refusal")),
    hown19_cat = factor(hown19_cat,
                        levels = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -8, -9),
                        labels = c("Owned outright",
                                  "Owned, buying with help of mortgage/loan",
                                  "Part rent, part mortgage",
                                  "Rent it",
                                  "live rent-free",
                                  "Other",
                                  "Item not applicable",
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed",
                                  "Don't know/insufficient information",
                                  "Refusal")),
    hown20_cat = factor(hown20_cat,
                        levels = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -8, -9),
                        labels = c("Owned outright",
                                  "Owned, buying with help of mortgage/loan",
                                  "Part rent, part mortgage",
                                  "Rent it",
                                  "live rent-free",
                                  "Other",
                                  "Item not applicable",
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed",
                                  "Don't know/insufficient information",
                                  "Refusal")),
    hown25_cat = factor(hown25_cat,
                        levels = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -8, -9),
                        labels = c("Owned outright",
                                  "Owned, buying with help of mortgage/loan",
                                  "Part rent, part mortgage",
                                  "Rent it",
                                  "live rent-free",
                                  "Other",
                                  "Item not applicable",
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed",
                                  "Don't know/insufficient information",
                                  "Refusal")),
    hown32_cat = factor(hown32_cat,
                        levels = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -8, -9),
                        labels = c("Owned outright",
                                  "Owned, buying with help of mortgage/loan",
                                  "Part rent, part mortgage",
                                  "Rent it",
                                  "live rent-free",
                                  "Other",
                                  "Item not applicable",
                                  "Script error/information lost",
                                  "Not asked at the fieldwork stage/participated/interviewed",
                                  "Don't know/insufficient information",
                                  "Refusal"))
  )

# ============================================================================
# 8. SELECT FINAL VARIABLES
# ============================================================================

# Keep only: NSID, collapsed variables (hown14-20, hown25, hown32), and detailed adolescent variables (hownteen14-20)
cleaned_data <- merged %>%
  select(NSID, 
         hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32,
         hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20)

# ============================================================================
# 9. OUTPUT
# ============================================================================

write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)

# Print summary
cat("Dataset cleaned successfully.\n")
cat("Number of observations:", nrow(cleaned_data), "\n")
cat("Number of variables:", ncol(cleaned_data), "\n")
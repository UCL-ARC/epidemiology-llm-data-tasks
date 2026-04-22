library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Recode detailed adolescent variables (ages 14-17: waves 1-4 have single variables)
# Mapping: 1->1, 2->2, 3->3, 4->4, 5->5, 6->6, 7->7, 8->8
# Missing: -999/-997/-995/-998/-99 -> -2 (script error/lost), -92 -> -9 (refusal), -91 -> -1 (not applicable), -1 -> -8 (don't know)

merged_data <- merged_data %>%
  mutate(
    hownteen14 = case_when(
      W1hous12HH %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ as.numeric(W1hous12HH),
      W1hous12HH %in% c(-999) ~ -2,
      W1hous12HH %in% c(-92) ~ -9,
      W1hous12HH %in% c(-91) ~ -1,
      W1hous12HH %in% c(-1) ~ -8,
      TRUE ~ -2
    ),
    hownteen15 = case_when(
      W2Hous12HH %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ as.numeric(W2Hous12HH),
      W2Hous12HH %in% c(-999, -997, -995, -998, -99) ~ -2,
      W2Hous12HH %in% c(-92) ~ -9,
      W2Hous12HH %in% c(-91) ~ -1,
      W2Hous12HH %in% c(-1) ~ -8,
      TRUE ~ -2
    ),
    hownteen16 = case_when(
      W3hous12HH %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ as.numeric(W3hous12HH),
      W3hous12HH %in% c(-999, -99) ~ -2,
      W3hous12HH %in% c(-92) ~ -9,
      W3hous12HH %in% c(-91) ~ -1,
      W3hous12HH %in% c(-1) ~ -8,
      TRUE ~ -2
    ),
    hownteen17 = case_when(
      W4Hous12HH %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ as.numeric(W4Hous12HH),
      W4Hous12HH %in% c(-999, -997) ~ -2,
      W4Hous12HH %in% c(-92) ~ -9,
      W4Hous12HH %in% c(-91) ~ -1,
      W4Hous12HH %in% c(-1) ~ -8,
      TRUE ~ -2
    )
  )

# Recode detailed adolescent variables for ages 18-20 (waves 5-7 have split variables)
# For owned (type=1): 1->1, 2->2, 3->3, 4->8
# For rented (type=2): 1->4, 2->5, 3->6, 4->7, 5->8
# Missing codes mapped based on labels

merged_data <- merged_data %>%
  mutate(
    hownteen18 = case_when(
      W5Hous12HH == 1 & W5Hous12BHH %in% c(1, 2, 3, 4) ~ case_when(
        W5Hous12BHH == 1 ~ 1,
        W5Hous12BHH == 2 ~ 2,
        W5Hous12BHH == 3 ~ 3,
        W5Hous12BHH == 4 ~ 8
      ),
      W5Hous12HH == 2 & W5Hous12CHH %in% c(1, 2, 3, 4, 5) ~ case_when(
        W5Hous12CHH == 1 ~ 4,
        W5Hous12CHH == 2 ~ 5,
        W5Hous12CHH == 3 ~ 6,
        W5Hous12CHH == 4 ~ 7,
        W5Hous12CHH == 5 ~ 8
      ),
      W5Hous12HH %in% c(-999) ~ -2,
      W5Hous12HH %in% c(-92) ~ -9,
      W5Hous12HH %in% c(-91) ~ -1,
      W5Hous12HH %in% c(-1) ~ -8,
      TRUE ~ -2
    ),
    hownteen19 = case_when(
      W6Hous12YP == 1 & W6Hous12bYP %in% c(1, 2, 3, 4) ~ case_when(
        W6Hous12bYP == 1 ~ 1,
        W6Hous12bYP == 2 ~ 2,
        W6Hous12bYP == 3 ~ 3,
        W6Hous12bYP == 4 ~ 8
      ),
      W6Hous12YP == 2 & W6Hous12cYP %in% c(1, 2, 3, 4, 5) ~ case_when(
        W6Hous12cYP == 1 ~ 4,
        W6Hous12cYP == 2 ~ 5,
        W6Hous12cYP == 3 ~ 6,
        W6Hous12cYP == 4 ~ 7,
        W6Hous12cYP == 5 ~ 8
      ),
      W6Hous12YP %in% c(-92) ~ -9,
      W6Hous12YP %in% c(-91) ~ -1,
      W6Hous12YP %in% c(-1) ~ -8,
      TRUE ~ -2
    ),
    hownteen20 = case_when(
      W7Hous12YP == 1 & W7Hous12bYP %in% c(1, 2, 3, 4) ~ case_when(
        W7Hous12bYP == 1 ~ 1,
        W7Hous12bYP == 2 ~ 2,
        W7Hous12bYP == 3 ~ 3,
        W7Hous12bYP == 4 ~ 8
      ),
      W7Hous12YP == 2 & W7Hous12cYP %in% c(1, 2, 3, 4, 5) ~ case_when(
        W7Hous12cYP == 1 ~ 4,
        W7Hous12cYP == 2 ~ 5,
        W7Hous12cYP == 3 ~ 6,
        W7Hous12cYP == 4 ~ 7,
        W7Hous12cYP == 5 ~ 8
      ),
      W7Hous12YP %in% c(-92) ~ -9,
      W7Hous12YP %in% c(-91) ~ -1,
      W7Hous12YP %in% c(-1) ~ -8,
      TRUE ~ -2
    )
  )

# Recode adult variables (ages 25 and 32) - use temp names to avoid overwriting
merged_data <- merged_data %>%
  mutate(
    hown25_temp = case_when(
      W8TENURE %in% c(1, 2, 3, 4, 5, 6, 7) ~ as.numeric(W8TENURE),
      W8TENURE %in% c(-9) ~ -9,
      W8TENURE %in% c(-8) ~ -8,
      W8TENURE %in% c(-1) ~ -1,
      TRUE ~ -2
    ),
    hown32_temp = case_when(
      W9DTENURE %in% c(1, 2, 3, 4, 5, 6, 7) ~ as.numeric(W9DTENURE),
      W9DTENURE %in% c(-8) ~ -8,
      TRUE ~ -1
    )
  )

# Create collapsed/harmonized variables for adolescent ages 14-20
# Mapping: 1-3->1 (owned), 4-6->2 (rent), 7->3 (rent-free), 8->4 (other)
# Missing: -1->5 (not applicable), -2->6 (script error), -3->7 (not asked), -8->8 (don't know), -9->9 (refusal)

merged_data <- merged_data %>%
  mutate(
    hown14 = case_when(
      hownteen14 %in% c(1, 2, 3) ~ 1,
      hownteen14 %in% c(4, 5, 6) ~ 2,
      hownteen14 %in% c(7) ~ 3,
      hownteen14 %in% c(8) ~ 4,
      hownteen14 %in% c(-1) ~ 5,
      hownteen14 %in% c(-2) ~ 6,
      hownteen14 %in% c(-3) ~ 7,
      hownteen14 %in% c(-8) ~ 8,
      hownteen14 %in% c(-9) ~ 9,
      TRUE ~ 7
    ),
    hown15 = case_when(
      hownteen15 %in% c(1, 2, 3) ~ 1,
      hownteen15 %in% c(4, 5, 6) ~ 2,
      hownteen15 %in% c(7) ~ 3,
      hownteen15 %in% c(8) ~ 4,
      hownteen15 %in% c(-1) ~ 5,
      hownteen15 %in% c(-2) ~ 6,
      hownteen15 %in% c(-3) ~ 7,
      hownteen15 %in% c(-8) ~ 8,
      hownteen15 %in% c(-9) ~ 9,
      TRUE ~ 7
    ),
    hown16 = case_when(
      hownteen16 %in% c(1, 2, 3) ~ 1,
      hownteen16 %in% c(4, 5, 6) ~ 2,
      hownteen16 %in% c(7) ~ 3,
      hownteen16 %in% c(8) ~ 4,
      hownteen16 %in% c(-1) ~ 5,
      hownteen16 %in% c(-2) ~ 6,
      hownteen16 %in% c(-3) ~ 7,
      hownteen16 %in% c(-8) ~ 8,
      hownteen16 %in% c(-9) ~ 9,
      TRUE ~ 7
    ),
    hown17 = case_when(
      hownteen17 %in% c(1, 2, 3) ~ 1,
      hownteen17 %in% c(4, 5, 6) ~ 2,
      hownteen17 %in% c(7) ~ 3,
      hownteen17 %in% c(8) ~ 4,
      hownteen17 %in% c(-1) ~ 5,
      hownteen17 %in% c(-2) ~ 6,
      hownteen17 %in% c(-3) ~ 7,
      hownteen17 %in% c(-8) ~ 8,
      hownteen17 %in% c(-9) ~ 9,
      TRUE ~ 7
    ),
    hown18 = case_when(
      hownteen18 %in% c(1, 2, 3) ~ 1,
      hownteen18 %in% c(4, 5, 6) ~ 2,
      hownteen18 %in% c(7) ~ 3,
      hownteen18 %in% c(8) ~ 4,
      hownteen18 %in% c(-1) ~ 5,
      hownteen18 %in% c(-2) ~ 6,
      hownteen18 %in% c(-3) ~ 7,
      hownteen18 %in% c(-8) ~ 8,
      hownteen18 %in% c(-9) ~ 9,
      TRUE ~ 7
    ),
    hown19 = case_when(
      hownteen19 %in% c(1, 2, 3) ~ 1,
      hownteen19 %in% c(4, 5, 6) ~ 2,
      hownteen19 %in% c(7) ~ 3,
      hownteen19 %in% c(8) ~ 4,
      hownteen19 %in% c(-1) ~ 5,
      hownteen19 %in% c(-2) ~ 6,
      hownteen19 %in% c(-3) ~ 7,
      hownteen19 %in% c(-8) ~ 8,
      hownteen19 %in% c(-9) ~ 9,
      TRUE ~ 7
    ),
    hown20 = case_when(
      hownteen20 %in% c(1, 2, 3) ~ 1,
      hownteen20 %in% c(4, 5, 6) ~ 2,
      hownteen20 %in% c(7) ~ 3,
      hownteen20 %in% c(8) ~ 4,
      hownteen20 %in% c(-1) ~ 5,
      hownteen20 %in% c(-2) ~ 6,
      hownteen20 %in% c(-3) ~ 7,
      hownteen20 %in% c(-8) ~ 8,
      hownteen20 %in% c(-9) ~ 9,
      TRUE ~ 7
    )
  )

# Create collapsed variables for adult ages 25 and 32
# Mapping: 1-3->1 (owned), 4->2 (rent), 5-6->3 (rent-free/squatting), 7->4 (other)
# Missing: -1->5 (not applicable), -2->6 (script error), -3->7 (not asked), -8->8 (don't know), -9->9 (refusal)

merged_data <- merged_data %>%
  mutate(
    hown25 = case_when(
      hown25_temp %in% c(1, 2, 3) ~ 1,
      hown25_temp %in% c(4) ~ 2,
      hown25_temp %in% c(5, 6) ~ 3,
      hown25_temp %in% c(7) ~ 4,
      hown25_temp %in% c(-1) ~ 5,
      hown25_temp %in% c(-2) ~ 6,
      hown25_temp %in% c(-3) ~ 7,
      hown25_temp %in% c(-8) ~ 8,
      hown25_temp %in% c(-9) ~ 9,
      TRUE ~ 7
    ),
    hown32 = case_when(
      hown32_temp %in% c(1, 2, 3) ~ 1,
      hown32_temp %in% c(4) ~ 2,
      hown32_temp %in% c(5, 6) ~ 3,
      hown32_temp %in% c(7) ~ 4,
      hown32_temp %in% c(-1) ~ 5,
      hown32_temp %in% c(-2) ~ 6,
      hown32_temp %in% c(-3) ~ 7,
      hown32_temp %in% c(-8) ~ 8,
      hown32_temp %in% c(-9) ~ 9,
      TRUE ~ 7
    )
  )

# Select only required variables for output
cleaned_data <- merged_data %>%
  select(NSID, hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32,
         hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20)

# Convert to factors with proper labels for detailed adolescent variables (13 categories)
# Codes: 1-8 are valid, -1=-1 (not applicable), -2=-2 (script error), -3=-3 (not asked), -8=-8 (don't know), -9=-9 (refusal)
# For factor labels, we map: 1-8 to 1-8, -1 to 9, -2 to 10, -3 to 11, -8 to 12, -9 to 13

cleaned_data <- cleaned_data %>%
  mutate(
    hownteen14 = factor(hownteen14, levels = c(1:8, -1, -2, -3, -8, -9),
                        labels = c("Owned outright", "Being bought on a mortgage/bank loan", 
                                   "Shared ownership (owns & rents property)", "Rented from a Council or New Town",
                                   "Rented from a Housing Association", "Rented privately", 
                                   "Rent free", "Some other arrangement",
                                   "Item not applicable", "Script error/information lost",
                                   "Not asked at the fieldwork stage/participated/interviewed",
                                   "Don't know/insufficient information", "Refusal")),
    hownteen15 = factor(hownteen15, levels = c(1:8, -1, -2, -3, -8, -9),
                        labels = c("Owned outright", "Being bought on a mortgage/bank loan", 
                                   "Shared ownership (owns & rents property)", "Rented from a Council or New Town",
                                   "Rented from a Housing Association", "Rented privately", 
                                   "Rent free", "Some other arrangement",
                                   "Item not applicable", "Script error/information lost",
                                   "Not asked at the fieldwork stage/participated/interviewed",
                                   "Don't know/insufficient information", "Refusal")),
    hownteen16 = factor(hownteen16, levels = c(1:8, -1, -2, -3, -8, -9),
                        labels = c("Owned outright", "Being bought on a mortgage/bank loan", 
                                   "Shared ownership (owns & rents property)", "Rented from a Council or New Town",
                                   "Rented from a Housing Association", "Rented privately", 
                                   "Rent free", "Some other arrangement",
                                   "Item not applicable", "Script error/information lost",
                                   "Not asked at the fieldwork stage/participated/interviewed",
                                   "Don't know/insufficient information", "Refusal")),
    hownteen17 = factor(hownteen17, levels = c(1:8, -1, -2, -3, -8, -9),
                        labels = c("Owned outright", "Being bought on a mortgage/bank loan", 
                                   "Shared ownership (owns & rents property)", "Rented from a Council or New Town",
                                   "Rented from a Housing Association", "Rented privately", 
                                   "Rent free", "Some other arrangement",
                                   "Item not applicable", "Script error/information lost",
                                   "Not asked at the fieldwork stage/participated/interviewed",
                                   "Don't know/insufficient information", "Refusal")),
    hownteen18 = factor(hownteen18, levels = c(1:8, -1, -2, -3, -8, -9),
                        labels = c("Owned outright", "Being bought on a mortgage/bank loan", 
                                   "Shared ownership (owns & rents property)", "Rented from a Council or New Town",
                                   "Rented from a Housing Association", "Rented privately", 
                                   "Rent free", "Some other arrangement",
                                   "Item not applicable", "Script error/information lost",
                                   "Not asked at the fieldwork stage/participated/interviewed",
                                   "Don't know/insufficient information", "Refusal")),
    hownteen19 = factor(hownteen19, levels = c(1:8, -1, -2, -3, -8, -9),
                        labels = c("Owned outright", "Being bought on a mortgage/bank loan", 
                                   "Shared ownership (owns & rents property)", "Rented from a Council or New Town",
                                   "Rented from a Housing Association", "Rented privately", 
                                   "Rent free", "Some other arrangement",
                                   "Item not applicable", "Script error/information lost",
                                   "Not asked at the fieldwork stage/participated/interviewed",
                                   "Don't know/insufficient information", "Refusal")),
    hownteen20 = factor(hownteen20, levels = c(1:8, -1, -2, -3, -8, -9),
                        labels = c("Owned outright", "Being bought on a mortgage/bank loan", 
                                   "Shared ownership (owns & rents property)", "Rented from a Council or New Town",
                                   "Rented from a Housing Association", "Rented privately", 
                                   "Rent free", "Some other arrangement",
                                   "Item not applicable", "Script error/information lost",
                                   "Not asked at the fieldwork stage/participated/interviewed",
                                   "Don't know/insufficient information", "Refusal"))
  )

# Convert collapsed variables to factors (9 categories)
# Codes: 1-4 are valid, 5-9 are missing codes
cleaned_data <- cleaned_data %>%
  mutate(
    hown14 = factor(hown14, levels = 1:9,
                    labels = c("Owned outright", "Owned, buying with help of mortgage/loan", 
                               "Part rent, part mortgage", "Rent it", "live rent-free", 
                               "Other", "Item not applicable", "Script error/information lost",
                               "Not asked at the fieldwork stage/participated/interviewed")),
    hown15 = factor(hown15, levels = 1:9,
                    labels = c("Owned outright", "Owned, buying with help of mortgage/loan", 
                               "Part rent, part mortgage", "Rent it", "live rent-free", 
                               "Other", "Item not applicable", "Script error/information lost",
                               "Not asked at the fieldwork stage/participated/interviewed")),
    hown16 = factor(hown16, levels = 1:9,
                    labels = c("Owned outright", "Owned, buying with help of mortgage/loan", 
                               "Part rent, part mortgage", "Rent it", "live rent-free", 
                               "Other", "Item not applicable", "Script error/information lost",
                               "Not asked at the fieldwork stage/participated/interviewed")),
    hown17 = factor(hown17, levels = 1:9,
                    labels = c("Owned outright", "Owned, buying with help of mortgage/loan", 
                               "Part rent, part mortgage", "Rent it", "live rent-free", 
                               "Other", "Item not applicable", "Script error/information lost",
                               "Not asked at the fieldwork stage/participated/interviewed")),
    hown18 = factor(hown18, levels = 1:9,
                    labels = c("Owned outright", "Owned, buying with help of mortgage/loan", 
                               "Part rent, part mortgage", "Rent it", "live rent-free", 
                               "Other", "Item not applicable", "Script error/information lost",
                               "Not asked at the fieldwork stage/participated/interviewed")),
    hown19 = factor(hown19, levels = 1:9,
                    labels = c("Owned outright", "Owned, buying with help of mortgage/loan", 
                               "Part rent, part mortgage", "Rent it", "live rent-free", 
                               "Other", "Item not applicable", "Script error/information lost",
                               "Not asked at the fieldwork stage/participated/interviewed")),
    hown20 = factor(hown20, levels = 1:9,
                    labels = c("Owned outright", "Owned, buying with help of mortgage/loan", 
                               "Part rent, part mortgage", "Rent it", "live rent-free", 
                               "Other", "Item not applicable", "Script error/information lost",
                               "Not asked at the fieldwork stage/participated/interviewed")),
    hown25 = factor(hown25, levels = 1:9,
                    labels = c("Owned outright", "Owned, buying with help of mortgage/loan", 
                               "Part rent, part mortgage", "Rent it", "live rent-free", 
                               "Other", "Item not applicable", "Script error/information lost",
                               "Not asked at the fieldwork stage/participated/interviewed")),
    hown32 = factor(hown32, levels = 1:9,
                    labels = c("Owned outright", "Owned, buying with help of mortgage/loan", 
                               "Part rent, part mortgage", "Rent it", "live rent-free", 
                               "Other", "Item not applicable", "Script error/information lost",
                               "Not asked at the fieldwork stage/participated/interviewed"))
  )

# Write output to CSV
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
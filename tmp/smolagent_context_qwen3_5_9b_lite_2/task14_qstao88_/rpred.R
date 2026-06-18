library(dplyr)
library(tidyr)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Map numeric values to standard codes for wave1
wave1 <- wave1 %>%
  mutate(W1hous12HH = case_when(
    as.character(W1hous12HH) == "-999" ~ -3,
    as.character(W1hous12HH) == "-92" ~ -9,
    as.character(W1hous12HH) == "-91" ~ -1,
    as.character(W1hous12HH) == "-1" ~ -8,
    W1hous12HH == 1 ~ 1,
    W1hous12HH == 2 ~ 2,
    W1hous12HH == 3 ~ 3,
    W1hous12HH == 4 ~ 4,
    W1hous12HH == 5 ~ 5,
    W1hous12HH == 6 ~ 6,
    W1hous12HH == 7 ~ 7,
    W1hous12HH == 8 ~ 8,
    TRUE ~ NA_integer_
  ))

# Map numeric values to standard codes for wave2
wave2 <- wave2 %>%
  mutate(W2Hous12HH = case_when(
    as.character(W2Hous12HH) == "-998" ~ -2,
    as.character(W2Hous12HH) == "-997" ~ -2,
    as.character(W2Hous12HH) == "-995" ~ -2,
    as.character(W2Hous12HH) == "-99" ~ -3,
    as.character(W2Hous12HH) == "-92" ~ -9,
    as.character(W2Hous12HH) == "-91" ~ -1,
    as.character(W2Hous12HH) == "-1" ~ -8,
    W2Hous12HH == 1 ~ 1,
    W2Hous12HH == 2 ~ 2,
    W2Hous12HH == 3 ~ 3,
    W2Hous12HH == 4 ~ 4,
    W2Hous12HH == 5 ~ 5,
    W2Hous12HH == 6 ~ 6,
    W2Hous12HH == 7 ~ 7,
    W2Hous12HH == 8 ~ 8,
    TRUE ~ NA_integer_
  ))

# Map numeric values to standard codes for wave3
wave3 <- wave3 %>%
  mutate(W3hous12HH = case_when(
    as.character(W3hous12HH) == "-999" ~ -3,
    as.character(W3hous12HH) == "-99" ~ -3,
    as.character(W3hous12HH) == "-92" ~ -9,
    as.character(W3hous12HH) == "-91" ~ -1,
    as.character(W3hous12HH) == "-1" ~ -8,
    W3hous12HH == 1 ~ 1,
    W3hous12HH == 2 ~ 2,
    W3hous12HH == 3 ~ 3,
    W3hous12HH == 4 ~ 4,
    W3hous12HH == 5 ~ 5,
    W3hous12HH == 6 ~ 6,
    W3hous12HH == 7 ~ 7,
    W3hous12HH == 8 ~ 8,
    TRUE ~ NA_integer_
  ))

# Map numeric values to standard codes for wave4
wave4 <- wave4 %>%
  mutate(W4Hous12HH = case_when(
    as.character(W4Hous12HH) == "-999" ~ -3,
    as.character(W4Hous12HH) == "-997" ~ -2,
    as.character(W4Hous12HH) == "-92" ~ -9,
    as.character(W4Hous12HH) == "-91" ~ -1,
    as.character(W4Hous12HH) == "-1" ~ -8,
    W4Hous12HH == 1 ~ 1,
    W4Hous12HH == 2 ~ 2,
    W4Hous12HH == 3 ~ 3,
    W4Hous12HH == 4 ~ 4,
    W4Hous12HH == 5 ~ 5,
    W4Hous12HH == 6 ~ 6,
    W4Hous12HH == 7 ~ 7,
    W4Hous12HH == 8 ~ 8,
    TRUE ~ NA_integer_
  ))

# Map numeric values to standard codes for wave5
wave5 <- wave5 %>%
  mutate(W5Hous12HH = case_when(
    as.character(W5Hous12HH) == "-999" ~ -3,
    as.character(W5Hous12HH) == "-92" ~ -9,
    as.character(W5Hous12HH) == "-91" ~ -1,
    as.character(W5Hous12HH) == "-1" ~ -8,
    W5Hous12HH == 1 ~ 1,
    W5Hous12HH == 2 ~ 2,
    W5Hous12HH == 3 ~ 3,
    as.character(W5Hous12HH) == "6" ~ -1,
    TRUE ~ NA_integer_
  ))

# Map numeric values to standard codes for wave6
wave6 <- wave6 %>%
  mutate(W6Hous12YP = case_when(
    as.character(W6Hous12YP) == "-92" ~ -9,
    as.character(W6Hous12YP) == "-91" ~ -1,
    as.character(W6Hous12YP) == "-1" ~ -8,
    W6Hous12YP == 1 ~ 1,
    W6Hous12YP == 2 ~ 2,
    W6Hous12YP == 3 ~ 3,
    TRUE ~ NA_integer_
  ))

# Map numeric values to standard codes for wave7
wave7 <- wave7 %>%
  mutate(W7Hous12YP = case_when(
    as.character(W7Hous12YP) == "-92" ~ -9,
    as.character(W7Hous12YP) == "-91" ~ -1,
    as.character(W7Hous12YP) == "-1" ~ -8,
    W7Hous12YP == 1 ~ 1,
    W7Hous12YP == 2 ~ 2,
    W7Hous12YP == 3 ~ 3,
    TRUE ~ NA_integer_
  ))

# Map numeric values to standard codes for wave8
wave8 <- wave8 %>%
  mutate(W8TENURE = case_when(
    as.character(W8TENURE) == "-9" ~ -9,
    as.character(W8TENURE) == "-8" ~ -8,
    as.character(W8TENURE) == "-1" ~ -1,
    W8TENURE == 1 ~ 1,
    W8TENURE == 2 ~ 2,
    W8TENURE == 3 ~ 3,
    W8TENURE == 4 ~ 4,
    W8TENURE == 5 ~ 5,
    W8TENURE == 6 ~ 6,
    W8TENURE == 7 ~ 7,
    TRUE ~ NA_integer_
  ))

# Map numeric values to standard codes for wave9  
wave9 <- wave9 %>%
  mutate(W9DTENURE = case_when(
    as.character(W9DTENURE) == "-8" ~ -8,
    W9DTENURE == 1 ~ 1,
    W9DTENURE == 2 ~ 2,
    W9DTENURE == 3 ~ 3,
    W9DTENURE == 4 ~ 4,
    W9DTENURE == 5 ~ 5,
    W9DTENURE == 6 ~ 6,
    W9DTENURE == 7 ~ 7,
    TRUE ~ NA_integer_
  ))

# Merge all datasets
all_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Create detailed teen variables (ages 14-20)
all_data <- all_data %>%
  mutate(
    hownteen14 = ifelse(is.na(W1hous12HH) | W1hous12HH %in% c(-3, -9, -1, -8), -3, W1hous12HH),
    hownteen15 = ifelse(is.na(W2Hous12HH) | W2Hous12HH %in% c(-2, -3, -9, -1, -8), -3, W2Hous12HH),
    hownteen16 = ifelse(is.na(W3hous12HH) | W3hous12HH %in% c(-3, -9, -1, -8), -3, W3hous12HH),
    hownteen17 = ifelse(is.na(W4Hous12HH) | W4Hous12HH %in% c(-3, -2, -9, -1, -8), -3, W4Hous12HH),
    hownteen18 = ifelse(is.na(W5Hous12HH) | W5Hous12HH %in% c(-3, -9, -1, -8), -3, W5Hous12HH),
    hownteen19 = ifelse(is.na(W6Hous12YP) | W6Hous12YP %in% c(-9, -1, -8), -3, W6Hous12YP),
    hownteen20 = ifelse(is.na(W7Hous12YP) | W7Hous12YP %in% c(-9, -1, -8), -3, W7Hous12YP)
  )

# Create collapsed variables - Ages 14-18 use family background vars
all_data <- all_data %>%
  mutate(
    hown14 = case_when(
      W1hous12HH %in% c(1, 2, 3) ~ 1,
      W1hous12HH %in% c(4, 5, 6, 7, 8) ~ 2,
      TRUE ~ -1
    ),
    hown15 = case_when(
      W2Hous12HH %in% c(1, 2, 3) ~ 1,
      W2Hous12HH %in% c(4, 5, 6, 7, 8) ~ 2,
      TRUE ~ -1
    ),
    hown16 = case_when(
      W3hous12HH %in% c(1, 2, 3) ~ 1,
      W3hous12HH %in% c(4, 5, 6, 7, 8) ~ 2,
      TRUE ~ -1
    ),
    hown17 = case_when(
      W4Hous12HH %in% c(1, 2, 3) ~ 1,
      W4Hous12HH %in% c(4, 5, 6, 7, 8) ~ 2,
      TRUE ~ -1
    ),
    hown18 = case_when(
      W5Hous12HH == 1 ~ 1,
      W5Hous12HH == 2 ~ 2,
      W5Hous12HH == 3 ~ 2,
      TRUE ~ -1
    )
  )

# Create collapsed variables - Ages 19-20
all_data <- all_data %>%
  mutate(
    hown19 = case_when(
      W6Hous12YP == 1 ~ 1,
      W6Hous12YP == 2 ~ 2,
      TRUE ~ -1
    ),
    hown20 = case_when(
      W7Hous12YP == 1 ~ 1,
      W7Hous12YP == 2 ~ 2,
      TRUE ~ -1
    )
  )

# Create collapsed variables - Ages 25 and 32
all_data <- all_data %>%
  mutate(
    hown25 = case_when(
      W8TENURE %in% c(1, 2, 3) ~ 1,
      W8TENURE %in% c(4, 5, 6, 7) ~ 2,
      TRUE ~ -1
    ),
    hown32 = case_when(
      W9DTENURE %in% c(1, 2, 3) ~ 1,
      W9DTENURE %in% c(4, 5, 6, 7) ~ 2,
      TRUE ~ -1
    )
  )

# Create labels
all_data <- all_data %>%
  mutate(
    hownteen14 = factor(hownteen14, levels = c(-3, -9, -8, -1, 1, 2, 3, 4, 5, 6, 7, 8),
                        labels = c("Not asked at the fieldwork stage / not interviewed",
                                  "Refusal",
                                  "Don't know / insufficient information",
                                  "Not applicable",
                                  "Own outright",
                                  "Being bought on a mortgage/ bank loan",
                                  "Shared ownership (owns & rents property)",
                                  "Rented from a Council or New Town",
                                  "Rented from a Housing Association",
                                  "Rented privately",
                                  "Rent free",
                                  "Some other arrangement")),
    hownteen15 = factor(hownteen15, levels = c(-3, -9, -8, -1, 1, 2, 3, 4, 5, 6, 7, 8),
                        labels = c("Not asked at the fieldwork stage / not interviewed",
                                  "Refusal",
                                  "Don't know / insufficient information",
                                  "Not applicable",
                                  "Own outright",
                                  "Being bought on a mortgage/ bank loan",
                                  "Shared ownership (owns & rents property)",
                                  "Rented from a Council or New Town",
                                  "Rented from a Housing Association",
                                  "Rented privately",
                                  "Rent free",
                                  "Some other arrangement")),
    hownteen16 = factor(hownteen16, levels = c(-3, -9, -8, -1, 1, 2, 3, 4, 5, 6, 7, 8),
                        labels = c("Not asked at the fieldwork stage / not interviewed",
                                  "Refusal",
                                  "Don't know / insufficient information",
                                  "Not applicable",
                                  "Own outright",
                                  "Being bought on a mortgage/ bank loan",
                                  "Shared ownership (owns & rents property)",
                                  "Rented from a Council or New Town",
                                  "Rented from a Housing Association",
                                  "Rented privately",
                                  "Rent free",
                                  "Some other arrangement")),
    hownteen17 = factor(hownteen17, levels = c(-3, -9, -8, -1, 1, 2, 3, 4, 5, 6, 7, 8),
                        labels = c("Not asked at the fieldwork stage / not interviewed",
                                  "Refusal",
                                  "Don't know / insufficient information",
                                  "Not applicable",
                                  "Own outright",
                                  "Being bought on a mortgage/ bank loan",
                                  "Shared ownership (owns & rents property)",
                                  "Rented from a Council or New Town",
                                  "Rented from a Housing Association",
                                  "Rented privately",
                                  "Rent free",
                                  "Some other arrangement")),
    hownteen18 = factor(hownteen18, levels = c(-3, -9, -8, -1, 1, 2, 3, 4, 5, 6, 7, 8),
                        labels = c("Not asked at the fieldwork stage / not interviewed",
                                  "Refusal",
                                  "Don't know / insufficient information",
                                  "Not applicable",
                                  "Own outright",
                                  "Being bought on a mortgage/ bank loan",
                                  "Shared ownership (owns & rents property)",
                                  "Rented from a Council or New Town",
                                  "Rented from a Housing Association",
                                  "Rented privately",
                                  "Rent free",
                                  "Some other arrangement")),
    hownteen19 = factor(hownteen19, levels = c(-3, -9, -8, -1, 1, 2, 3, 4, 5, 6, 7, 8),
                        labels = c("Not asked at the fieldwork stage / not interviewed",
                                  "Refusal",
                                  "Don't know / insufficient information",
                                  "Not applicable",
                                  "Own outright",
                                  "Being bought on a mortgage/ bank loan",
                                  "Shared ownership (owns & rents property)",
                                  "Rented from a Council or New Town",
                                  "Rented from a Housing Association",
                                  "Rented privately",
                                  "Rent free",
                                  "Some other arrangement")),
    hownteen20 = factor(hownteen20, levels = c(-3, -9, -8, -1, 1, 2, 3, 4, 5, 6, 7, 8),
                        labels = c("Not asked at the fieldwork stage / not interviewed",
                                  "Refusal",
                                  "Don't know / insufficient information",
                                  "Not applicable",
                                  "Own outright",
                                  "Being bought on a mortgage/ bank loan",
                                  "Shared ownership (owns & rents property)",
                                  "Rented from a Council or New Town",
                                  "Rented from a Housing Association",
                                  "Rented privately",
                                  "Rent free",
                                  "Some other arrangement")),
    hown14 = factor(hown14, levels = c(-1, 1, 2), labels = c("Not applicable", "Own outright", "Rent it")),
    hown15 = factor(hown15, levels = c(-1, 1, 2), labels = c("Not applicable", "Own outright", "Rent it")),
    hown16 = factor(hown16, levels = c(-1, 1, 2), labels = c("Not applicable", "Own outright", "Rent it")),
    hown17 = factor(hown17, levels = c(-1, 1, 2), labels = c("Not applicable", "Own outright", "Rent it")),
    hown18 = factor(hown18, levels = c(-1, 1, 2), labels = c("Not applicable", "Own outright", "Rent it")),
    hown19 = factor(hown19, levels = c(-1, 1, 2), labels = c("Not applicable", "Own outright", "Rent it")),
    hown20 = factor(hown20, levels = c(-1, 1, 2), labels = c("Not applicable", "Own outright", "Rent it")),
    hown25 = factor(hown25, levels = c(-1, 1, 2), labels = c("Not applicable", "Own outright", "Rent it")),
    hown32 = factor(hown32, levels = c(-1, 1, 2), labels = c("Not applicable", "Own outright", "Rent it"))
  )

# Select only final variables
final_vars <- c("NSID", paste0("hownteen", 14:20), paste0("hown", c(14, 15, 16, 17, 18, 19, 20, 25, 32)))

cleaned_data <- all_data %>% select(all_of(final_vars))

# Write output
write_csv(cleaned_data, "data/output/cleaned_data.csv")

cat("Variables in output:", paste(colnames(cleaned_data), collapse = ", "), "\n")
cat("Number of rows:", nrow(cleaned_data), "\n")
cat("First 5 rows:\n")
print(head(cleaned_data, 5))
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directories if they don't exist
if (!dir.exists("data/input")) {
  dir.create("data/input", recursive = TRUE)
}
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Define file paths
files <- list(
  wave1 = "data/input/wave_one_lsype_family_background_2020.tab",
  wave2 = "data/input/wave_two_lsype_family_background_2020.tab",
  wave3 = "data/input/wave_three_lsype_family_background_2020.tab",
  wave4 = "data/input/wave_four_lsype_family_background_2020.tab",
  wave5 = "data/input/wave_five_lsype_family_background_2020.tab",
  wave6 = "data/input/wave_six_lsype_young_person_2020.tab",
  wave7 = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave8 = "data/input/ns8_2015_main_interview.tab",
  wave9 = "data/input/ns9_2022_derived_variables.tab"
)

# Wave 1 (Age 14) - only one variable
w1 <- read_delim(files$wave1, delim = "\t")

# Wave 2 (Age 15) - only one variable
w2 <- read_delim(files$wave2, delim = "\t")

# Wave 3 (Age 16) - only one variable
w3 <- read_delim(files$wave3, delim = "\t")

# Wave 4 (Age 17) - only one variable
w4 <- read_delim(files$wave4, delim = "\t")

# Wave 5 (Age 18) - three variables (type + subtypes)
w5 <- read_delim(files$wave5, delim = "\t")

# Wave 6 (Age 19) - three variables (type + subtypes)
w6 <- read_delim(files$wave6, delim = "\t")

# Wave 7 (Age 20) - three variables (type + subtypes)
w7 <- read_delim(files$wave7, delim = "\t")

# Wave 8 (Age 25) - only one variable
w8 <- read_delim(files$wave8, delim = "\t")

# Wave 9 (Age 32) - only one variable
w9 <- read_delim(files$wave9, delim = "\t")

# Function to map missing values to standard codes
# Standard: -9=Refusal, -8=Don't know, -1=Not applicable, -3=Not asked

# For waves 1-4, 5-7, 8-9 we need to map their specific codes

# Wave 1 (Age 14): user_missing_values: '-999.0 thru -1.0'
map_w1 <- function(x) {
  x %>%
    replace(x == -999, -3) %>%
    replace(x == -92, -9) %>%
    replace(x == -91, -1) %>%
    replace(x == -1, -8)
}

# Wave 2 (Age 15): user_missing_values: '-999.0 thru -1.0'
map_w2 <- function(x) {
  x %>%
    replace(x == -998, -2) %>%
    replace(x == -997, -2) %>%
    replace(x == -995, -2) %>%
    replace(x == -999, -3) %>%
    replace(x == -92, -9) %>%
    replace(x == -91, -1) %>%
    replace(x == -1, -8)
}

# Wave 3 (Age 16): user_missing_values: '-999.0 thru -1.0'
map_w3 <- function(x) {
  x %>%
    replace(x == -999, -3) %>%
    replace(x == -99, -3) %>%
    replace(x == -92, -9) %>%
    replace(x == -91, -1) %>%
    replace(x == -1, -8)
}

# Wave 4 (Age 17): user_missing_values: '-999.0 thru -1.0'
map_w4 <- function(x) {
  x %>%
    replace(x == -998, -2) %>%
    replace(x == -997, -2) %>%
    replace(x == -995, -2) %>%
    replace(x == -999, -3) %>%
    replace(x == -92, -9) %>%
    replace(x == -91, -1) %>%
    replace(x == -1, -8)
}

# Wave 5 (Age 18): user_missing_values: '-999.0 thru -1.0'
map_w5 <- function(x) {
  x %>%
    replace(x == -999, -3) %>%
    replace(x == -92, -9) %>%
    replace(x == -91, -1) %>%
    replace(x == -1, -8)
}

# Wave 6 (Age 19): user_missing_values: '-999.0 thru -1.0'
map_w6 <- function(x) {
  x %>%
    replace(x == -999, -3) %>%
    replace(x == -92, -9) %>%
    replace(x == -91, -1) %>%
    replace(x == -1, -8)
}

# Wave 7 (Age 20): user_missing_values: '-999.0 thru -1.0'
map_w7 <- function(x) {
  x %>%
    replace(x == -999, -3) %>%
    replace(x == -92, -9) %>%
    replace(x == -91, -1) %>%
    replace(x == -1, -8)
}

# Wave 8 (Age 25): user_missing_values: '-9.0 thru -8.0 and -1.0'
map_w8 <- function(x) {
  x %>%
    replace(x == -9, -9) %>%
    replace(x == -8, -8) %>%
    replace(x == -1, -1)
}

# Wave 9 (Age 32): user_missing_values: '-8.0 thru None'
map_w9 <- function(x) {
  x %>%
    replace(x == -8, -8) %>%
    # Replace NA/None with -1
    replace(is.na(x), -1)
}

# Apply missing value mappings
w1$W1hous12HH <- map_w1(w1$W1hous12HH)
w2$W2Hous12HH <- map_w2(w2$W2Hous12HH)
w3$W3hous12HH <- map_w3(w3$W3hous12HH)
w4$W4Hous12HH <- map_w4(w4$W4Hous12HH)
w5$W5Hous12HH <- map_w5(w5$W5Hous12HH)
w5$W5Hous12BHH <- map_w5(w5$W5Hous12BHH)
w5$W5Hous12CHH <- map_w5(w5$W5Hous12CHH)
w6$W6Hous12YP <- map_w6(w6$W6Hous12YP)
w6$W6Hous12bYP <- map_w6(w6$W6Hous12bYP)
w6$W6Hous12cYP <- map_w6(w6$W6Hous12cYP)
w7$W7Hous12YP <- map_w7(w7$W7Hous12YP)
w7$W7Hous12bYP <- map_w7(w7$W7Hous12bYP)
w7$W7Hous12cYP <- map_w7(w7$W7Hous12cYP)
w8$W8TENURE <- map_w8(w8$W8TENURE)
w9$W9DTENURE <- map_w9(w9$W9DTENURE)

# Merge all waves by NSID
all_data <- w1
all_data <- full_join(all_data, w2, by = "NSID")
all_data <- full_join(all_data, w3, by = "NSID")
all_data <- full_join(all_data, w4, by = "NSID")
all_data <- full_join(all_data, w5, by = "NSID")
all_data <- full_join(all_data, w6, by = "NSID")
all_data <- full_join(all_data, w7, by = "NSID")
all_data <- full_join(all_data, w8, by = "NSID")
all_data <- full_join(all_data, w9, by = "NSID")

# Create detailed adolescent variables (hownteen14-20)
# Wave 1 (Age 14)
all_data$hownteen14 <- case_when(
  all_data$W1hous12HH == 1 ~ "Owned outright",
  all_data$W1hous12HH == 2 ~ "Being bought on a mortgage/ bank loan",
  all_data$W1hous12HH == 3 ~ "Shared ownership (owns & rents property)",
  all_data$W1hous12HH == 4 ~ "Rented from a Council or New Town",
  all_data$W1hous12HH == 5 ~ "Rented from a Housing Association",
  all_data$W1hous12HH == 6 ~ "Rented privately",
  all_data$W1hous12HH == 7 ~ "Rent free",
  all_data$W1hous12HH == 8 ~ "Some other arrangement",
  all_data$W1hous12HH == -9 ~ "Refusal",
  all_data$W1hous12HH == -8 ~ "Don't know/insufficient information",
  all_data$W1hous12HH == -1 ~ "Item not applicable",
  TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
)

# Wave 2 (Age 15)
all_data$hownteen15 <- case_when(
  all_data$W2Hous12HH == 1 ~ "Owned outright",
  all_data$W2Hous12HH == 2 ~ "Being bought on a mortgage/ bank loan",
  all_data$W2Hous12HH == 3 ~ "Shared ownership (owns & rents property)",
  all_data$W2Hous12HH == 4 ~ "Rented from a Council or New Town",
  all_data$W2Hous12HH == 5 ~ "Rented from a Housing Association",
  all_data$W2Hous12HH == 6 ~ "Rented privately",
  all_data$W2Hous12HH == 7 ~ "Rent free",
  all_data$W2Hous12HH == 8 ~ "Some other arrangement",
  all_data$W2Hous12HH == -9 ~ "Refusal",
  all_data$W2Hous12HH == -8 ~ "Don't know/insufficient information",
  all_data$W2Hous12HH == -1 ~ "Item not applicable",
  TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
)

# Wave 3 (Age 16)
all_data$hownteen16 <- case_when(
  all_data$W3hous12HH == 1 ~ "Owned outright",
  all_data$W3hous12HH == 2 ~ "Being bought on a mortgage/ bank loan",
  all_data$W3hous12HH == 3 ~ "Shared ownership (owns & rents property)",
  all_data$W3hous12HH == 4 ~ "Rented from a Council or New Town",
  all_data$W3hous12HH == 5 ~ "Rented from a Housing Association",
  all_data$W3hous12HH == 6 ~ "Rented privately",
  all_data$W3hous12HH == 7 ~ "Rent free",
  all_data$W3hous12HH == 8 ~ "Some other arrangement",
  all_data$W3hous12HH == -9 ~ "Refusal",
  all_data$W3hous12HH == -8 ~ "Don't know/insufficient information",
  all_data$W3hous12HH == -1 ~ "Item not applicable",
  TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
)

# Wave 4 (Age 17)
all_data$hownteen17 <- case_when(
  all_data$W4Hous12HH == 1 ~ "Owned outright",
  all_data$W4Hous12HH == 2 ~ "Being bought on a mortgage/ bank loan",
  all_data$W4Hous12HH == 3 ~ "Shared ownership (owns & rents property)",
  all_data$W4Hous12HH == 4 ~ "Rented from a Council or New Town",
  all_data$W4Hous12HH == 5 ~ "Rented from a Housing Association",
  all_data$W4Hous12HH == 6 ~ "Rented privately",
  all_data$W4Hous12HH == 7 ~ "Rent free",
  all_data$W4Hous12HH == 8 ~ "Some other arrangement",
  all_data$W4Hous12HH == -9 ~ "Refusal",
  all_data$W4Hous12HH == -8 ~ "Don't know/insufficient information",
  all_data$W4Hous12HH == -1 ~ "Item not applicable",
  TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
)

# Create collapsed variables for ages 14-20
# hown14-20 should be based on teen data
all_data$hown14_20 <- case_when(
  all_data$hownteen14 == "Owned outright" | all_data$hownteen14 == "Being bought on a mortgage/ bank loan" ~ "Owned outright",
  all_data$hownteen14 == "Shared ownership (owns & rents property)" ~ "Part rent, part mortgage",
  all_data$hownteen14 == "Rented from a Council or New Town" | all_data$hownteen14 == "Rented from a Housing Association" | all_data$hownteen14 == "Rented privately" | all_data$hownteen14 == "Rent free" ~ "Rent it",
  all_data$hownteen14 == "Some other arrangement" | is.na(all_data$hownteen14) ~ "Other",
  all_data$hownteen14 == "Item not applicable" ~ "Item not applicable",
  all_data$hownteen14 == "Script error/information lost" ~ "Script error/information lost",
  all_data$hownteen14 == "Not asked at the fieldwork stage/participated/interviewed" ~ "Not asked at the fieldwork stage/participated/interviewed",
  all_data$hownteen14 == "Don't know/insufficient information" ~ "Don't know/insufficient information",
  all_data$hownteen14 == "Refusal" ~ "Refusal",
  TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
)

# Create collapsed variable for age 25
all_data$hown25 <- case_when(
  all_data$W8TENURE %in% c(1, 2) ~ "Owned outright",
  all_data$W8TENURE %in% c(3) ~ "Part rent, part mortgage",
  all_data$W8TENURE %in% c(4, 5, 7) ~ "Rent it",
  all_data$W8TENURE %in% c(6) ~ "Other",
  all_data$W8TENURE == -9 ~ "Refusal",
  all_data$W8TENURE == -8 ~ "Don't know/insufficient information",
  all_data$W8TENURE == -1 ~ "Item not applicable",
  TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
)

# Create collapsed variable for age 32
all_data$hown32 <- case_when(
  all_data$W9DTENURE %in% c(1, 2) ~ "Owned outright",
  all_data$W9DTENURE %in% c(3) ~ "Part rent, part mortgage",
  all_data$W9DTENURE %in% c(4, 6, 7) ~ "Rent it",
  all_data$W9DTENURE %in% c(5) ~ "Other",
  all_data$W9DTENURE == -8 ~ "Don't know/insufficient information",
  all_data$W9DTENURE == -1 ~ "Item not applicable",
  TRUE ~ "Not asked at the fieldwork stage/participated/interviewed"
)

# Keep only required variables
keep_vars <- c("NSID",
               "hown14_20", "hown25", "hown32",
               "hownteen14", "hownteen15", "hownteen16", "hownteen17")

cleaned_data <- all_data %>%
  select(all_of(keep_vars))

# Write output
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)

cat("Data cleaning complete!\n")
cat("Total records:", nrow(cleaned_data), "\n")
cat("Total variables:", ncol(cleaned_data), "\n")
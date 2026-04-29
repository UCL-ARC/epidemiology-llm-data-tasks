# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Load all files
data14 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
data15 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
data16 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
data17 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
data18 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
data19 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
data20 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
data25 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
data32 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets using full_join by NSID
cleaned_data <- data14 %>%
  full_join(data15, by = "NSID") %>%
  full_join(data16, by = "NSID") %>%
  full_join(data17, by = "NSID") %>%
  full_join(data18, by = "NSID") %>%
  full_join(data19, by = "NSID") %>%
  full_join(data20, by = "NSID") %>%
  full_join(data25, by = "NSID") %>%
  full_join(data32, by = "NSID")

# Age 14: W1hous12HH
# Labels from metadata: -999, -92, -91, -1, 1, 2, 3, 4, 5, 6, 7, 8
hownteen14 <- cleaned_data$W1hous12HH
cleaned_data$hownteen14 <- factor(hownteen14, 
  levels = c(-999, -92, -91, -1, 1, 2, 3, 4, 5, 6, 7, 8),
  labels = c("Not asked at the fieldwork stage/participated/interviewed",
             "Refusal",
             "Item not applicable",
             "Don't know/insufficient information",
             "Owned outright",
             "Being bought on a mortgage/bank loan",
             "Shared ownership (owns & rents property)",
             "Rented from a Council or New Town",
             "Rented from a Housing Association",
             "Rented privately",
             "Rent free",
             "Some other arrangement"))
cleaned_data$hown14 <- factor(hownteen14,
  levels = c(-999, -92, -91, -1, 1, 2, 3, 4, 5, 6, 7, 8),
  labels = c("Not asked at the fieldwork stage/participated/interviewed",
             "Refusal",
             "Item not applicable",
             "Don't know/insufficient information",
             "Owned outright",
             "Owned, buying with help of mortgage/loan",
             "Part rent, part mortgage",
             "Rent it",
             "Rent it",
             "Rent it",
             "live rent-free",
             "Other"))

# Age 15: W2Hous12HH
# Labels from metadata: -998, -997, -995, -99, -92, -91, -1, 1, 2, 3, 4, 5, 6, 7, 8
hownteen15 <- cleaned_data$W2Hous12HH
cleaned_data$hownteen15 <- factor(hownteen15,
  levels = c(-998, -997, -995, -99, -92, -91, -1, 1, 2, 3, 4, 5, 6, 7, 8),
  labels = c("Script error/information lost",
             "Script error/information lost",
             "Script error/information lost",
             "Not asked at the fieldwork stage/participated/interviewed",
             "Refusal",
             "Item not applicable",
             "Don't know/insufficient information",
             "Owned outright",
             "Being bought on a mortgage/bank loan",
             "Shared ownership (owns & rents property)",
             "Rented from a Council or New Town",
             "Rented from a Housing Association",
             "Rented privately",
             "Rent free",
             "Some other arrangement"))
cleaned_data$hown15 <- factor(hownteen15,
  levels = c(-998, -997, -995, -99, -92, -91, -1, 1, 2, 3, 4, 5, 6, 7, 8),
  labels = c("Script error/information lost",
             "Script error/information lost",
             "Script error/information lost",
             "Not asked at the fieldwork stage/participated/interviewed",
             "Refusal",
             "Item not applicable",
             "Don't know/insufficient information",
             "Owned outright",
             "Owned, buying with help of mortgage/loan",
             "Part rent, part mortgage",
             "Rent it",
             "Rent it",
             "Rent it",
             "live rent-free",
             "Other"))

# Age 16: W3hous12HH
# Labels from metadata: -999, -99, -92, -91, -1, 1, 2, 3, 4, 5, 6, 7, 8
hownteen16 <- cleaned_data$W3hous12HH
cleaned_data$hownteen16 <- factor(hownteen16,
  levels = c(-999, -99, -92, -91, -1, 1, 2, 3, 4, 5, 6, 7, 8),
  labels = c("Not asked at the fieldwork stage/participated/interviewed",
             "Not asked at the fieldwork stage/participated/interviewed",
             "Refusal",
             "Item not applicable",
             "Don't know/insufficient information",
             "Owned outright",
             "Being bought on a mortgage/bank loan",
             "Shared ownership (owns & rents property)",
             "Rented from a Council or New Town",
             "Rented from a Housing Association",
             "Rented privately",
             "Rent free",
             "Some other arrangement"))
cleaned_data$hown16 <- factor(hownteen16,
  levels = c(-999, -99, -92, -91, -1, 1, 2, 3, 4, 5, 6, 7, 8),
  labels = c("Not asked at the fieldwork stage/participated/interviewed",
             "Not asked at the fieldwork stage/participated/interviewed",
             "Refusal",
             "Item not applicable",
             "Don't know/insufficient information",
             "Owned outright",
             "Owned, buying with help of mortgage/loan",
             "Part rent, part mortgage",
             "Rent it",
             "Rent it",
             "Rent it",
             "live rent-free",
             "Other"))

# Age 17: W4Hous12HH
# Labels from metadata: -999, -997, -92, -91, -1, 1, 2, 3, 4, 5, 6, 7, 8
hownteen17 <- cleaned_data$W4Hous12HH
cleaned_data$hownteen17 <- factor(hownteen17,
  levels = c(-999, -997, -92, -91, -1, 1, 2, 3, 4, 5, 6, 7, 8),
  labels = c("Not asked at the fieldwork stage/participated/interviewed",
             "Script error/information lost",
             "Refusal",
             "Item not applicable",
             "Don't know/insufficient information",
             "Owned outright",
             "Being bought on a mortgage/bank loan",
             "Shared ownership (owns & rents property)",
             "Rented from a Council or New Town",
             "Rented from a Housing Association",
             "Rented privately",
             "Rent free",
             "Some other arrangement"))
cleaned_data$hown17 <- factor(hownteen17,
  levels = c(-999, -997, -92, -91, -1, 1, 2, 3, 4, 5, 6, 7, 8),
  labels = c("Not asked at the fieldwork stage/participated/interviewed",
             "Script error/information lost",
             "Refusal",
             "Item not applicable",
             "Don't know/insufficient information",
             "Owned outright",
             "Owned, buying with help of mortgage/loan",
             "Part rent, part mortgage",
             "Rent it",
             "Rent it",
             "Rent it",
             "live rent-free",
             "Other"))

# Age 18: W5Hous12HH + W5Hous12BHH + W5Hous12CHH
# W5Hous12HH: 1=Owned, 2=Rented, 3=Something else
# W5Hous12BHH (for owned): 1=Owned outright, 2=Buying mortgage, 3=Shared ownership, 4=Other
# W5Hous12CHH (for rented): 1=Council, 2=Housing Association, 3=Privately, 4=Rent free, 5=Other

# Create detailed variable for age 18
hownteen18 <- case_when(
  cleaned_data$W5Hous12HH == 1 ~ cleaned_data$W5Hous12BHH,
  cleaned_data$W5Hous12HH == 2 ~ cleaned_data$W5Hous12CHH,
  cleaned_data$W5Hous12HH == 3 ~ 4,  # Something else -> maps to "Some other arrangement"
  TRUE ~ NA_real_
)

# For age 18, the sub-type codes are: -999, -92, -91, -1, 1, 2, 3, 4, 5 (from CHH)
# Combined with 3 for "Something else"
cleaned_data$hownteen18 <- factor(hownteen18,
  levels = c(-999, -92, -91, -1, 1, 2, 3, 4, 5),
  labels = c("Not asked at the fieldwork stage/participated/interviewed",
             "Refusal",
             "Item not applicable",
             "Don't know/insufficient information",
             "Owned outright",
             "Being bought on a mortgage/bank loan",
             "Shared ownership (owns & rents property)",
             "Some other arrangement",
             "Some other arrangement"))

cleaned_data$hown18 <- factor(hownteen18,
  levels = c(-999, -92, -91, -1, 1, 2, 3, 4, 5),
  labels = c("Not asked at the fieldwork stage/participated/interviewed",
             "Refusal",
             "Item not applicable",
             "Don't know/insufficient information",
             "Owned outright",
             "Owned, buying with help of mortgage/loan",
             "Part rent, part mortgage",
             "Other",
             "Other"))

# Age 19: W6Hous12YP + W6Hous12bYP + W6Hous12cYP
# W6Hous12YP: 1=Owned, 2=Rented, 3=Something else
# W6Hous12bYP (for owned): 1=Owned outright, 2=Buying mortgage, 3=Shared ownership, 4=Other
# W6Hous12cYP (for rented): 1=Council, 2=Housing Association, 3=Privately, 4=Rent free, 5=Other

hownteen19 <- case_when(
  cleaned_data$W6Hous12YP == 1 ~ cleaned_data$W6Hous12bYP,
  cleaned_data$W6Hous12YP == 2 ~ cleaned_data$W6Hous12cYP,
  cleaned_data$W6Hous12YP == 3 ~ 4,  # Something else
  TRUE ~ NA_real_
)

cleaned_data$hownteen19 <- factor(hownteen19,
  levels = c(-92, -91, -1, 1, 2, 3, 4, 5),
  labels = c("Refusal",
             "Item not applicable",
             "Don't know/insufficient information",
             "Owned outright",
             "Being bought on a mortgage/bank loan",
             "Shared ownership (owns & rents property)",
             "Some other arrangement",
             "Some other arrangement"))

cleaned_data$hown19 <- factor(hownteen19,
  levels = c(-92, -91, -1, 1, 2, 3, 4, 5),
  labels = c("Refusal",
             "Item not applicable",
             "Don't know/insufficient information",
             "Owned outright",
             "Owned, buying with help of mortgage/loan",
             "Part rent, part mortgage",
             "Other",
             "Other"))

# Age 20: W7Hous12YP + W7Hous12bYP + W7Hous12cYP
# Same structure as age 19

hownteen20 <- case_when(
  cleaned_data$W7Hous12YP == 1 ~ cleaned_data$W7Hous12bYP,
  cleaned_data$W7Hous12YP == 2 ~ cleaned_data$W7Hous12cYP,
  cleaned_data$W7Hous12YP == 3 ~ 4,  # Something else
  TRUE ~ NA_real_
)

cleaned_data$hownteen20 <- factor(hownteen20,
  levels = c(-92, -91, -1, 1, 2, 3, 4, 5),
  labels = c("Refusal",
             "Item not applicable",
             "Don't know/insufficient information",
             "Owned outright",
             "Being bought on a mortgage/bank loan",
             "Shared ownership (owns & rents property)",
             "Some other arrangement",
             "Some other arrangement"))

cleaned_data$hown20 <- factor(hownteen20,
  levels = c(-92, -91, -1, 1, 2, 3, 4, 5),
  labels = c("Refusal",
             "Item not applicable",
             "Don't know/insufficient information",
             "Owned outright",
             "Owned, buying with help of mortgage/loan",
             "Part rent, part mortgage",
             "Other",
             "Other"))

# Age 25: W8TENURE
# Labels: -9=Refused, -8=Don't know, -1=Not applicable, 1-7=tenure types
hownteen25 <- cleaned_data$W8TENURE
cleaned_data$hown25 <- factor(hownteen25,
  levels = c(-9, -8, -1, 1, 2, 3, 4, 5, 6, 7),
  labels = c("Refusal",
             "Don't know/insufficient information",
             "Item not applicable",
             "Owned outright",
             "Owned, buying with help of mortgage/loan",
             "Part rent, part mortgage",
             "Rent it",
             "live rent-free",
             "Other",
             "Other"))

# Age 32: W9DTENURE
# Labels: -8=Insufficient information, 1-7=tenure types
hownteen32 <- cleaned_data$W9DTENURE
cleaned_data$hown32 <- factor(hownteen32,
  levels = c(-8, 1, 2, 3, 4, 5, 6, 7),
  labels = c("Don't know/insufficient information",
             "Owned outright",
             "Owned, buying with help of mortgage/loan",
             "Part rent, part mortgage",
             "Rent it",
             "live rent-free",
             "Other",
             "Other"))

# Select only required variables
output_vars <- c("NSID",
                 "hown14", "hown15", "hown16", "hown17", "hown18", "hown19", "hown20",
                 "hown25", "hown32",
                 "hownteen14", "hownteen15", "hownteen16", "hownteen17", "hownteen18",
                 "hownteen19", "hownteen20")

cleaned_data <- cleaned_data %>% select(all_of(output_vars))

# Write output
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")

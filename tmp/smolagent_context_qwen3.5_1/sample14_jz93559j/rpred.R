library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets from data/input/
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID using full_join
merged_data <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w3, by = "NSID") %>%
  full_join(w4, by = "NSID") %>%
  full_join(w5, by = "NSID") %>%
  full_join(w6, by = "NSID") %>%
  full_join(w7, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9, by = "NSID")

# Function to recode missing values for waves 1-4
recode_missing_w1w4 <- function(x) {
  case_when(
    x %in% c(-999, -99, -995) ~ -3,  # Not asked
    x == -997 ~ -2,  # Script error
    x == -998 ~ -2,  # Interviewer missed question
    x == -92 ~ -9,  # Refused
    x == -91 ~ -1,  # Not applicable
    x == -1 ~ -8,  # Don't know
    TRUE ~ as.numeric(x)
  )
}

# Function to recode missing values for waves 5-7
recode_missing_w5w7 <- function(x) {
  case_when(
    x == -999 ~ -3,  # Not asked
    x == -92 ~ -9,  # Refused
    x == -91 ~ -1,  # Not applicable
    x == -1 ~ -8,  # Don't know
    TRUE ~ as.numeric(x)
  )
}

# Function to recode missing values for wave 8
recode_missing_w8 <- function(x) {
  case_when(
    x == -9 ~ -9,  # Refused
    x == -8 ~ -8,  # Don't know
    x == -1 ~ -1,  # Not applicable
    TRUE ~ as.numeric(x)
  )
}

# Function to recode missing values for wave 9
recode_missing_w9 <- function(x) {
  case_when(
    x == -8 ~ -8,  # Insufficient information
    x == -1 ~ -1,  # Not applicable
    TRUE ~ as.numeric(x)
  )
}

# Create detailed adolescent variables (hownteen14-20)
# Ages 14-17: Direct mapping with recoded missing values
merged_data <- merged_data %>%
  mutate(
    hownteen14 = recode_missing_w1w4(W1hous12HH),
    hownteen15 = recode_missing_w1w4(W2Hous12HH),
    hownteen16 = recode_missing_w1w4(W3hous12HH),
    hownteen17 = recode_missing_w1w4(W4Hous12HH)
  )

# Ages 18-20: Combine type and sub-type variables
# For age 18 (wave 5)
merged_data <- merged_data %>%
  mutate(
    hownteen18 = case_when(
      W5Hous12HH %in% c(-999, -92, -91, -1) ~ recode_missing_w5w7(W5Hous12HH),
      W5Hous12HH == 1 & W5Hous12BHH %in% c(-999, -92, -91, -1) ~ recode_missing_w5w7(W5Hous12BHH),
      W5Hous12HH == 1 & W5Hous12BHH == 1 ~ 1,  # Owned outright
      W5Hous12HH == 1 & W5Hous12BHH == 2 ~ 2,  # Mortgage/loan
      W5Hous12HH == 1 & W5Hous12BHH == 3 ~ 3,  # Shared ownership
      W5Hous12HH == 1 & W5Hous12BHH == 4 ~ 8,  # Other owned
      W5Hous12HH == 2 & W5Hous12CHH %in% c(-999, -92, -91, -1) ~ recode_missing_w5w7(W5Hous12CHH),
      W5Hous12HH == 2 & W5Hous12CHH == 1 ~ 4,  # Council/New Town
      W5Hous12HH == 2 & W5Hous12CHH == 2 ~ 5,  # Housing Association
      W5Hous12HH == 2 & W5Hous12CHH == 3 ~ 6,  # Private
      W5Hous12HH == 2 & W5Hous12CHH == 4 ~ 7,  # Rent free
      W5Hous12HH == 2 & W5Hous12CHH == 5 ~ 8,  # Other rented
      W5Hous12HH == 3 ~ 8,  # Something else
      W5Hous12HH == 6 ~ -3,  # Not to be asked
      TRUE ~ as.numeric(NA)
    )
  )

# For age 19 (wave 6)
merged_data <- merged_data %>%
  mutate(
    hownteen19 = case_when(
      W6Hous12YP %in% c(-92, -91, -1) ~ recode_missing_w5w7(W6Hous12YP),
      W6Hous12YP == 1 & W6Hous12bYP %in% c(-92, -91, -1) ~ recode_missing_w5w7(W6Hous12bYP),
      W6Hous12YP == 1 & W6Hous12bYP == 1 ~ 1,  # Owned outright
      W6Hous12YP == 1 & W6Hous12bYP == 2 ~ 2,  # Mortgage/loan
      W6Hous12YP == 1 & W6Hous12bYP == 3 ~ 3,  # Shared ownership
      W6Hous12YP == 1 & W6Hous12bYP == 4 ~ 8,  # Other owned
      W6Hous12YP == 2 & W6Hous12cYP %in% c(-92, -91, -1) ~ recode_missing_w5w7(W6Hous12cYP),
      W6Hous12YP == 2 & W6Hous12cYP == 1 ~ 4,  # Council/New Town
      W6Hous12YP == 2 & W6Hous12cYP == 2 ~ 5,  # Housing Association
      W6Hous12YP == 2 & W6Hous12cYP == 3 ~ 6,  # Private
      W6Hous12YP == 2 & W6Hous12cYP == 4 ~ 7,  # Rent free
      W6Hous12YP == 2 & W6Hous12cYP == 5 ~ 8,  # Other rented
      W6Hous12YP == 3 ~ 8,  # Something else
      TRUE ~ as.numeric(NA)
    )
  )

# For age 20 (wave 7)
merged_data <- merged_data %>%
  mutate(
    hownteen20 = case_when(
      W7Hous12YP %in% c(-92, -91, -1) ~ recode_missing_w5w7(W7Hous12YP),
      W7Hous12YP == 1 & W7Hous12bYP %in% c(-92, -91, -1) ~ recode_missing_w5w7(W7Hous12bYP),
      W7Hous12YP == 1 & W7Hous12bYP == 1 ~ 1,  # Owned outright
      W7Hous12YP == 1 & W7Hous12bYP == 2 ~ 2,  # Mortgage/loan
      W7Hous12YP == 1 & W7Hous12bYP == 3 ~ 3,  # Shared ownership
      W7Hous12YP == 1 & W7Hous12bYP == 4 ~ 8,  # Other owned
      W7Hous12YP == 2 & W7Hous12cYP %in% c(-92, -91, -1) ~ recode_missing_w5w7(W7Hous12cYP),
      W7Hous12YP == 2 & W7Hous12cYP == 1 ~ 4,  # Council/New Town
      W7Hous12YP == 2 & W7Hous12cYP == 2 ~ 5,  # Housing Association
      W7Hous12YP == 2 & W7Hous12cYP == 3 ~ 6,  # Private
      W7Hous12YP == 2 & W7Hous12cYP == 4 ~ 7,  # Rent free
      W7Hous12YP == 2 & W7Hous12cYP == 5 ~ 8,  # Other rented
      W7Hous12YP == 3 ~ 8,  # Something else
      TRUE ~ as.numeric(NA)
    )
  )

# Create collapsed variables for ages 14-20
# Map detailed 8-category to collapsed 6-category
# 1->1, 2->2, 3->3, 4-6->4, 7->5, 8->6
# Missing codes stay the same
collapse_teen <- function(x) {
  case_when(
    x %in% c(-9, -8, -3, -2, -1) ~ x,  # Keep missing codes
    x %in% c(1, 2, 3) ~ x,  # Owned outright, mortgage, shared
    x %in% c(4, 5, 6) ~ 4,  # All rented -> "Rent it"
    x == 7 ~ 5,  # Rent free
    x == 8 ~ 6,  # Other
    TRUE ~ as.numeric(NA)
  )
}

merged_data <- merged_data %>%
  mutate(
    hown14 = collapse_teen(hownteen14),
    hown15 = collapse_teen(hownteen15),
    hown16 = collapse_teen(hownteen16),
    hown17 = collapse_teen(hownteen17),
    hown18 = collapse_teen(hownteen18),
    hown19 = collapse_teen(hownteen19),
    hown20 = collapse_teen(hownteen20)
  )

# Create hown25 from W8TENURE (age 25)
# Map to standard missing codes and keep categories 1-7
merged_data <- merged_data %>%
  mutate(
    hown25 = recode_missing_w8(W8TENURE)
  )

# Create hown32 from W9DTENURE (age 32)
# Map to standard missing codes and keep categories 1-7
merged_data <- merged_data %>%
  mutate(
    hown32 = recode_missing_w9(W9DTENURE)
  )

# Define labels for detailed adolescent variables (8 categories + missing)
# Use numeric keys for labels
detailed_labels <- c(
  "1" = "Owned outright",
  "2" = "Being bought on a mortgage/bank loan",
  "3" = "Shared ownership (owns & rents property)",
  "4" = "Rented from a Council or New Town",
  "5" = "Rented from a Housing Association",
  "6" = "Rented privately",
  "7" = "Rent free",
  "8" = "Some other arrangement",
  "-1" = "Item not applicable",
  "-2" = "Script error/information lost",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-8" = "Don't know/insufficient information",
  "-9" = "Refusal"
)

# Define labels for collapsed variables (6 categories for teens, 7 for adults + missing)
collapsed_teen_labels <- c(
  "1" = "Owned outright",
  "2" = "Owned, buying with help of mortgage/loan",
  "3" = "Part rent, part mortgage",
  "4" = "Rent it",
  "5" = "live rent-free",
  "6" = "Other",
  "-1" = "Item not applicable",
  "-2" = "Script error/information lost",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-8" = "Don't know/insufficient information",
  "-9" = "Refusal"
)

collapsed_adult_labels <- c(
  "1" = "Owned outright",
  "2" = "Owned, buying with help of mortgage/loan",
  "3" = "Part rent, part mortgage (shared equity)",
  "4" = "Rent it",
  "5" = "Live rent-free, incl. relatives/friends",
  "6" = "Squatting",
  "7" = "Other",
  "-1" = "Item not applicable",
  "-2" = "Script error/information lost",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-8" = "Don't know/insufficient information",
  "-9" = "Refusal"
)

# Apply labels to detailed adolescent variables using set_label_values
merged_data <- merged_data %>%
  mutate(
    hownteen14 = labelled(hownteen14, labels = setNames(as.numeric(names(detailed_labels)), detailed_labels)),
    hownteen15 = labelled(hownteen15, labels = setNames(as.numeric(names(detailed_labels)), detailed_labels)),
    hownteen16 = labelled(hownteen16, labels = setNames(as.numeric(names(detailed_labels)), detailed_labels)),
    hownteen17 = labelled(hownteen17, labels = setNames(as.numeric(names(detailed_labels)), detailed_labels)),
    hownteen18 = labelled(hownteen18, labels = setNames(as.numeric(names(detailed_labels)), detailed_labels)),
    hownteen19 = labelled(hownteen19, labels = setNames(as.numeric(names(detailed_labels)), detailed_labels)),
    hownteen20 = labelled(hownteen20, labels = setNames(as.numeric(names(detailed_labels)), detailed_labels))
  )

# Apply labels to collapsed variables
merged_data <- merged_data %>%
  mutate(
    hown14 = labelled(hown14, labels = setNames(as.numeric(names(collapsed_teen_labels)), collapsed_teen_labels)),
    hown15 = labelled(hown15, labels = setNames(as.numeric(names(collapsed_teen_labels)), collapsed_teen_labels)),
    hown16 = labelled(hown16, labels = setNames(as.numeric(names(collapsed_teen_labels)), collapsed_teen_labels)),
    hown17 = labelled(hown17, labels = setNames(as.numeric(names(collapsed_teen_labels)), collapsed_teen_labels)),
    hown18 = labelled(hown18, labels = setNames(as.numeric(names(collapsed_teen_labels)), collapsed_teen_labels)),
    hown19 = labelled(hown19, labels = setNames(as.numeric(names(collapsed_teen_labels)), collapsed_teen_labels)),
    hown20 = labelled(hown20, labels = setNames(as.numeric(names(collapsed_teen_labels)), collapsed_teen_labels)),
    hown25 = labelled(hown25, labels = setNames(as.numeric(names(collapsed_adult_labels)), collapsed_adult_labels)),
    hown32 = labelled(hown32, labels = setNames(as.numeric(names(collapsed_adult_labels)), collapsed_adult_labels))
  )

# Select only required variables for output
cleaned_data <- merged_data %>%
  select(NSID, hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32,
         hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20)

# Write to CSV
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)

# Print summary
cat("Data cleaning complete.\n")
cat("Number of rows:", nrow(cleaned_data), "\n")
cat("Number of columns:", ncol(cleaned_data), "\n")
cat("Variables:", paste(names(cleaned_data), collapse = ", "), "\n")
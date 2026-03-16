# Load required packages
library(haven)
library(dplyr)
library(readr)

# Step 1: Load datasets
files <- list(
  wave_one = "data/input/wave_one_lsype_family_background_2020.tab",
  wave_two = "data/input/wave_two_lsype_family_background_2020.tab",
  wave_three = "data/input/wave_three_lsype_family_background_2020.tab",
  wave_four = "data/input/wave_four_lsype_family_background_2020.tab",
  wave_five = "data/input/wave_five_lsype_family_background_2020.tab",
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "data/input/wave_seven_lsype_young_person_2020.tab",
  ns8_2015 = "data/input/ns8_2015_main_interview.tab",
  ns9_2022 = "data/input/ns9_2022_derived_variables.tab"
)

# Load each dataset
wave_one_data <- readr::read_delim(files$wave_one, delim = "\t")
wave_two_data <- readr::read_delim(files$wave_two, delim = "\t")
wave_three_data <- readr::read_delim(files$wave_three, delim = "\t")
wave_four_data <- readr::read_delim(files$wave_four, delim = "\t")
wave_five_data <- readr::read_delim(files$wave_five, delim = "\t")
wave_six_data <- readr::read_delim(files$wave_six, delim = "\t")
wave_seven_data <- readr::read_delim(files$wave_seven, delim = "\t")
ns8_data <- readr::read_delim(files$ns8_2015, delim = "\t")
ns9_data <- readr::read_delim(files$ns9_2022, delim = "\t")

# Step 2: Merge datasets by NSID
merged_data <- 
  wave_one_data %>% select(NSID, W1hous12HH) %>% rename(hownteen14 = W1hous12HH) %>% 
  full_join(wave_two_data %>% select(NSID, W2Hous12HH) %>% rename(hownteen15 = W2Hous12HH), by = "NSID") %>%
  full_join(wave_three_data %>% select(NSID, W3hous12HH) %>% rename(hownteen16 = W3hous12HH), by = "NSID") %>%
  full_join(wave_four_data %>% select(NSID, W4Hous12HH) %>% rename(hownteen17 = W4Hous12HH), by = "NSID")

# Handle wave five data
wave_five_cols <- c("NSID", "W5Hous12BHH", "W5Hous12CHH")
wave_five_data <- wave_five_data %>% select(all_of(wave_five_cols))
merged_data <- merged_data %>% full_join(wave_five_data, by = "NSID")

# Handle wave six and seven data
merged_data <- merged_data %>% 
  full_join(wave_six_data %>% select(NSID, W6Hous12YP, W6Hous12bYP, W6Hous12cYP) %>% 
             mutate(hownteen19 = case_when(W6Hous12YP == 1 ~ W6Hous12bYP, W6Hous12YP == 2 ~ W6Hous12cYP + 3, TRUE ~ NA_real_)), by = "NSID") %>%
  full_join(wave_seven_data %>% select(NSID, W7Hous12YP, W7Hous12bYP, W7Hous12cYP) %>% 
             mutate(hownteen20 = case_when(W7Hous12YP == 1 ~ W7Hous12bYP, W7Hous12YP == 2 ~ W7Hous12cYP + 3, TRUE ~ NA_real_)), by = "NSID")

# Join adult data
merged_data <- merged_data %>% 
  full_join(ns8_data %>% select(NSID, W8TENURE) %>% rename(hown25 = W8TENURE), by = "NSID") %>%
  full_join(ns9_data %>% select(NSID, W9DTENURE) %>% rename(hown32 = W9DTENURE), by = "NSID")

# Step 3: Define mapping function for missing values
map_missing_values <- function(x) {
  x <- ifelse(is.na(x) | x %in% c(-999, -998, -997, -995, -99), -3, x)
  x <- ifelse(x == -92, -9, x)  # Refused
  x <- ifelse(x == -91, -1, x)  # Not applicable
  x <- ifelse(x == -1, -8, x)   # Don't know
  x <- ifelse(x == -9, -9, x)   # Refused
  x <- ifelse(x == -8, -8, x)   # Don't know
  return(x)
}

# Step 4: Map missing values for all tenure variables
teen_vars <- c("hownteen14", "hownteen15", "hownteen16", "hownteen17", "hownteen19", "hownteen20")
adult_vars <- c("hown25", "hown32")

for (var in c(teen_vars, adult_vars)) {
  if (var %in% colnames(merged_data)) {
    merged_data[[var]] <- map_missing_values(merged_data[[var]])
  }
}

# Step 5: Create harmonized variable for ages 14-20
merged_data <- merged_data %>% 
  mutate(hown14_20 = coalesce(
    hownteen20, hownteen19, hownteen17, hownteen16, hownteen15, hownteen14
  ))

# Step 6: Select only required variables
required_vars <- c("NSID", "hown14_20", "hown25", "hown32", teen_vars)
cleaned_data <- merged_data %>% select(all_of(required_vars))

# Step 7: Write output
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)
message("Data cleaning completed successfully!")
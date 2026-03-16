library(haven)
library(dplyr)
library(purrr)
library(readr)

# Define file paths
files <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_five = "data/input/wave_five_lsype_young_person_2020.tab",
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave_eight = "data/input/ns8_2015_derived.tab",
  wave_nine = "data/input/ns9_2022_main_interview.tab"
)

# Load all datasets
wave_one_data <- readr::read_delim(files$wave_one, delim = "\t")
wave_four_data <- readr::read_delim(files$wave_four, delim = "\t")
wave_five_data <- readr::read_delim(files$wave_five, delim = "\t")
wave_six_data <- readr::read_delim(files$wave_six, delim = "\t")
wave_seven_data <- readr::read_delim(files$wave_seven, delim = "\t")
wave_eight_data <- readr::read_delim(files$wave_eight, delim = "\t")
wave_nine_data <- readr::read_delim(files$wave_nine, delim = "\t")

# Merge all datasets by NSID
merged_data <- wave_one_data %>%
  full_join(wave_four_data, by = "NSID") %>%
  full_join(wave_five_data, by = "NSID") %>%
  full_join(wave_six_data, by = "NSID") %>%
  full_join(wave_seven_data, by = "NSID") %>%
  full_join(wave_eight_data, by = "NSID") %>%
  full_join(wave_nine_data, by = "NSID")

# Define missing value mapping
missing_mapping <- list(
  wave_four = list("-99.0" = -3, "-91.0" = -1, "-999.0" = -3),
  wave_five = list("-91.0" = -1, "-999.0" = -3),
  wave_six = list("-91.0" = -1, "-999.0" = -3),
  wave_seven = list("-91.0" = -1, "-999.0" = -3),
  wave_eight = list("-9.0" = -9, "-8.0" = -8, "-1.0" = -1),
  wave_nine = list("-9.0" = -9, "-8.0" = -8, "-7.0" = -7, "-3.0" = -3, "-2.0" = -2, "-1.0" = -1)
)

# Function to process NS-SEC variables
process_nssec <- function(data, wave, variable_name, age) {
  new_name <- paste0("nssec", age)

  # Map missing values
  data <- data %>%
    mutate(!!sym(variable_name) := recode(!!sym(variable_name), !!!missing_mapping[[wave]]))

  # Collapse fractional codes to integer categories
  data <- data %>%
    mutate(!!new_name := floor(!!sym(variable_name)))

  # Recode NA to -3
  data <- data %>%
    mutate(!!new_name := ifelse(is.na(!!sym(new_name)), -3, !!sym(new_name)))

  # Filter to keep only valid categories (1-17) and missing codes
  data <- data %>%
    mutate(!!new_name := ifelse(!!sym(new_name) >= 1 & !!sym(new_name) <= 17, !!sym(new_name), -3))

  return(data)
}

# Process each NS-SEC variable
nssec17 <- process_nssec(merged_data, "wave_four", "W4nsseccatYP", 17)
nssec18 <- process_nssec(merged_data, "wave_five", "W5nsseccatYP", 18)
nssec19 <- process_nssec(merged_data, "wave_six", "w6nsseccatYP", 19)
nssec20 <- process_nssec(merged_data, "wave_seven", "W7NSSECCat", 20)

# Process wave 8 NS-SEC (age 25) separately to handle the full-time student derivation
nssec25_data <- merged_data %>%
  mutate(W8DNSSEC17_processed = recode(W8DNSSEC17, !!!missing_mapping$wave_eight)) %>%
  mutate(nssec25 = floor(W8DNSSEC17_processed)) %>%
  mutate(nssec25 = ifelse(is.na(nssec25), -3, nssec25)) %>%
  mutate(nssec25 = ifelse(nssec25 >= 1 & nssec25 <= 17, nssec25, -3))

# Derive full-time student category (15) for age 25 if economic activity is education
merged_data <- merged_data %>%
  mutate(nssec25_derived = ifelse(
    is.na(nssec25_data$nssec25) & W8DACTIVITYC == 5, 15,
    ifelse(is.na(nssec25_data$nssec25), -3, nssec25_data$nssec25)
  ))

# Process wave 9 NS-SEC (age 32)
nssec32 <- process_nssec(merged_data, "wave_nine", "W9NSSEC", 32)

# Merge all processed NS-SEC variables with NSID
final_data <- merged_data %>%
  select(NSID, nssec25_derived) %>%
  left_join(nssec17 %>% select(NSID, nssec17), by = "NSID") %>%
  left_join(nssec18 %>% select(NSID, nssec18), by = "NSID") %>%
  left_join(nssec19 %>% select(NSID, nssec19), by = "NSID") %>%
  left_join(nssec20 %>% select(NSID, nssec20), by = "NSID") %>%
  left_join(nssec32 %>% select(NSID, nssec32), by = "NSID") %>%
  rename(nssec25 = nssec25_derived) %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write output
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)

# Print success message
cat('Data cleaning and harmonization completed successfully. Output file: data/output/cleaned_data.csv')
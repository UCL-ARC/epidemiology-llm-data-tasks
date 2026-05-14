library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
library(stringr)

# Load all datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave_three <- readr::read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
ns8_self_completion <- readr::read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
ns9_main_interview <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Create a function to process each dataset
data_processing <- function(df, age, ever_var, mon_var = NULL) {
  if (!is.null(mon_var)) {
    # Special case for age 14
    df <- df %>% mutate(drinker = ifelse(!!sym(ever_var) == 1 & !!sym(mon_var) == 1, age, NA))
  } else if (age %in% c(25, 32)) {
    # AUDIT variables
    df <- df %>% mutate(drinker = ifelse(!!sym(ever_var) > 1, age, NA))
  } else {
    # Regular ever had alcohol
    df <- df %>% mutate(drinker = ifelse(!!sym(ever_var) == 1, age, NA))
  }
  return(df)
}

# Process each dataset
wave_one <- data_processing(wave_one, 14, "W1alceverYP", "W1alcmonYP")
wave_two <- data_processing(wave_two, 15, "W2alceverYP")
wave_three <- data_processing(wave_three, 16, "W3alceverYP")
wave_four <- data_processing(wave_four, 17, "W4AlcEverYP")
wave_six <- data_processing(wave_six, 19, "W6AlcEverYP")
wave_seven <- data_processing(wave_seven, 20, "W7AlcEverYP")
ns8_self_completion <- data_processing(ns8_self_completion, 25, "W8AUDIT1")
ns9_main_interview <- data_processing(ns9_main_interview, 32, "W9AUDIT1")

# Merge all datasets
merged_data <- reduce(list(wave_one, wave_two, wave_three, wave_four, wave_six, wave_seven, ns8_self_completion, ns9_main_interview), 
                       full_join, by = "NSID")

# Calculate first age of drinking
merged_data <- merged_data %>% 
  mutate(alcfst = pmin(drinker, na.rm = TRUE))

# Determine never drinker status
never_drinker <- merged_data %>% 
  mutate(across(c(W1alceverYP, W2alceverYP, W3alceverYP, W4AlcEverYP, W6AlcEverYP, W7AlcEverYP, W8AUDIT1, W9AUDIT1), 
                ~ ifelse(is.na(.) | . == 2 | (. > 1 & . != 1), 1, 0), .names = "{.col}_never")) %>% 
  mutate(never_drinker = ifelse(all(c_across(ends_with("_never")) == 1), 1, 0))

# Final alcfst assignment
merged_data <- merged_data %>% 
  mutate(alcfst = case_when(
    !is.na(alcfst) ~ alcfst,
    never_drinker == 1 ~ 99,
    TRUE ~ -8
  ))

# Convert to factor
merged_data <- merged_data %>% 
  mutate(alcfst = factor(alcfst, levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
                          labels = c("14", "15", "16", "17", "19", "20", "25", "32", "Never had alcohol", "Don't know/insufficient information")))

# Output
write.csv(merged_data %>% select(NSID, alcfst), "data/output/cleaned_data.csv", row.names = FALSE)
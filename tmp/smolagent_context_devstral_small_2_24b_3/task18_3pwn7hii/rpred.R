library(dplyr)
library(readr)
library(haven)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8_sc <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8_sc, by = "NSID") %>%
  full_join(wave8_derived, by = "NSID") %>%
  full_join(wave9_main, by = "NSID") %>%
  full_join(wave9_derived, by = "NSID")

# Function to compute item-summed Likert scores
compute_ghqtl <- function(data, item_vars, age_suffix) {
  # Check for all NA
  all_na <- rowSums(!is.na(data[, item_vars])) == 0
  # Check for any negative values
  any_neg <- rowSums(data[, item_vars] < 0, na.rm = TRUE) > 0
  # Compute sum
  item_sum <- rowSums(data[, item_vars], na.rm = TRUE)
  # Apply rules
  result <- ifelse(all_na, -3, ifelse(any_neg, -8, item_sum))
  return(result)
}

# Define item variables for each wave
wave2_items <- c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP",
                 "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")
wave4_items <- c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP",
                 "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP")
wave8_items <- paste0("W8GHQ12_", 1:12)
wave9_items <- paste0("W9GHQ12_", 1:12)

# Compute item-summed Likert scores
merged_data <- merged_data %>%
  mutate(ghqtl15 = compute_ghqtl(., wave2_items, 15)) %>%
  mutate(ghqtl17 = compute_ghqtl(., wave4_items, 17)) %>%
  mutate(ghqtl25 = compute_ghqtl(., wave8_items, 25)) %>%
  mutate(ghqtl32 = compute_ghqtl(., wave9_items, 32))

# Function to harmonize pre-derived caseness scores
harmonize_ghq <- function(var, wave) {
  if (wave == 2 | wave == 4) {
    # For waves 2 and 4, map -97 and -92 to -9
    var <- ifelse(var == -97 | var == -92, -9, var)
  }
  # Convert NA to -3
  var <- ifelse(is.na(var), -3, var)
  return(var)
}

# Harmonize pre-derived caseness scores
merged_data <- merged_data %>%
  mutate(ghq15 = harmonize_ghq(W2ghq12scr, 2)) %>%
  mutate(ghq17 = harmonize_ghq(W4ghq12scr, 4)) %>%
  mutate(ghq25 = harmonize_ghq(W8DGHQSC, 8)) %>%
  mutate(ghq32 = harmonize_ghq(W9DGHQSC, 9))

# Select only NSID and the 8 derived variables
output_data <- merged_data %>%
  select(NSID, ghqtl15, ghq15, ghqtl17, ghq17, ghqtl25, ghq25, ghqtl32, ghq32)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")
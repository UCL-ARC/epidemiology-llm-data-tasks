library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8_sc <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using NSID
merged_data <- wave2 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8_sc, by = "NSID") %>%
  full_join(wave8_derived, by = "NSID") %>%
  full_join(wave9_main, by = "NSID") %>%
  full_join(wave9_derived, by = "NSID")

# Define item-summed Likert score function
compute_ghqtl <- function(data, items) {
  # Check if all items are NA
  all_na <- apply(data[, items], 1, function(x) all(is.na(x)))
  
  # Check if any item has a negative value
  any_negative <- apply(data[, items], 1, function(x) any(x < 0, na.rm = TRUE))
  
  # Compute sum
  ghqtl <- rowSums(data[, items], na.rm = TRUE)
  
  # Apply rules
  ghqtl[all_na] <- -3
  ghqtl[any_negative] <- -8
  
  return(ghqtl)
}

# Compute item-summed Likert scores
ghqtl15_items <- c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP", 
                   "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")
merged_data$ghqtl15 <- compute_ghqtl(merged_data, ghqtl15_items)

ghqtl17_items <- c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP",
                   "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP")
merged_data$ghqtl17 <- compute_ghqtl(merged_data, ghqtl17_items)

ghqtl25_items <- c("W8GHQ12_1", "W8GHQ12_2", "W8GHQ12_3", "W8GHQ12_4", "W8GHQ12_5", "W8GHQ12_6",
                   "W8GHQ12_7", "W8GHQ12_8", "W8GHQ12_9", "W8GHQ12_10", "W8GHQ12_11", "W8GHQ12_12")
merged_data$ghqtl25 <- compute_ghqtl(merged_data, ghqtl25_items)

ghqtl32_items <- c("W9GHQ12_1", "W9GHQ12_2", "W9GHQ12_3", "W9GHQ12_4", "W9GHQ12_5", "W9GHQ12_6",
                   "W9GHQ12_7", "W9GHQ12_8", "W9GHQ12_9", "W9GHQ12_10", "W9GHQ12_11", "W9GHQ12_12")
merged_data$ghqtl32 <- compute_ghqtl(merged_data, ghqtl32_items)

# Harmonize pre-derived caseness scores
harmonize_ghq <- function(ghq_var) {
  # Convert to numeric, handling non-numeric values
  ghq_var <- as.numeric(ghq_var)
  
  # Map wave-specific negative codes to standard missing-value scheme
  ghq_var[ghq_var == -97] <- -9
  ghq_var[ghq_var == -92] <- -9
  ghq_var[ghq_var == -99] <- -3
  ghq_var[ghq_var == -96] <- -2
  ghq_var[ghq_var == -95] <- -2
  ghq_var[ghq_var == -94] <- -8
  ghq_var[ghq_var == -91] <- -1
  ghq_var[ghq_var == -9] <- -9
  ghq_var[ghq_var == -8] <- -8
  ghq_var[ghq_var == -3] <- -3
  ghq_var[ghq_var == -2] <- -2
  ghq_var[ghq_var == -1] <- -1
  
  return(ghq_var)
}

merged_data$ghq15 <- harmonize_ghq(merged_data$W2ghq12scr)
merged_data$ghq17 <- harmonize_ghq(merged_data$W4ghq12scr)
merged_data$ghq25 <- harmonize_ghq(merged_data$W8DGHQSC)
merged_data$ghq32 <- harmonize_ghq(merged_data$W9DGHQSC)

# Select final variables
final_data <- merged_data %>%
  select(NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")

print("Cleaned data written to data/output/cleaned_data.csv")
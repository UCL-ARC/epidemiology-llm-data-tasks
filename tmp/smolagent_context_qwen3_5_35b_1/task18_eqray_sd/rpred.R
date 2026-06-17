# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from data/input/
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8_self <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", show_col_types = FALSE)
wave8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID using full_join
data <- full_join(wave2, wave4, by = "NSID")
data <- full_join(data, wave8_self, by = "NSID")
data <- full_join(data, wave8_derived, by = "NSID")
data <- full_join(data, wave9_main, by = "NSID")
data <- full_join(data, wave9_derived, by = "NSID")

# Compute item-summed GHQ scores for wave 2 (age 15)
items_w2 <- as.matrix(data[, c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")])
all_na_w2 <- rowSums(is.na(items_w2)) == 12
any_neg_w2 <- rowSums(items_w2 < 0, na.rm = TRUE) > 0
sum_valid_w2 <- rowSums(items_w2, na.rm = TRUE)
data$ghqtl15 <- ifelse(all_na_w2, -3, ifelse(any_neg_w2, -8, sum_valid_w2))

# Compute item-summed GHQ scores for wave 4 (age 17)
items_w4 <- as.matrix(data[, c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP")])
all_na_w4 <- rowSums(is.na(items_w4)) == 12
any_neg_w4 <- rowSums(items_w4 < 0, na.rm = TRUE) > 0
sum_valid_w4 <- rowSums(items_w4, na.rm = TRUE)
data$ghqtl17 <- ifelse(all_na_w4, -3, ifelse(any_neg_w4, -8, sum_valid_w4))

# Compute item-summed GHQ scores for wave 8 (age 25)
items_w8 <- as.matrix(data[, paste0("W8GHQ12_", 1:12)])
all_na_w8 <- rowSums(is.na(items_w8)) == 12
any_neg_w8 <- rowSums(items_w8 < 0, na.rm = TRUE) > 0
sum_valid_w8 <- rowSums(items_w8, na.rm = TRUE)
data$ghqtl25 <- ifelse(all_na_w8, -3, ifelse(any_neg_w8, -8, sum_valid_w8))

# Compute item-summed GHQ scores for wave 9 (age 32)
items_w9 <- as.matrix(data[, paste0("W9GHQ12_", 1:12)])
all_na_w9 <- rowSums(is.na(items_w9)) == 12
any_neg_w9 <- rowSums(items_w9 < 0, na.rm = TRUE) > 0
sum_valid_w9 <- rowSums(items_w9, na.rm = TRUE)
data$ghqtl32 <- ifelse(all_na_w9, -3, ifelse(any_neg_w9, -8, sum_valid_w9))

# Harmonize pre-derived GHQ scores
# Wave 2 (age 15): -97 and -92 map to -9, -99 to -3, -96 to -2, -91 to -1, -1 to -8
data$ghq15 <- case_when(
  data$W2ghq12scr %in% c(-97, -92) ~ -9,
  data$W2ghq12scr == -99 ~ -3,
  data$W2ghq12scr == -96 ~ -2,
  data$W2ghq12scr == -91 ~ -1,
  data$W2ghq12scr == -1 ~ -8,
  is.na(data$W2ghq12scr) ~ -3,
  TRUE ~ as.integer(data$W2ghq12scr)
)

# Wave 4 (age 17): -97 and -92 map to -9, -99 to -3, -96 to -2, -91 to -1, -1 to -8
data$ghq17 <- case_when(
  data$W4ghq12scr %in% c(-97, -92) ~ -9,
  data$W4ghq12scr == -99 ~ -3,
  data$W4ghq12scr == -96 ~ -2,
  data$W4ghq12scr == -91 ~ -1,
  data$W4ghq12scr == -1 ~ -8,
  is.na(data$W4ghq12scr) ~ -3,
  TRUE ~ as.integer(data$W4ghq12scr)
)

# Wave 8 (age 25): -9 to -9, -8 to -8, -1 to -1
data$ghq25 <- case_when(
  data$W8DGHQSC == -9 ~ -9,
  data$W8DGHQSC == -8 ~ -8,
  data$W8DGHQSC == -1 ~ -1,
  is.na(data$W8DGHQSC) ~ -3,
  TRUE ~ as.integer(data$W8DGHQSC)
)

# Wave 9 (age 32): -9 to -9, -8 to -8, -3 to -3, -1 to -1
data$ghq32 <- case_when(
  data$W9DGHQSC == -9 ~ -9,
  data$W9DGHQSC == -8 ~ -8,
  data$W9DGHQSC == -3 ~ -3,
  data$W9DGHQSC == -1 ~ -1,
  is.na(data$W9DGHQSC) ~ -3,
  TRUE ~ as.integer(data$W9DGHQSC)
)

# Select final variables
output <- data %>%
  select(NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32)

# Create output directory if needed
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Write output to CSV
write_csv(output, "data/output/cleaned_data.csv")

# Confirm completion
cat("Successfully created data/output/cleaned_data.csv\n")
cat("Variables: NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32\n")

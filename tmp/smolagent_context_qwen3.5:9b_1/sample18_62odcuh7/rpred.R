library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8_self_comp <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Function to compute GHQ-12 sum score
compute_ghqtl <- function(df, wave_num) {
  if (wave_num == 2) {
    ghq_cols <- c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP",
                  "W2difficYP", "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP",
                  "W2wthlessYP", "W2happyYP")
  } else if (wave_num == 4) {
    ghq_cols <- c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP",
                  "W4DifficYP", "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP",
                  "W4WthlessYP", "W4HappyYP")
  } else if (wave_num == 8) {
    ghq_cols <- paste0("W8GHQ12_", 1:12)
  } else if (wave_num == 9) {
    ghq_cols <- paste0("W9GHQ12_", 1:12)
  }
  ghq_items <- df[, ghq_cols]
  all_na <- all(is.na(ghq_items))
  any_neg <- any(sapply(ghq_items, function(x) any(x < 0)))
  if (all_na) return(-3)
  if (any_neg) return(-8)
  return(rowSums(sapply(ghq_items, function(x) ifelse(x < 0, NA, x)), na.rm = TRUE))
}

# Process wave 2 (age 15)
wave2_processed <- wave2 %>%
  select(NSID, W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, W2strainYP,
         W2difficYP, W2activYP, W2probsYP, W2depressYP, W2noconfYP, W2wthlessYP,
         W2happyYP, W2ghq12scr) %>%
  rename(ghq15 = W2ghq12scr) %>%
  mutate(ghqtl15 = compute_ghqtl(., wave_num = 2)) %>%
  mutate(ghq15 = case_when(is.na(ghq15) ~ -3, ghq15 %in% c(-96, -99) ~ -3, ghq15 %in% c(-97, -92) ~ -9, TRUE ~ ghq15)) %>%
  mutate(ghq15 = if_else(ghq15 == -9, -9, ghq15)) %>%
  mutate(ghqtl15 = if_else(ghqtl15 == -9, -9, ghqtl15))

# Process wave 4 (age 17)
wave4_processed <- wave4 %>%
  select(NSID, W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, W4StrainYP,
         W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP, W4NoConfYP, W4WthlessYP,
         W4HappyYP, W4ghq12scr) %>%
  rename(ghq17 = W4ghq12scr) %>%
  mutate(ghqtl17 = compute_ghqtl(., wave_num = 4)) %>%
  mutate(ghq17 = case_when(is.na(ghq17) ~ -3, ghq17 %in% c(-96, -99) ~ -3, ghq17 %in% c(-97, -92) ~ -9, TRUE ~ ghq17)) %>%
  mutate(ghq17 = if_else(ghq17 == -9, -9, ghq17)) %>%
  mutate(ghqtl17 = if_else(ghqtl17 == -9, -9, ghqtl17))

# Process wave 8 (age 25)
wave8_items_processed <- select(wave8_self_comp, NSID, W8GHQ12_1:W8GHQ12_12)
wave8_derived_items <- select(wave8_derived, NSID, W8DGHQSC)
wave8_processed <- wave8_derived_items %>%
  full_join(wave8_items_processed, by = "NSID") %>%
  mutate(ghq25 = W8DGHQSC, ghqtl25 = compute_ghqtl(., wave_num = 8)) %>%
  mutate(ghq25 = case_when(is.na(ghq25) ~ -3, TRUE ~ ghq25)) %>%
  mutate(ghq25 = if_else(ghq25 == -9, -9, ghq25)) %>%
  mutate(ghqtl25 = if_else(ghqtl25 == -9, -9, ghqtl25))

# Process wave 9 (age 32)
wave9_items_processed <- select(wave9, NSID, W9GHQ12_1:W9GHQ12_12)
wave9_derived_items <- select(wave9_derived, NSID, W9DGHQSC)
wave9_processed <- wave9_derived_items %>%
  full_join(wave9_items_processed, by = "NSID") %>%
  mutate(ghq32 = W9DGHQSC, ghqtl32 = compute_ghqtl(., wave_num = 9)) %>%
  mutate(ghq32 = case_when(is.na(ghq32) ~ -3, TRUE ~ ghq32)) %>%
  mutate(ghq32 = if_else(ghq32 == -9, -9, ghq32)) %>%
  mutate(ghqtl32 = if_else(ghqtl32 == -9, -9, ghqtl32))

# Merge all datasets
final_data <- full_join(wave1, wave2_processed, by = "NSID")
final_data <- full_join(final_data, wave4_processed, by = "NSID")
final_data <- full_join(final_data, wave8_processed, by = "NSID")
final_data <- full_join(final_data, wave9_processed, by = "NSID")

# Convert to factors
final_data$ghq15 <- as.factor(final_data$ghq15)
final_data$ghq17 <- as.factor(final_data$ghq17)
final_data$ghq25 <- as.factor(final_data$ghq25)
final_data$ghq32 <- as.factor(final_data$ghq32)
final_data$ghqtl15 <- as.factor(final_data$ghqtl15)
final_data$ghqtl17 <- as.factor(final_data$ghqtl17)
final_data$ghqtl25 <- as.factor(final_data$ghqtl25)
final_data$ghqtl32 <- as.factor(final_data$ghqtl32)

# Create label values as character vector
label_vals <- c("-3" = "Not asked", "-8" = "Insufficient information", "-9" = "Refusal", "0" = "GHQ-12 score", "1" = "GHQ-12 score", "2" = "GHQ-12 score", "3" = "GHQ-12 score", "4" = "GHQ-12 score", "5" = "GHQ-12 score", "6" = "GHQ-12 score", "7" = "GHQ-12 score", "8" = "GHQ-12 score", "9" = "GHQ-12 score", "10" = "GHQ-12 score", "11" = "GHQ-12 score", "12" = "GHQ-12 score", "13" = "GHQ-12 score")

# Set factor levels to match labels
levels(final_data$ghq15) <- label_vals
levels(final_data$ghq17) <- label_vals
levels(final_data$ghq25) <- label_vals
levels(final_data$ghq32) <- label_vals
levels(final_data$ghqtl15) <- label_vals
levels(final_data$ghqtl17) <- label_vals
levels(final_data$ghqtl25) <- label_vals
levels(final_data$ghqtl32) <- label_vals

# Set variable labels
attr(final_data$ghq15, "label") <- "Pre-derived GHQ-12 score at age 15"
attr(final_data$ghq17, "label") <- "Pre-derived GHQ-12 score at age 17"
attr(final_data$ghq25, "label") <- "Pre-derived GHQ-12 score at age 25"
attr(final_data$ghq32, "label") <- "Pre-derived GHQ-12 score at age 32"
attr(final_data$ghqtl15, "label") <- "Manually computed GHQ-12 sum score at age 15"
attr(final_data$ghqtl17, "label") <- "Manually computed GHQ-12 sum score at age 17"
attr(final_data$ghqtl25, "label") <- "Manually computed GHQ-12 sum score at age 25"
attr(final_data$ghqtl32, "label") <- "Manually computed GHQ-12 sum score at age 32"

# Select only required variables
final_data <- select(final_data, NSID, ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")
print(paste("Output written to data/output/cleaned_data.csv with", nrow(final_data), "rows and", ncol(final_data), "columns"))
print("Variables:", names(final_data))
}
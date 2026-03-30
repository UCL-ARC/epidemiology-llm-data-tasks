library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define function to harmonize pre-derived scores for earlier sweeps (wave 2 and 4)
harmonize_early_sweep <- function(x) {
  x[x == -96 | x == -99] <- -3
  x[x == -97 | x == -92] <- -9
  x[is.na(x)] <- -3
  return(x)
}

# Define function to harmonize pre-derived scores for later sweeps (wave 8 and 9)
harmonize_later_sweep <- function(x) {
  x[is.na(x)] <- -3
  return(x)
}

# Define function to compute GHQ sum score from 12 items
compute_ghq_sum <- function(items) {
  # Check if ALL 12 items are NA
  all_na <- rowSums(is.na(items)) == 12
  
  # Check if ANY item contains a negative value (missing-value code)
  has_negative <- rowSums(items < 0, na.rm = TRUE) > 0
  
  result <- rep(NA, nrow(items))
  result[all_na] <- -3  # did not participate
  result[has_negative] <- -8  # insufficient information
  result[!all_na & !has_negative] <- rowSums(items, na.rm = TRUE)[!all_na & !has_negative]
  
  return(result)
}

# Load each file explicitly
# Wave 2 (Age 15)
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", 
                 delim = "\t", show_col_types = FALSE) %>%
  select(NSID, 
         W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, 
         W2strainYP, W2difficYP, W2activYP, W2probsYP, 
         W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP,
         W2ghq12scr)

# Wave 4 (Age 17)
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab",
                 delim = "\t", show_col_types = FALSE) %>%
  select(NSID,
         W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP,
         W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP,
         W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP,
         W4ghq12scr)

# Wave 8 (Age 25) - items from self-completion
w8_items <- read_delim("data/input/ns8_2015_self_completion.tab",
                       delim = "\t", show_col_types = FALSE) %>%
  select(NSID,
         W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4,
         W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8,
         W8GHQ12_9, W8GHQ12_10, W8GHQ12_11, W8GHQ12_12)

# Wave 8 (Age 25) - pre-derived score
w8_derived <- read_delim("data/input/ns8_2015_derived.tab",
                         delim = "\t", show_col_types = FALSE) %>%
  select(NSID, W8DGHQSC)

# Wave 9 (Age 32)
w9 <- read_delim("data/input/ns9_2022_main_interview.tab",
                 delim = "\t", show_col_types = FALSE) %>%
  select(NSID,
         W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4,
         W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8,
         W9GHQ12_9, W9GHQ12_10, W9GHQ12_11, W9GHQ12_12)

# Wave 9 (Age 32) - pre-derived score
w9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab",
                         delim = "\t", show_col_types = FALSE) %>%
  select(NSID, W9DGHQSC)

# Merge all datasets using full_join by NSID
df <- full_join(w2, w4, by = "NSID")
df <- full_join(df, w8_items, by = "NSID")
df <- full_join(df, w8_derived, by = "NSID")
df <- full_join(df, w9, by = "NSID")
df <- full_join(df, w9_derived, by = "NSID")

# Compute manual GHQ sum scores for each sweep
# Wave 2 (Age 15)
df$ghqtl15 <- compute_ghq_sum(df[, c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP",
                                     "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP",
                                     "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")])

# Wave 4 (Age 17)
df$ghqtl17 <- compute_ghq_sum(df[, c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP",
                                     "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP",
                                     "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP")])

# Wave 8 (Age 25)
df$ghqtl25 <- compute_ghq_sum(df[, c("W8GHQ12_1", "W8GHQ12_2", "W8GHQ12_3", "W8GHQ12_4",
                                     "W8GHQ12_5", "W8GHQ12_6", "W8GHQ12_7", "W8GHQ12_8",
                                     "W8GHQ12_9", "W8GHQ12_10", "W8GHQ12_11", "W8GHQ12_12")])

# Wave 9 (Age 32)
df$ghqtl32 <- compute_ghq_sum(df[, c("W9GHQ12_1", "W9GHQ12_2", "W9GHQ12_3", "W9GHQ12_4",
                                     "W9GHQ12_5", "W9GHQ12_6", "W9GHQ12_7", "W9GHQ12_8",
                                     "W9GHQ12_9", "W9GHQ12_10", "W9GHQ12_11", "W9GHQ12_12")])

# Harmonize pre-derived GHQ scores
# Wave 2 (Age 15) - earlier sweep
df$ghq15 <- harmonize_early_sweep(df$W2ghq12scr)

# Wave 4 (Age 17) - earlier sweep
df$ghq17 <- harmonize_early_sweep(df$W4ghq12scr)

# Wave 8 (Age 25) - later sweep
df$ghq25 <- harmonize_later_sweep(df$W8DGHQSC)

# Wave 9 (Age 32) - later sweep
df$ghq32 <- harmonize_later_sweep(df$W9DGHQSC)

# Create a named list for labels using numeric keys explicitly
code_labels <- c(
  "Item not applicable" = -1,
  "Script error/information lost" = -2,
  "Not asked at the fieldwork stage/participated/interviewed" = -3,
  "Don't know/insufficient information" = -8,
  "Refusal" = -9
)

# Apply missing value labels using labelled::set_value_labels with named list
apply_missing_labels <- function(x, labels) {
  labelled::set_value_labels(x, labels)
}

df$ghq15 <- apply_missing_labels(df$ghq15, code_labels)
df$ghq17 <- apply_missing_labels(df$ghq17, code_labels)
df$ghq25 <- apply_missing_labels(df$ghq25, code_labels)
df$ghq32 <- apply_missing_labels(df$ghq32, code_labels)
df$ghqtl15 <- apply_missing_labels(df$ghqtl15, code_labels)
df$ghqtl17 <- apply_missing_labels(df$ghqtl17, code_labels)
df$ghqtl25 <- apply_missing_labels(df$ghqtl25, code_labels)
df$ghqtl32 <- apply_missing_labels(df$ghqtl32, code_labels)

# Select only required columns: NSID, ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32
df_output <- df %>%
  select(NSID, ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32)

# Convert all derived GHQ variables (excluding NSID) to factors
df_output$ghq15 <- as.factor(df_output$ghq15)
df_output$ghq17 <- as.factor(df_output$ghq17)
df_output$ghq25 <- as.factor(df_output$ghq25)
df_output$ghq32 <- as.factor(df_output$ghq32)
df_output$ghqtl15 <- as.factor(df_output$ghqtl15)
df_output$ghqtl17 <- as.factor(df_output$ghqtl17)
df_output$ghqtl25 <- as.factor(df_output$ghqtl25)
df_output$ghqtl32 <- as.factor(df_output$ghqtl32)

# Write output to CSV
write_csv(df_output, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")

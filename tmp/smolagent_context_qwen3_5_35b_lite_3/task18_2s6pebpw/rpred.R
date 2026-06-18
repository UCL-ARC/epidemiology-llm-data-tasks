library(readr)
library(dplyr)
library(tidyr)
library(haven)
library(labelled)

# Load all files from metadata
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
ns8_self <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", show_col_types = FALSE)
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID using full_join
data <- full_join(wave1, wave2, by = "NSID")
data <- full_join(data, wave4, by = "NSID")
data <- full_join(data, ns8_self, by = "NSID")
data <- full_join(data, ns8_derived, by = "NSID")
data <- full_join(data, ns9_main, by = "NSID")
data <- full_join(data, ns9_derived, by = "NSID")

# Function to recode missing values to standard codes
recoded <- function(x) {
  case_when(
    x == -999 | x == -998 | x == -997 | x == -995 ~ -2,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -99 ~ -3,
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -3 ~ -3,
    x == -1 ~ -1,
    TRUE ~ as.numeric(x)
  )
}

# Recode Wave 2 (Age 15) GHQ items
items_w2 <- c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", 
              "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP",
              "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")

for (item in items_w2) {
  data[[paste0(item, "_rec")]] <- recoded(data[[item]])
}

# Create ghqtl15: sum of items only if all are non-negative
data$ghqtl15 <- rowSums(data[paste0(items_w2, "_rec")], na.rm = FALSE)
data$ghqtl15[data$ghqtl15 == -Inf] <- NA
data$ghqtl15[is.na(data$ghqtl15)] <- -3

# Recode pre-derived W2ghq12scr for caseness
ghq2_codes <- c(-99, -97, -96, -92)
data$ghq15 <- recoded(data$W2ghq12scr)
data$ghq15[data$ghq15 == -9] <- -9

# Recode Wave 4 (Age 17) GHQ items
items_w4 <- c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP",
              "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP",
              "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP")

for (item in items_w4) {
  data[[paste0(item, "_rec")]] <- recoded(data[[item]])
}

data$ghqtl17 <- rowSums(data[paste0(items_w4, "_rec")], na.rm = FALSE)
data$ghqtl17[data$ghqtl17 == -Inf] <- NA
data$ghqtl17[is.na(data$ghqtl17)] <- -3
data$ghq17 <- recoded(data$W4ghq12scr)

# Recode Wave 8 (Age 25) GHQ items
items_w8 <- c("W8GHQ12_1", "W8GHQ12_2", "W8GHQ12_3", "W8GHQ12_4",
              "W8GHQ12_5", "W8GHQ12_6", "W8GHQ12_7", "W8GHQ12_8",
              "W8GHQ12_9", "W8GHQ12_10", "W8GHQ12_11", "W8GHQ12_12")

for (item in items_w8) {
  data[[paste0(item, "_rec")]] <- recoded(data[[item]])
}

data$ghqtl25 <- rowSums(data[paste0(items_w8, "_rec")], na.rm = FALSE)
data$ghqtl25[data$ghqtl25 == -Inf] <- NA
data$ghqtl25[is.na(data$ghqtl25)] <- -3
data$ghq25 <- recoded(data$W8DGHQSC)

# Recode Wave 9 (Age 32) GHQ items
items_w9 <- c("W9GHQ12_1", "W9GHQ12_2", "W9GHQ12_3", "W9GHQ12_4",
              "W9GHQ12_5", "W9GHQ12_6", "W9GHQ12_7", "W9GHQ12_8",
              "W9GHQ12_9", "W9GHQ12_10", "W9GHQ12_11", "W9GHQ12_12")

for (item in items_w9) {
  data[[paste0(item, "_rec")]] <- recoded(data[[item]])
}

data$ghqtl32 <- rowSums(data[paste0(items_w9, "_rec")], na.rm = FALSE)
data$ghqtl32[data$ghqtl32 == -Inf] <- NA
data$ghqtl32[is.na(data$ghqtl32)] <- -3
data$ghq32 <- recoded(data$W9DGHQSC)

# Select only final derived variables and NSID
output <- data %>%
  select(NSID, ghqtl15, ghq15, ghqtl17, ghq17, ghqtl25, ghq25, ghqtl32, ghq32)

# Write output
colnames(output) <- c("NSID", "ghqtl15", "ghq15", "ghqtl17", "ghq17", "ghqtl25", "ghq25", "ghqtl32", "ghq32")
write_csv(output, "data/output/cleaned_data.csv")

# Verify output
print(head(output))
print(dim(output))
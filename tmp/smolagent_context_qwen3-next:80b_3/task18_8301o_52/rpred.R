library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load wave two data
df2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", col_select = c("NSID", "W2ghq12scr", "W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP"))

# Load wave four data
df4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_select = c("NSID", "W4ghq12scr", "W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP"))

# Load wave eight self-completion
df8_self <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", col_select = c("NSID", "W8GHQ12_1", "W8GHQ12_2", "W8GHQ12_3", "W8GHQ12_4", "W8GHQ12_5", "W8GHQ12_6", "W8GHQ12_7", "W8GHQ12_8", "W8GHQ12_9", "W8GHQ12_10", "W8GHQ12_11", "W8GHQ12_12"))

# Load wave eight derived
df8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_select = c("NSID", "W8DGHQSC"))

# Load wave nine main interview
df9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", col_select = c("NSID", "W9GHQ12_1", "W9GHQ12_2", "W9GHQ12_3", "W9GHQ12_4", "W9GHQ12_5", "W9GHQ12_6", "W9GHQ12_7", "W9GHQ12_8", "W9GHQ12_9", "W9GHQ12_10", "W9GHQ12_11", "W9GHQ12_12"))

# Load wave nine derived
df9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_select = c("NSID", "W9DGHQSC"))

# Merge all datasets
df_merged <- df2 %>%
  full_join(df4, by = "NSID") %>%
  full_join(df8_self, by = "NSID") %>%
  full_join(df8_derived, by = "NSID") %>%
  full_join(df9_main, by = "NSID") %>%
  full_join(df9_derived, by = "NSID")

# Process pre-derived scores
# Wave 2 (age 15)
df_merged <- df_merged %>%
  mutate(ghq15 = case_when(
    W2ghq12scr %in% c(-96, -99) ~ -3,
    W2ghq12scr %in% c(-97, -92) ~ -9,
    is.na(W2ghq12scr) ~ -3,
    TRUE ~ W2ghq12scr
  ))

# Wave 4 (age 17)
df_merged <- df_merged %>%
  mutate(ghq17 = case_when(
    W4ghq12scr %in% c(-96, -99) ~ -3,
    W4ghq12scr %in% c(-97, -92) ~ -9,
    is.na(W4ghq12scr) ~ -3,
    TRUE ~ W4ghq12scr
  ))

# Wave 8 (age 25)
df_merged <- df_merged %>%
  mutate(ghq25 = ifelse(is.na(W8DGHQSC), -3, W8DGHQSC))

# Wave 9 (age 32)
df_merged <- df_merged %>%
  mutate(ghq32 = ifelse(is.na(W9DGHQSC), -3, W9DGHQSC))

# Process sum scores
# Wave 2 items
items2 <- df_merged %>% select(W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, W2strainYP, W2difficYP, W2activYP, W2probsYP, W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP)
ghqtl15 <- apply(items2, 1, function(x) {
  x[is.na(x)] <- -3
  if (all(x == -3)) {
    -3
  } else if (any(x < 0)) {
    -8
  } else {
    sum(x)
  }
})
df_merged$ghqtl15 <- ghqtl15

# Wave 4 items
items4 <- df_merged %>% select(W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP)
ghqtl17 <- apply(items4, 1, function(x) {
  x[is.na(x)] <- -3
  if (all(x == -3)) {
    -3
  } else if (any(x < 0)) {
    -8
  } else {
    sum(x)
  }
})
df_merged$ghqtl17 <- ghqtl17

# Wave 8 items
items8 <- df_merged %>% select(W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4, W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8, W8GHQ12_9, W8GHQ12_10, W8GHQ12_11, W8GHQ12_12)
ghqtl25 <- apply(items8, 1, function(x) {
  x[is.na(x)] <- -3
  if (all(x == -3)) {
    -3
  } else if (any(x < 0)) {
    -8
  } else {
    sum(x)
  }
})
df_merged$ghqtl25 <- ghqtl25

# Wave 9 items
items9 <- df_merged %>% select(W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4, W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8, W9GHQ12_9, W9GHQ12_10, W9GHQ12_11, W9GHQ12_12)
ghqtl32 <- apply(items9, 1, function(x) {
  x[is.na(x)] <- -3
  if (all(x == -3)) {
    -3
  } else if (any(x < 0)) {
    -8
  } else {
    sum(x)
  }
})
df_merged$ghqtl32 <- ghqtl32

# Apply labels
df_merged$ghq15 <- labelled::labelled(df_merged$ghq15, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked" = -3, "Don't know" = -8, "Refusal" = -9))
df_merged$ghq17 <- labelled::labelled(df_merged$ghq17, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked" = -3, "Don't know" = -8, "Refusal" = -9))
df_merged$ghq25 <- labelled::labelled(df_merged$ghq25, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked" = -3, "Don't know" = -8, "Refusal" = -9))
df_merged$ghq32 <- labelled::labelled(df_merged$ghq32, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked" = -3, "Don't know" = -8, "Refusal" = -9))
df_merged$ghqtl15 <- labelled::labelled(df_merged$ghqtl15, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked" = -3, "Don't know" = -8, "Refusal" = -9))
df_merged$ghqtl17 <- labelled::labelled(df_merged$ghqtl17, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked" = -3, "Don't know" = -8, "Refusal" = -9))
df_merged$ghqtl25 <- labelled::labelled(df_merged$ghqtl25, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked" = -3, "Don't know" = -8, "Refusal" = -9))
df_merged$ghqtl32 <- labelled::labelled(df_merged$ghqtl32, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked" = -3, "Don't know" = -8, "Refusal" = -9))

# Convert to factors
df_merged <- df_merged %>%
  mutate(across(c(ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32), as.factor))

# Save to CSV
write_csv(df_merged, "data/output/cleaned_data.csv")
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load wave two (age 15)
data_w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t") %>%
  select(NSID, W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, W2strainYP, W2difficYP, W2activYP, W2probsYP, W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP, W2ghq12scr)

# Harmonize pre-derived score for wave two (earlier sweep)
data_w2 <- data_w2 %>%
  mutate(ghq15 = case_when(
    W2ghq12scr == -96 | W2ghq12scr == -99 ~ -3,
    W2ghq12scr == -97 | W2ghq12scr == -92 ~ -9,
    is.na(W2ghq12scr) ~ -3,
    TRUE ~ W2ghq12scr
  ))

# Compute sum score for wave two items
item_cols_w2 <- c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")
data_w2 <- data_w2 %>%
  mutate(
    ghqtl15 = case_when(
      rowSums(is.na(across(all_of(item_cols_w2)))) == 12 ~ -3,
      any(across(all_of(item_cols_w2), ~ .x < 0), na.rm = TRUE) ~ -8,
      TRUE ~ rowSums(across(all_of(item_cols_w2)), na.rm = TRUE)
    )
  )

# Load wave four (age 17)
data_w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t") %>%
  select(NSID, W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP, W4ghq12scr)

# Harmonize pre-derived score for wave four (earlier sweep)
data_w4 <- data_w4 %>%
  mutate(ghq17 = case_when(
    W4ghq12scr == -96 | W4ghq12scr == -99 ~ -3,
    W4ghq12scr == -97 | W4ghq12scr == -92 ~ -9,
    is.na(W4ghq12scr) ~ -3,
    TRUE ~ W4ghq12scr
  ))

# Compute sum score for wave four items
item_cols_w4 <- c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP")
data_w4 <- data_w4 %>%
  mutate(
    ghqtl17 = case_when(
      rowSums(is.na(across(all_of(item_cols_w4)))) == 12 ~ -3,
      any(across(all_of(item_cols_w4), ~ .x < 0), na.rm = TRUE) ~ -8,
      TRUE ~ rowSums(across(all_of(item_cols_w4)), na.rm = TRUE)
    )
  )

# Load wave eight self-completion (items)
data_ns8_self <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t") %>%
  select(NSID, W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4, W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8, W8GHQ12_9, W8GHQ12_10, W8GHQ12_11, W8GHQ12_12)

# Load wave eight derived (pre-derived)
data_ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t") %>%
  select(NSID, W8DGHQSC)

# Harmonize pre-derived score for wave eight (later sweep)
data_ns8_derived <- data_ns8_derived %>%
  mutate(ghq25 = ifelse(is.na(W8DGHQSC), -3, W8DGHQSC))

# Compute sum score for wave eight items
item_cols_ns8 <- c("W8GHQ12_1", "W8GHQ12_2", "W8GHQ12_3", "W8GHQ12_4", "W8GHQ12_5", "W8GHQ12_6", "W8GHQ12_7", "W8GHQ12_8", "W8GHQ12_9", "W8GHQ12_10", "W8GHQ12_11", "W8GHQ12_12")
data_ns8_self <- data_ns8_self %>%
  mutate(
    ghqtl25 = case_when(
      rowSums(is.na(across(all_of(item_cols_ns8)))) == 12 ~ -3,
      any(across(all_of(item_cols_ns8), ~ .x < 0), na.rm = TRUE) ~ -8,
      TRUE ~ rowSums(across(all_of(item_cols_ns8)), na.rm = TRUE)
    )
  )

# Load wave nine main interview (items)
data_ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t") %>%
  select(NSID, W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4, W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8, W9GHQ12_9, W9GHQ12_10, W9GHQ12_11, W9GHQ12_12)

# Load wave nine derived (pre-derived)
data_ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t") %>%
  select(NSID, W9DGHQSC)

# Harmonize pre-derived score for wave nine (later sweep)
data_ns9_derived <- data_ns9_derived %>%
  mutate(ghq32 = ifelse(is.na(W9DGHQSC), -3, W9DGHQSC))

# Compute sum score for wave nine items
item_cols_ns9 <- c("W9GHQ12_1", "W9GHQ12_2", "W9GHQ12_3", "W9GHQ12_4", "W9GHQ12_5", "W9GHQ12_6", "W9GHQ12_7", "W9GHQ12_8", "W9GHQ12_9", "W9GHQ12_10", "W9GHQ12_11", "W9GHQ12_12")
data_ns9_main <- data_ns9_main %>%
  mutate(
    ghqtl32 = case_when(
      rowSums(is.na(across(all_of(item_cols_ns9)))) == 12 ~ -3,
      any(across(all_of(item_cols_ns9), ~ .x < 0), na.rm = TRUE) ~ -8,
      TRUE ~ rowSums(across(all_of(item_cols_ns9)), na.rm = TRUE)
    )
  )

# Merge all datasets
data_merged <- data_w2 %>%
  select(NSID, ghq15, ghqtl15) %>%
  full_join(data_w4 %>% select(NSID, ghq17, ghqtl17), by = "NSID") %>%
  full_join(data_ns8_self %>% select(NSID, ghqtl25), by = "NSID") %>%
  full_join(data_ns8_derived %>% select(NSID, ghq25), by = "NSID") %>%
  full_join(data_ns9_main %>% select(NSID, ghqtl32), by = "NSID") %>%
  full_join(data_ns9_derived %>% select(NSID, ghq32), by = "NSID")

# Apply labels to all eight variables
data_merged <- data_merged %>%
  mutate(
    ghq15 = labelled(ghq15, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked at fieldwork stage" = -3, "Don't know" = -8, "Refusal" = -9)),
    ghq17 = labelled(ghq17, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked at fieldwork stage" = -3, "Don't know" = -8, "Refusal" = -9)),
    ghq25 = labelled(ghq25, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked at fieldwork stage" = -3, "Don't know" = -8, "Refusal" = -9)),
    ghq32 = labelled(ghq32, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked at fieldwork stage" = -3, "Don't know" = -8, "Refusal" = -9)),
    ghqtl15 = labelled(ghqtl15, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked at fieldwork stage" = -3, "Don't know" = -8, "Refusal" = -9)),
    ghqtl17 = labelled(ghqtl17, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked at fieldwork stage" = -3, "Don't know" = -8, "Refusal" = -9)),
    ghqtl25 = labelled(ghqtl25, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked at fieldwork stage" = -3, "Don't know" = -8, "Refusal" = -9)),
    ghqtl32 = labelled(ghqtl32, labels = c("Item not applicable" = -1, "Script error" = -2, "Not asked at fieldwork stage" = -3, "Don't know" = -8, "Refusal" = -9))
  )

# Convert all derived GHQ variables to factors
data_merged <- data_merged %>%
  mutate(across(ghq15:ghqtl32, as.factor))

# Write to CSV
write_csv(data_merged, "data/output/cleaned_data.csv")
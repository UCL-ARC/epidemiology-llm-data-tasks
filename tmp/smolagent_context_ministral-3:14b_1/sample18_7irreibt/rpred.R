
library(haven)
library(dplyr)
library(purrr)
library(readr)

# Step 1: Load input files
wave1 <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t") %>%
  select(NSID)

wave2 <- readr::read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t") %>%
  select(NSID, W2ghq12scr) %>%
  rename(ghq15 = W2ghq12scr) %>%
  mutate(ghq15 = case_when(
    is.na(ghq15) ~ -3,
    ghq15 %in% c(-96, -99) ~ -3,
    ghq15 %in% c(-97, -92) ~ -9,
    TRUE ~ ghq15
  ))

wave4 <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t") %>%
  select(NSID, W4ghq12scr) %>%
  rename(ghq17 = W4ghq12scr) %>%
  mutate(ghq17 = case_when(
    is.na(ghq17) ~ -3,
    ghq17 %in% c(-96, -99) ~ -3,
    ghq17 %in% c(-97, -92) ~ -9,
    TRUE ~ ghq17
  ))

wave8 <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t") %>%
  select(NSID, W8DGHQSC) %>%
  rename(ghq25 = W8DGHQSC) %>%
  mutate(ghq25 = case_when(
    is.na(ghq25) ~ -3,
    TRUE ~ ghq25
  ))

wave9 <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t") %>%
  select(NSID, W9DGHQSC) %>%
  rename(ghq32 = W9DGHQSC) %>%
  mutate(ghq32 = case_when(
    is.na(ghq32) ~ -3,
    TRUE ~ ghq32
  ))

# Load item-level data for manual computation
wave2_items <- readr::read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t") %>%
  select(NSID, W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, W2strainYP,
         W2difficYP, W2activYP, W2probsYP, W2depressYP, W2noconfYP,
         W2wthlessYP, W2happyYP)

wave4_items <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t") %>%
  select(NSID, W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, W4StrainYP,
         W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP, W4NoConfYP,
         W4WthlessYP, W4HappyYP)

wave8_items <- readr::read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t") %>%
  select(NSID, W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4, W8GHQ12_5,
         W8GHQ12_6, W8GHQ12_7, W8GHQ12_8, W8GHQ12_9, W8GHQ12_10,
         W8GHQ12_11, W8GHQ12_12)

wave9_items <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t") %>%
  select(NSID, W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4, W9GHQ12_5,
         W9GHQ12_6, W9GHQ12_7, W9GHQ12_8, W9GHQ12_9, W9GHQ12_10,
         W9GHQ12_11, W9GHQ12_12)

# Step 2: Compute GHQ sum scores
compute_ghq_sum <- function(data, age) {
  ghq_items <- data %>% select(-NSID)
  data %>%
    mutate(!!paste0("ghqtl", age) :=
      ifelse(rowSums(is.na(ghq_items)) == ncol(ghq_items), -3,
             ifelse(rowSums(ghq_items < 0) > 0, -8, rowSums(ghq_items, na.rm = TRUE)))) %>%
    select(NSID, matches(paste0("ghqtl", age)))
}

wave2_computed <- compute_ghq_sum(wave2_items, "15")
wave4_computed <- compute_ghq_sum(wave4_items, "17")
wave8_computed <- compute_ghq_sum(wave8_items, "25")
wave9_computed <- compute_ghq_sum(wave9_items, "32")

# Step 3: Merge datasets
merged_data <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID") %>%
  full_join(wave2_computed, by = "NSID") %>%
  full_join(wave4_computed, by = "NSID") %>%
  full_join(wave8_computed, by = "NSID") %>%
  full_join(wave9_computed, by = "NSID")

# Step 4: Convert derived variables to factors
ghq_vars <- c("ghq15", "ghq17", "ghq25", "ghq32", "ghqtl15", "ghqtl17", "ghqtl25", "ghqtl32")
for (var in ghq_vars) {
  if (var %in% names(merged_data)) {
    merged_data[[var]] <- as.factor(merged_data[[var]])
  }
}

# Step 5: Save output
output_path <- "data/output/cleaned_data.csv"
write.csv(merged_data %>% select(NSID, ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32),
           output_path, row.names = FALSE)

# Confirmation message
cat("Successfully created cleaned data file at:", output_path, "\n")

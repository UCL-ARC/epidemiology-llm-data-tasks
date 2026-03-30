library(haven)
library(dplyr)
library(purrr)
library(readr)

# Load input files
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t") %>% 
  select(NSID)

wave_two <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t") %>% 
  select(NSID, W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, W2strainYP, W2difficYP, W2activYP, W2probsYP, W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP, W2ghq12scr)

wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t") %>% 
  select(NSID, W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP, W4ghq12scr)

wave_eight <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t") %>% 
  select(NSID, W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4, W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8, W8GHQ12_9, W8GHQ12_10, W8GHQ12_11, W8GHQ12_12)

wave_eight_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t") %>% 
  select(NSID, W8DGHQSC)

wave_nine <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t") %>% 
  select(NSID, W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4, W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8, W9GHQ12_9, W9GHQ12_10, W9GHQ12_11, W9GHQ12_12)

wave_nine_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t") %>% 
  select(NSID, W9DGHQSC)

# Merge all datasets
merged_data <- full_join(wave_one, wave_two, by = "NSID") %>% 
  full_join(wave_four, by = "NSID") %>% 
  full_join(wave_eight, by = "NSID") %>% 
  full_join(wave_eight_derived, by = "NSID") %>% 
  full_join(wave_nine, by = "NSID") %>% 
  full_join(wave_nine_derived, by = "NSID")

# Helper function for GHQ sum calculation
calculate_ghq_sum <- function(...) {
  items <- list(...)
  if (all(is.na(unlist(items)))) {
    return(-3)
  }
  if (any(unlist(items) < 0)) {
    return(-8)
  }
  return(sum(unlist(items), na.rm = TRUE))
}

# Derive GHQ-12 sum scores (ghqtlXX variables)
merged_data <- merged_data %>% 
  mutate(
    ghqtl15 = calculate_ghq_sum(W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, W2strainYP, W2difficYP, W2activYP, W2probsYP, W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP),
    ghqtl17 = calculate_ghq_sum(W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP),
    ghqtl25 = calculate_ghq_sum(W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4, W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8, W8GHQ12_9, W8GHQ12_10, W8GHQ12_11, W8GHQ12_12),
    ghqtl32 = calculate_ghq_sum(W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4, W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8, W9GHQ12_9, W9GHQ12_10, W9GHQ12_11, W9GHQ12_12)
  )

# Harmonise pre-derived GHQ scores (ghqXX variables)
merged_data <- merged_data %>% 
  mutate(
    ghq15 = case_when(
      W2ghq12scr == -96 | W2ghq12scr == -99 ~ -3,
      W2ghq12scr == -97 | W2ghq12scr == -92 ~ -9,
      is.na(W2ghq12scr) ~ -3,
      TRUE ~ W2ghq12scr
    ),
    ghq17 = case_when(
      W4ghq12scr == -96 | W4ghq12scr == -99 ~ -3,
      W4ghq12scr == -97 | W4ghq12scr == -92 ~ -9,
      is.na(W4ghq12scr) ~ -3,
      TRUE ~ W4ghq12scr
    ),
    ghq25 = case_when(
      is.na(W8DGHQSC) ~ -3,
      TRUE ~ W8DGHQSC
    ),
    ghq32 = case_when(
      is.na(W9DGHQSC) ~ -3,
      TRUE ~ W9DGHQSC
    )
  )

# Select output variables and convert to factors
output_data <- merged_data %>% 
  select(NSID, ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32) %>% 
  mutate(across(where(is.numeric), as.factor))

# Write output file
write_csv(output_data, "data/output/cleaned_data.csv")

library(readr)
library(dplyr)
library(tidyr)
library(purrr)

# Load all necessary files
wave_two <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_self_comp <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Function to calculate GHQ-12 item-summed score
calculate_ghq12 <- function(data, age) {
  if (age == 2) {
    ghq_items <- data[, c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")]
  } else if (age == 4) {
    ghq_items <- data[, c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP")]
  } else if (age == 8) {
    ghq_items <- data[, c("W8GHQ12_1", "W8GHQ12_2", "W8GHQ12_3", "W8GHQ12_4", "W8GHQ12_5", "W8GHQ12_6", "W8GHQ12_7", "W8GHQ12_8", "W8GHQ12_9", "W8GHQ12_10", "W8GHQ12_11", "W8GHQ12_12")]
  } else if (age == 9) {
    ghq_items <- data[, c("W9GHQ12_1", "W9GHQ12_2", "W9GHQ12_3", "W9GHQ12_4", "W9GHQ12_5", "W9GHQ12_6", "W9GHQ12_7", "W9GHQ12_8", "W9GHQ12_9", "W9GHQ12_10", "W9GHQ12_11", "W9GHQ12_12")]
  }

  ghq12 <- rowSums(ghq_items, na.rm = FALSE)
  ghq12[is.na(ghq12)] <- -3

  # Check for negative values in any item
  has_negative <- apply(ghq_items, 1, function(x) any(x < 0, na.rm = TRUE))
  ghq12[has_negative] <- -8

  return(ghq12)
}

# Map missing values for pre-derived GHQ scores
map_pre_derived <- function(x) {
  x <- ifelse(x == -97, -9, x)
  x <- ifelse(x == -92, -9, x)
  x <- ifelse(is.na(x), -3, x)
  return(x)
}

# Process Wave 2 (Age 15)
wave_two <- wave_two %>%
  mutate(
    ghqtl15 = calculate_ghq12(., age = 2),
    ghq15 = map_pre_derived(W2ghq12scr)
  )

# Process Wave 4 (Age 17)
wave_four <- wave_four %>%
  mutate(
    ghqtl17 = calculate_ghq12(., age = 4),
    ghq17 = map_pre_derived(W4ghq12scr)
  )

# Process Wave 8 (Age 25)
ns8_self_comp <- ns8_self_comp %>%
  mutate(
    ghqtl25 = calculate_ghq12(., age = 8)
  )

# Merge Wave 8 self-completion with derived variables
ns8_merged <- full_join(ns8_self_comp, ns8_derived, by = "NSID") %>%
  mutate(
    ghq25 = map_pre_derived(W8DGHQSC)
  )

# Process Wave 9 (Age 32)
ns9_main <- ns9_main %>%
  mutate(
    ghqtl32 = calculate_ghq12(., age = 9)
  )

# Merge Wave 9 main interview with derived variables
ns9_derived <- ns9_derived %>%
  mutate(
    ghq32 = map_pre_derived(W9DGHQSC)
  )

ns9_merged <- full_join(ns9_main, ns9_derived, by = "NSID")

# Merge all datasets
merged_data <- full_join(wave_two, wave_four, by = "NSID") %>%
  full_join(ns8_merged, by = "NSID") %>%
  full_join(ns9_merged, by = "NSID")

# Select only the required variables
final_data <- merged_data %>%
  select(NSID, ghqtl15, ghq15, ghqtl17, ghq17, ghqtl25, ghq25, ghqtl32, ghq32)

# Write the final output
readr::write_csv(final_data, "data/output/cleaned_data.csv")
cat("Output file 'cleaned_data.csv' has been successfully written to data/output/")

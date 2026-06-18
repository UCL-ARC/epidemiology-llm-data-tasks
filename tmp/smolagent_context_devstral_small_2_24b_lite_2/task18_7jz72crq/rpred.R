library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all required datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8_sc <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets using NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8_sc, by = "NSID") %>%
  full_join(wave8_derived, by = "NSID") %>%
  full_join(wave9_main, by = "NSID") %>%
  full_join(wave9_derived, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(var) {
  case_when(
    var %in% c(-998, -997, -995) ~ -2,  # Schedule not applicable / script error / information lost
    var == -99 ~ -3,  # Not interviewed
    var == -97 ~ -9,  # Refused self-completion
    var == -96 ~ -3,  # Using interpreter (treated as not interviewed)
    var == -92 ~ -9,  # Refused
    var == -91 ~ -1,  # Not applicable
    var == -8 ~ -8,   # Don't know / insufficient information
    var == -3 ~ -3,   # Not asked at fieldwork stage
    var == -1 ~ -1,   # Not applicable
    is.na(var) ~ -3,  # Handle NA values
    TRUE ~ var
  )
}

# Function to compute Likert score for a set of variables
compute_likert <- function(data, vars) {
  # Apply missing value mapping to each variable
  mapped_vars <- lapply(vars, function(var_name) {
    map_missing(data[[var_name]])
  })
  
  # Convert to a matrix
  var_matrix <- as.matrix(do.call(cbind, mapped_vars))
  
  # Initialize the result vector
  likert_scores <- rep(NA, nrow(var_matrix))
  
  # Compute row sums for rows where all values are non-negative
  for (i in 1:nrow(var_matrix)) {
    if (all(var_matrix[i, ] >= 0)) {
      likert_scores[i] <- sum(var_matrix[i, ])
    }
  }
  
  return(likert_scores)
}

# Process each wave's GHQ-12 items and compute Likert scores
# Wave 2 (Age 15)
wave2_vars <- c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP",
                "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")

merged_data$ghqtl15 <- compute_likert(merged_data, wave2_vars)

# Wave 4 (Age 17)
wave4_vars <- c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP",
                "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP")

merged_data$ghqtl17 <- compute_likert(merged_data, wave4_vars)

# Wave 8 (Age 25)
wave8_vars <- c("W8GHQ12_1", "W8GHQ12_2", "W8GHQ12_3", "W8GHQ12_4", "W8GHQ12_5", "W8GHQ12_6",
                "W8GHQ12_7", "W8GHQ12_8", "W8GHQ12_9", "W8GHQ12_10", "W8GHQ12_11", "W8GHQ12_12")

merged_data$ghqtl25 <- compute_likert(merged_data, wave8_vars)

# Wave 9 (Age 32)
wave9_vars <- c("W9GHQ12_1", "W9GHQ12_2", "W9GHQ12_3", "W9GHQ12_4", "W9GHQ12_5", "W9GHQ12_6",
                "W9GHQ12_7", "W9GHQ12_8", "W9GHQ12_9", "W9GHQ12_10", "W9GHQ12_11", "W9GHQ12_12")

merged_data$ghqtl32 <- compute_likert(merged_data, wave9_vars)

# Process caseness scores using pre-derived variables
# Wave 2 (Age 15)
merged_data$ghq15 <- case_when(
  merged_data$W2ghq12scr == -99 ~ -3,
  merged_data$W2ghq12scr == -97 ~ -9,
  merged_data$W2ghq12scr == -96 ~ -3,
  merged_data$W2ghq12scr == -92 ~ -9,
  TRUE ~ merged_data$W2ghq12scr
)

# Wave 4 (Age 17)
merged_data$ghq17 <- case_when(
  merged_data$W4ghq12scr == -99 ~ -3,
  merged_data$W4ghq12scr == -97 ~ -9,
  merged_data$W4ghq12scr == -96 ~ -3,
  merged_data$W4ghq12scr == -92 ~ -9,
  TRUE ~ merged_data$W4ghq12scr
)

# Wave 8 (Age 25)
merged_data$ghq25 <- case_when(
  merged_data$W8DGHQSC == -9 ~ -9,
  merged_data$W8DGHQSC == -8 ~ -8,
  merged_data$W8DGHQSC == -1 ~ -1,
  TRUE ~ merged_data$W8DGHQSC
)

# Wave 9 (Age 32)
merged_data$ghq32 <- case_when(
  merged_data$W9DGHQSC == -9 ~ -9,
  merged_data$W9DGHQSC == -8 ~ -8,
  merged_data$W9DGHQSC == -1 ~ -1,
  TRUE ~ merged_data$W9DGHQSC
)

# Select only the required output variables
output_data <- merged_data %>%
  select(NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"
library(haven)
library(dplyr)
library(readr)

# Load datasets
wave1 <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t") %>%
  select(NSID)

wave2 <- readr::read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8_derived <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9_derived <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
wave8_self <- readr::read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave9_main <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Extract and rename GHQ scores
wave2_ghq <- wave2 %>% select(NSID, W2ghq12scr) %>% rename(ghq15 = W2ghq12scr)
wave4_ghq <- wave4 %>% select(NSID, W4ghq12scr) %>% rename(ghq17 = W4ghq12scr)
wave8_ghq <- wave8_derived %>% select(NSID, W8DGHQSC) %>% rename(ghq25 = W8DGHQSC)
wave9_ghq <- wave9_derived %>% select(NSID, W9DGHQSC) %>% rename(ghq32 = W9DGHQSC)

# Merge GHQ scores
merged_ghq <- full_join(wave1, wave2_ghq, by = "NSID") %>%
  full_join(wave4_ghq, by = "NSID") %>%
  full_join(wave8_ghq, by = "NSID") %>%
  full_join(wave9_ghq, by = "NSID")

# Load item-level data
wave2_items <- wave2 %>% select(NSID, W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, W2strainYP,
                                 W2difficYP, W2activYP, W2probsYP, W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP)
wave4_items <- wave4 %>% select(NSID, W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, W4StrainYP,
                                 W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP)
wave8_items <- wave8_self %>% select(NSID, starts_with("W8GHQ12_"))
wave9_items <- wave9_main %>% select(NSID, starts_with("W9GHQ12_"))

# Merge item-level data with GHQ scores
merged_data <- merged_ghq %>%
  left_join(wave2_items, by = "NSID") %>%
  left_join(wave4_items, by = "NSID") %>%
  left_join(wave8_items, by = "NSID") %>%
  left_join(wave9_items, by = "NSID")

# Diagnostic message
cat("Columns in merged_data:", paste(names(merged_data), collapse = ", "), "\n")

# Function to compute GHQ sum scores
compute_ghq_sum <- function(data, age, item_vars) {
  missing_vars <- setdiff(item_vars, names(data))
  if (length(missing_vars) > 0) {
    cat("Warning: Missing columns for age", age, ":", paste(missing_vars, collapse = ", "), "\n")
    data[[paste0("ghqtl", age)]] <- -8
    return(data)
  }

  all_na <- sapply(data[, item_vars], function(x) all(is.na(x)))
  data[[paste0("ghqtl", age)]] <- ifelse(all(all_na), -3,
    ifelse(any(data[, item_vars] < 0), -8, rowSums(data[, item_vars], na.rm = TRUE)))
  return(data)
}

# Define item variables
item_vars_15 <- c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP",
                  "W2difficYP", "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP",
                  "W2wthlessYP", "W2happyYP")

item_vars_17 <- c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP",
                  "W4DifficYP", "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP",
                  "W4WthlessYP", "W4HappyYP")

item_vars_25 <- grep("^W8GHQ12_", names(merged_data), value = TRUE)
item_vars_32 <- grep("^W9GHQ12_", names(merged_data), value = TRUE)

# Compute GHQ sum scores
cat("Computing GHQ scores for age 15\n")
merged_data <- compute_ghq_sum(merged_data, 15, item_vars_15)

cat("Computing GHQ scores for age 17\n")
merged_data <- compute_ghq_sum(merged_data, 17, item_vars_17)

cat("Computing GHQ scores for age 25\n")
if (length(item_vars_25) == 12) {
  merged_data <- compute_ghq_sum(merged_data, 25, item_vars_25)
} else {
  cat("Warning: Not enough W8GHQ12_ columns found, skipping age 25\n")
}

cat("Computing GHQ scores for age 32\n")
if (length(item_vars_32) == 12) {
  merged_data <- compute_ghq_sum(merged_data, 32, item_vars_32)
} else {
  cat("Warning: Not enough W9GHQ12_ columns found, skipping age 32\n")
}

# Harmonize pre-derived GHQ scores
merged_data <- merged_data %>%
  mutate(
    ghq15 = case_when(
      is.na(ghq15) ~ -3,
      ghq15 == -96 | ghq15 == -99 ~ -3,
      ghq15 == -97 | ghq15 == -92 ~ -9,
      TRUE ~ ghq15
    ),
    ghq17 = case_when(
      is.na(ghq17) ~ -3,
      ghq17 == -96 | ghq17 == -99 ~ -3,
      ghq17 == -97 | ghq17 == -92 ~ -9,
      TRUE ~ ghq17
    ),
    ghq25 = case_when(is.na(ghq25) ~ -3, TRUE ~ ghq25),
    ghq32 = case_when(is.na(ghq32) ~ -3, TRUE ~ ghq32)
  )

# Convert derived variables to factors
derived_vars <- c("ghq15", "ghq17", "ghq25", "ghq32")
if ("ghqtl15" %in% names(merged_data)) derived_vars <- c(derived_vars, "ghqtl15")
if ("ghqtl17" %in% names(merged_data)) derived_vars <- c(derived_vars, "ghqtl17")
if ("ghqtl25" %in% names(merged_data)) derived_vars <- c(derived_vars, "ghqtl25")
if ("ghqtl32" %in% names(merged_data)) derived_vars <- c(derived_vars, "ghqtl32")

for (var in derived_vars) {
  if (var %in% names(merged_data)) {
    merged_data[[var]] <- as.factor(merged_data[[var]])
  }
}

# Select only required variables
final_data <- merged_data %>%
  select(NSID, ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32)

# Write output file
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)
cat("Output file written to data/output/cleaned_data.csv\n")
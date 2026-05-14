library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load wave 2 (age 15)
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", 
                     delim = "\t") %>%
  select(NSID, W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, 
         W2strainYP, W2difficYP, W2activYP, W2probsYP, 
         W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP,
         W2ghq12scr)

# Load wave 4 (age 17)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab",
                     delim = "\t") %>%
  select(NSID, W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP,
         W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP,
         W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP,
         W4ghq12scr)

# Load wave 8 (age 25) self completion
wave8_items <- read_delim("data/input/ns8_2015_self_completion.tab",
                          delim = "\t") %>%
  select(NSID, W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4,
         W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8,
         W8GHQ12_9, W8GHQ12_10, W8GHQ12_11, W8GHQ12_12)

# Load wave 8 (age 25) derived
wave8_derived <- read_delim("data/input/ns8_2015_derived.tab",
                            delim = "\t") %>%
  select(NSID, W8DGHQSC)

# Load wave 9 (age 32) main interview
wave9_items <- read_delim("data/input/ns9_2022_main_interview.tab",
                          delim = "\t") %>%
  select(NSID, W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4,
         W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8,
         W9GHQ12_9, W9GHQ12_10, W9GHQ12_11, W9GHQ12_12)

# Load wave 9 (age 32) derived
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab",
                            delim = "\t") %>%
  select(NSID, W9DGHQSC)

# Merge all datasets
merged <- full_join(wave2, wave4, by = "NSID")
merged <- full_join(merged, wave8_items, by = "NSID")
merged <- full_join(merged, wave8_derived, by = "NSID")
merged <- full_join(merged, wave9_items, by = "NSID")
merged <- full_join(merged, wave9_derived, by = "NSID")

# Function to compute ghqtl scores for a single row
calc_ghqtl_row <- function(row_items) {
  all_na <- all(is.na(row_items))
  has_neg <- any(row_items < 0, na.rm = TRUE)
  
  if (all_na) {
    return(-3)
  } else if (has_neg) {
    return(-8)
  } else {
    return(sum(row_items, na.rm = TRUE))
  }
}

# Compute ghqtl15 from wave 2 items
ghq15_items <- merged %>% select(W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, 
                                  W2strainYP, W2difficYP, W2activYP, W2probsYP, 
                                  W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP)
merged$ghqtl15 <- apply(ghq15_items, 1, calc_ghqtl_row)

# Compute ghqtl17 from wave 4 items
ghq17_items <- merged %>% select(W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP,
                                  W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP,
                                  W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP)
merged$ghqtl17 <- apply(ghq17_items, 1, calc_ghqtl_row)

# Compute ghqtl25 from wave 8 items
ghq25_items <- merged %>% select(W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4,
                                  W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8,
                                  W8GHQ12_9, W8GHQ12_10, W8GHQ12_11, W8GHQ12_12)
merged$ghqtl25 <- apply(ghq25_items, 1, calc_ghqtl_row)

# Compute ghqtl32 from wave 9 items
ghq32_items <- merged %>% select(W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4,
                                  W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8,
                                  W9GHQ12_9, W9GHQ12_10, W9GHQ12_11, W9GHQ12_12)
merged$ghqtl32 <- apply(ghq32_items, 1, calc_ghqtl_row)

# Harmonize pre-derived GHQ scores for wave 2 (age 15)
merged$ghq15 <- merged$W2ghq12scr
merged$ghq15[merged$ghq15 == -96 | merged$ghq15 == -99] <- -3
merged$ghq15[merged$ghq15 == -97 | merged$ghq15 == -92] <- -9
merged$ghq15[is.na(merged$ghq15)] <- -3

# Harmonize pre-derived GHQ scores for wave 4 (age 17)
merged$ghq17 <- merged$W4ghq12scr
merged$ghq17[merged$ghq17 == -96 | merged$ghq17 == -99] <- -3
merged$ghq17[merged$ghq17 == -97 | merged$ghq17 == -92] <- -9
merged$ghq17[is.na(merged$ghq17)] <- -3

# Harmonize pre-derived GHQ scores for wave 8 (age 25)
merged$ghq25 <- merged$W8DGHQSC
merged$ghq25[is.na(merged$ghq25)] <- -3

# Harmonize pre-derived GHQ scores for wave 9 (age 32)
merged$ghq32 <- merged$W9DGHQSC
merged$ghq32[is.na(merged$ghq32)] <- -3

# Define value labels as a named list with character keys
val_labels <- list(
  "-1" = "Item not applicable",
  "-2" = "Script error/information lost",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-8" = "Don't know/insufficient information",
  "-9" = "Refusal"
)

# Apply labels to all derived variables using labelled::set_value_labels
label_vars <- c("ghq15", "ghq17", "ghq25", "ghq32", "ghqtl15", "ghqtl17", "ghqtl25", "ghqtl32")
for (var in label_vars) {
  merged[[var]] <- labelled::set_value_labels(merged[[var]], labels = val_labels)
}

# Convert all derived GHQ variables to factors
for (var in label_vars) {
  merged[[var]] <- as.factor(merged[[var]])
}

# Select only required columns
output <- merged %>% select(NSID, ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32)

# Write output
csv_path <- "data/output/cleaned_data.csv"
dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
write_csv(output, csv_path)

cat("Output written to:", csv_path, "\n")
cat("Number of rows:", nrow(output), "\n")
cat("Number of columns:", ncol(output), "\n")

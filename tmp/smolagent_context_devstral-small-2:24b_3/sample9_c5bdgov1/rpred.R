library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave4 <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets
merged <- full_join(wave1, wave2, by = "NSID")
merged <- full_join(merged, wave4, by = "NSID")

# Select variables
selected <- merged %>% select(NSID, W1hiqualmum, W1hiqualdad, W2hiqualmum, W2hiqualdad, w4hiqualmum, w4hiqualdad)

# Consolidation logic for maternal education
educdtlma <- case_when(
  !is.na(selected$W1hiqualmum) & selected$W1hiqualmum >= 1 ~ selected$W1hiqualmum,
  !is.na(selected$W1hiqualmum) & selected$W1hiqualmum < 1 ~ selected$W1hiqualmum,
  !is.na(selected$W2hiqualmum) & selected$W2hiqualmum >= 1 ~ selected$W2hiqualmum,
  !is.na(selected$W2hiqualmum) & selected$W2hiqualmum < 1 ~ selected$W2hiqualmum,
  !is.na(selected$w4hiqualmum) & selected$w4hiqualmum >= 1 ~ selected$w4hiqualmum,
  !is.na(selected$w4hiqualmum) & selected$w4hiqualmum < 1 ~ selected$w4hiqualmum,
  TRUE ~ -3
)

# Consolidation logic for paternal education
educdtlpa <- case_when(
  !is.na(selected$W1hiqualdad) & selected$W1hiqualdad >= 1 ~ selected$W1hiqualdad,
  !is.na(selected$W1hiqualdad) & selected$W1hiqualdad < 1 ~ selected$W1hiqualdad,
  !is.na(selected$W2hiqualdad) & selected$W2hiqualdad >= 1 ~ selected$W2hiqualdad,
  !is.na(selected$W2hiqualdad) & selected$W2hiqualdad < 1 ~ selected$W2hiqualdad,
  !is.na(selected$w4hiqualdad) & selected$w4hiqualdad >= 1 ~ selected$w4hiqualdad,
  !is.na(selected$w4hiqualdad) & selected$w4hiqualdad < 1 ~ selected$w4hiqualdad,
  TRUE ~ -3
)

# Derived collapsed variables for maternal education
educma <- case_when(
  educdtlma %in% c(1, 2, 3, 4) ~ 0,
  educdtlma %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,
  educdtlma %in% c(18) ~ 2,
  educdtlma %in% c(19) ~ 3,
  educdtlma %in% c(20) ~ 4,
  educdtlma < 0 ~ educdtlma,
  TRUE ~ -3
)

# Derived collapsed variables for paternal education
educpa <- case_when(
  educdtlpa %in% c(1, 2, 3, 4) ~ 0,
  educdtlpa %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,
  educdtlpa %in% c(18) ~ 2,
  educdtlpa %in% c(19) ~ 3,
  educdtlpa %in% c(20) ~ 4,
  educdtlpa < 0 ~ educdtlpa,
  TRUE ~ -3
)

# Create final dataset
final <- data.frame(NSID = selected$NSID, educma, educpa, educdtlma, educdtlpa)

# Save to CSV
write.csv(final, "data/output/cleaned_data.csv", row.names = FALSE)
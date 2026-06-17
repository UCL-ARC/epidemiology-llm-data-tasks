library(readr)
library(dplyr)
library(tidyr)
library(haven)
library(labelled)
library(purrr)

# Load all files from metadata
# Wave 1 (Age 14) - only ID
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", 
                     delim = "\t", col_types = cols())

# Wave 2 (Age 15)
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", 
                     delim = "\t", col_types = cols())

# Wave 4 (Age 17)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", 
                     delim = "\t", col_types = cols())

# Wave 8 (Age 25) - self completion
wave8_sc <- read_delim("data/input/ns8_2015_self_completion.tab", 
                       delim = "\t", col_types = cols())

# Wave 8 (Age 25) - derived
wave8_derived <- read_delim("data/input/ns8_2015_derived.tab", 
                            delim = "\t", col_types = cols())

# Wave 9 (Age 32) - main interview
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", 
                    delim = "\t", col_types = cols())

# Wave 9 (Age 32) - derived
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", 
                            delim = "\t", col_types = cols())

# Function to compute item-summed GHQ score
calc_ghq_tl <- function(items) {
  # Check if all items are NA
  if (all(is.na(items))) {
    return(-3)  # did not participate
  }
  # Check if any item has a negative value
  if (any(items < 0)) {
    return(-8)  # insufficient information
  }
  # Sum valid items
  return(sum(items, na.rm = TRUE))
}

# Compute item-summed scores for each wave separately
# Wave 2 (Age 15) GHQ items
ghq_items_w2 <- wave2 %>%
  select(W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, W2strainYP, 
         W2difficYP, W2activYP, W2probsYP, W2depressYP, W2noconfYP, 
         W2wthlessYP, W2happyYP)

ghq_tl15 <- as.data.frame(apply(as.matrix(ghq_items_w2), 1, calc_ghq_tl))
names(ghq_tl15) <- "ghqtl15"
ghq_tl15$NSID <- wave2$NSID

# Wave 4 (Age 17) GHQ items
ghq_items_w4 <- wave4 %>%
  select(W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, W4StrainYP, 
         W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP, W4NoConfYP, 
         W4WthlessYP, W4HappyYP)

ghq_tl17 <- as.data.frame(apply(as.matrix(ghq_items_w4), 1, calc_ghq_tl))
names(ghq_tl17) <- "ghqtl17"
ghq_tl17$NSID <- wave4$NSID

# Wave 8 (Age 25) GHQ items
ghq_items_w8 <- wave8_sc %>%
  select(W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4, W8GHQ12_5, 
         W8GHQ12_6, W8GHQ12_7, W8GHQ12_8, W8GHQ12_9, W8GHQ12_10, 
         W8GHQ12_11, W8GHQ12_12)

ghq_tl25 <- as.data.frame(apply(as.matrix(ghq_items_w8), 1, calc_ghq_tl))
names(ghq_tl25) <- "ghqtl25"
ghq_tl25$NSID <- wave8_sc$NSID

# Wave 9 (Age 32) GHQ items
ghq_items_w9 <- wave9 %>%
  select(W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4, W9GHQ12_5, 
         W9GHQ12_6, W9GHQ12_7, W9GHQ12_8, W9GHQ12_9, W9GHQ12_10, 
         W9GHQ12_11, W9GHQ12_12)

ghq_tl32 <- as.data.frame(apply(as.matrix(ghq_items_w9), 1, calc_ghq_tl))
names(ghq_tl32) <- "ghqtl32"
ghq_tl32$NSID <- wave9$NSID

cat("Item-summed scores computed.\n")

# Function to harmonise pre-derived GHQ scores
harmonise_ghq_derived <- function(score) {
  # -97 (YP refused self completion) and -92 (Refused) map to -9
  if (score %in% c(-97, -92)) {
    return(-9)
  }
  # -99 (YP not interviewed) maps to -3 (not asked at fieldwork stage / not interviewed)
  if (score == -99) {
    return(-3)
  }
  # -96 (YP using interpreter) - treat as not applicable
  if (score == -96) {
    return(-1)
  }
  # -999, -998, -997, -995 (various errors/missing) map to -2
  if (score %in% c(-999, -998, -997, -995)) {
    return(-2)
  }
  # -91 (Not applicable) maps to -1
  if (score == -91) {
    return(-1)
  }
  # -8 (Don't know) maps to -8
  if (score == -8) {
    return(-8)
  }
  # -9 (Refused) stays as -9
  if (score == -9) {
    return(-9)
  }
  # -1 (Not applicable) stays as -1
  if (score == -1) {
    return(-1)
  }
  # -3 (Not asked at fieldwork stage) stays as -3
  if (score == -3) {
    return(-3)
  }
  # Valid scores (0-12) stay as is
  if (score >= 0 && score <= 12) {
    return(score)
  }
  # Default: NA for unknown codes
  return(NA)
}

# Harmonise pre-derived scores for waves 2 and 4 (apply special rules)
ghq15 <- sapply(wave2$W2ghq12scr, harmonise_ghq_derived)
ghq17 <- sapply(wave4$W4ghq12scr, harmonise_ghq_derived)

df_ghq15 <- data.frame(NSID = wave2$NSID, ghq15 = ghq15)
df_ghq17 <- data.frame(NSID = wave4$NSID, ghq17 = ghq17)

# For waves 8 and 9, just map standard codes
df_ghq25 <- sapply(wave8_derived$W8DGHQSC, function(x) {
  if (x %in% c(-9, -8, -1)) {
    return(x)
  } else if (x == -3) {
    return(-3)
  } else if (x >= 0 && x <= 12) {
    return(x)
  } else {
    return(NA)
  }
})

df_ghq25 <- data.frame(NSID = wave8_derived$NSID, ghq25 = df_ghq25)

df_ghq32 <- sapply(wave9_derived$W9DGHQSC, function(x) {
  if (x %in% c(-9, -8, -1)) {
    return(x)
  } else if (x == -3) {
    return(-3)
  } else if (x >= 0 && x <= 12) {
    return(x)
  } else {
    return(NA)
  }
})

df_ghq32 <- data.frame(NSID = wave9_derived$NSID, ghq32 = df_ghq32)

cat("Pre-derived scores harmonised.\n")

# Merge all files by NSID
df <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8_sc, by = "NSID") %>%
  full_join(wave8_derived, by = "NSID") %>%
  full_join(wave9, by = "NSID") %>%
  full_join(wave9_derived, by = "NSID")

# Now add the computed scores
df <- df %>%
  full_join(ghq_tl15, by = "NSID") %>%
  full_join(ghq_tl17, by = "NSID") %>%
  full_join(ghq_tl25, by = "NSID") %>%
  full_join(ghq_tl32, by = "NSID") %>%
  full_join(df_ghq15, by = "NSID") %>%
  full_join(df_ghq17, by = "NSID") %>%
  full_join(df_ghq25, by = "NSID") %>%
  full_join(df_ghq32, by = "NSID")

cat("Data merged. Dimensions:", nrow(df), "rows,", ncol(df), "columns\n")

# Select only final variables: NSID and 8 GHQ variables
result <- df %>%
  select(NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32)

cat("Result dimensions:", nrow(result), "rows,", ncol(result), "columns\n")

# Write output
write_csv(result, "data/output/cleaned_data.csv")
cat("Output written to data/output/cleaned_data.csv\n")

# Summary of results
summarise_result <- result %>%
  summarise(
    n = n(),
    ghqtl15_n_valid = sum(ghqtl15 >= 0 & ghqtl15 <= 12, na.rm = TRUE),
    ghqtl17_n_valid = sum(ghqtl17 >= 0 & ghqtl17 <= 12, na.rm = TRUE),
    ghqtl25_n_valid = sum(ghqtl25 >= 0 & ghqtl25 <= 12, na.rm = TRUE),
    ghqtl32_n_valid = sum(ghqtl32 >= 0 & ghqtl32 <= 12, na.rm = TRUE),
    ghq15_n_valid = sum(ghq15 >= 0 & ghq15 <= 12, na.rm = TRUE),
    ghq17_n_valid = sum(ghq17 >= 0 & ghq17 <= 12, na.rm = TRUE),
    ghq25_n_valid = sum(ghq25 >= 0 & ghq25 <= 12, na.rm = TRUE),
    ghq32_n_valid = sum(ghq32 >= 0 & ghq32 <= 12, na.rm = TRUE)
  )

cat("Summary:\n")
print(summarise_result)

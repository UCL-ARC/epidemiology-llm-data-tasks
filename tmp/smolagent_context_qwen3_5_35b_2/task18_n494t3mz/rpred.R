library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", recursive = TRUE, showWarnings = FALSE)

# Load each file explicitly by name
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
w8_self <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
w8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
w9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
w9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Full join all files by NSID
df <- full_join(wave2, wave4, by = "NSID")
df <- full_join(df, w8_self, by = "NSID")
df <- full_join(df, w8_derived, by = "NSID")
df <- full_join(df, w9_main, by = "NSID")
df <- full_join(df, w9_derived, by = "NSID")

# Function to recode negative values to standard missing codes
# For waves 2 and 4: -97 and -92 map to -9 (Refused)
# For waves 8 and 9: -9 maps to -9 (Refused), -8 maps to -8 (Don't know), -1 maps to -1 (Not applicable)
recodew2w4 <- function(x) {
  x[x == -999] <- -2
  x[x == -998] <- -2
  x[x == -997] <- -2
  x[x == -995] <- -2
  x[x == -99] <- -3
  x[x == -97] <- -9
  x[x == -96] <- -3
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  x
}

recodew8w9 <- function(x) {
  x[x == -9] <- -9
  x[x == -8] <- -8
  x[x == -1] <- -1
  x
}

# Recode GHQ items for wave 2 (age 15)
df$W2concenYP <- recodew2w4(df$W2concenYP)
df$W2nosleepYP <- recodew2w4(df$W2nosleepYP)
df$W2usefulYP <- recodew2w4(df$W2usefulYP)
df$W2decideYP <- recodew2w4(df$W2decideYP)
df$W2strainYP <- recodew2w4(df$W2strainYP)
df$W2difficYP <- recodew2w4(df$W2difficYP)
df$W2activYP <- recodew2w4(df$W2activYP)
df$W2probsYP <- recodew2w4(df$W2probsYP)
df$W2depressYP <- recodew2w4(df$W2depressYP)
df$W2noconfYP <- recodew2w4(df$W2noconfYP)
df$W2wthlessYP <- recodew2w4(df$W2wthlessYP)
df$W2happyYP <- recodew2w4(df$W2happyYP)

# Recode pre-derived score for wave 2 (age 15)
df$W2ghq12scr <- recodew2w4(df$W2ghq12scr)

# Recode GHQ items for wave 4 (age 17)
df$W4ConcenYP <- recodew2w4(df$W4ConcenYP)
df$W4NoSleepYP <- recodew2w4(df$W4NoSleepYP)
df$W4UsefulYP <- recodew2w4(df$W4UsefulYP)
df$W4DecideYP <- recodew2w4(df$W4DecideYP)
df$W4StrainYP <- recodew2w4(df$W4StrainYP)
df$W4DifficYP <- recodew2w4(df$W4DifficYP)
df$W4ActivYP <- recodew2w4(df$W4ActivYP)
df$W4ProbsYP <- recodew2w4(df$W4ProbsYP)
df$W4DepressYP <- recodew2w4(df$W4DepressYP)
df$W4NoConfYP <- recodew2w4(df$W4NoConfYP)
df$W4WthlessYP <- recodew2w4(df$W4WthlessYP)
df$W4HappyYP <- recodew2w4(df$W4HappyYP)

# Recode pre-derived score for wave 4 (age 17)
df$W4ghq12scr <- recodew2w4(df$W4ghq12scr)

# Recode GHQ items for wave 8 (age 25)
df$W8GHQ12_1 <- recodew8w9(df$W8GHQ12_1)
df$W8GHQ12_2 <- recodew8w9(df$W8GHQ12_2)
df$W8GHQ12_3 <- recodew8w9(df$W8GHQ12_3)
df$W8GHQ12_4 <- recodew8w9(df$W8GHQ12_4)
df$W8GHQ12_5 <- recodew8w9(df$W8GHQ12_5)
df$W8GHQ12_6 <- recodew8w9(df$W8GHQ12_6)
df$W8GHQ12_7 <- recodew8w9(df$W8GHQ12_7)
df$W8GHQ12_8 <- recodew8w9(df$W8GHQ12_8)
df$W8GHQ12_9 <- recodew8w9(df$W8GHQ12_9)
df$W8GHQ12_10 <- recodew8w9(df$W8GHQ12_10)
df$W8GHQ12_11 <- recodew8w9(df$W8GHQ12_11)
df$W8GHQ12_12 <- recodew8w9(df$W8GHQ12_12)

# Recode pre-derived score for wave 8 (age 25)
df$W8DGHQSC <- recodew8w9(df$W8DGHQSC)

# Recode GHQ items for wave 9 (age 32)
df$W9GHQ12_1 <- recodew8w9(df$W9GHQ12_1)
df$W9GHQ12_2 <- recodew8w9(df$W9GHQ12_2)
df$W9GHQ12_3 <- recodew8w9(df$W9GHQ12_3)
df$W9GHQ12_4 <- recodew8w9(df$W9GHQ12_4)
df$W9GHQ12_5 <- recodew8w9(df$W9GHQ12_5)
df$W9GHQ12_6 <- recodew8w9(df$W9GHQ12_6)
df$W9GHQ12_7 <- recodew8w9(df$W9GHQ12_7)
df$W9GHQ12_8 <- recodew8w9(df$W9GHQ12_8)
df$W9GHQ12_9 <- recodew8w9(df$W9GHQ12_9)
df$W9GHQ12_10 <- recodew8w9(df$W9GHQ12_10)
df$W9GHQ12_11 <- recodew8w9(df$W9GHQ12_11)
df$W9GHQ12_12 <- recodew8w9(df$W9GHQ12_12)

# Recode pre-derived score for wave 9 (age 32)
df$W9DGHQSC <- recodew8w9(df$W9DGHQSC)

# Function to compute item-summed Likert scores
# If all 12 items are NA -> assign -3 (did not participate)
# If any item has a negative value -> assign -8 (insufficient information)
# Otherwise -> sum the 12 items (valid score range 0-12)
compute_ghq_sum <- function(items) {
  all_na <- all(is.na(items))
  has_neg <- any(items < 0, na.rm = TRUE)
  
  if (all_na) {
    return(-3)
  } else if (has_neg) {
    return(-8)
  } else {
    return(sum(items, na.rm = TRUE))
  }
}

# Create item-summed Likert scores (ghqtl15, ghqtl17, ghqtl25, ghqtl32)
df$ghqtl15 <- apply(df[, c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", 
                           "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP",
                           "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")], 
                    1, compute_ghq_sum)

df$ghqtl17 <- apply(df[, c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", 
                           "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP",
                           "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP")], 
                    1, compute_ghq_sum)

df$ghqtl25 <- apply(df[, c("W8GHQ12_1", "W8GHQ12_2", "W8GHQ12_3", "W8GHQ12_4", 
                           "W8GHQ12_5", "W8GHQ12_6", "W8GHQ12_7", "W8GHQ12_8",
                           "W8GHQ12_9", "W8GHQ12_10", "W8GHQ12_11", "W8GHQ12_12")], 
                    1, compute_ghq_sum)

df$ghqtl32 <- apply(df[, c("W9GHQ12_1", "W9GHQ12_2", "W9GHQ12_3", "W9GHQ12_4", 
                           "W9GHQ12_5", "W9GHQ12_6", "W9GHQ12_7", "W9GHQ12_8",
                           "W9GHQ12_9", "W9GHQ12_10", "W9GHQ12_11", "W9GHQ12_12")], 
                    1, compute_ghq_sum)

# Create pre-derived caseness scores (ghq15, ghq17, ghq25, ghq32)
df$ghq15 <- df$W2ghq12scr
df$ghq17 <- df$W4ghq12scr
df$ghq25 <- df$W8DGHQSC
df$ghq32 <- df$W9DGHQSC

# Select only NSID and final derived variables
output <- df %>%
  select(NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32)

# Write output
cat("Output dimensions:", nrow(output), "rows,", ncol(output), "columns\n")
cat("Output variable names:", names(output), "\n")
write_csv(output, "data/output/cleaned_data.csv")
cat("Output written to data/output/cleaned_data.csv\n")

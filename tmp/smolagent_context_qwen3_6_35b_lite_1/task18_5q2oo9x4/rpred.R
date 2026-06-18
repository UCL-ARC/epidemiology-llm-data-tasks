# Load required libraries
library(dplyr)
library(readr)
library(tidyr)
library(haven)
library(labelled)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Define file paths
files <- c(
  "data/input/wave_one_lsype_young_person_2020.tab",
  "data/input/wave_two_lsype_young_person_2020.tab",
  "data/input/wave_four_lsype_young_person_2020.tab",
  "data/input/ns8_2015_self_completion.tab",
  "data/input/ns8_2015_derived.tab",
  "data/input/ns9_2022_main_interview.tab",
  "data/input/ns9_2022_derived_variables.tab"
)

# Load all files
w1 <- read_delim(files[1], delim = "\t", col_select = NSID, show_col_types = FALSE)
w2 <- read_delim(files[2], delim = "\t", col_select = c(NSID, W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, W2strainYP, W2difficYP, W2activYP, W2probsYP, W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP), show_col_types = FALSE)
w4 <- read_delim(files[3], delim = "\t", col_select = c(NSID, W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP), show_col_types = FALSE)
w8 <- read_delim(files[4], delim = "\t", col_select = c(NSID, W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4, W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8, W8GHQ12_9, W8GHQ12_10, W8GHQ12_11, W8GHQ12_12), show_col_types = FALSE)
w8d <- read_delim(files[5], delim = "\t", col_select = NSID, show_col_types = FALSE)
w9 <- read_delim(files[6], delim = "\t", col_select = c(NSID, W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4, W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8, W9GHQ12_9, W9GHQ12_10, W9GHQ12_11, W9GHQ12_12), show_col_types = FALSE)
w9d <- read_delim(files[7], delim = "\t", col_select = NSID, show_col_types = FALSE)

# Merge all files
all_data <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w4, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w8d, by = "NSID") %>%
  full_join(w9, by = "NSID") %>%
  full_join(w9d, by = "NSID")

cat("Merged data dimensions:", dim(all_data), "\n")

# Function to recode missing values for Wave 2/4 style missing codes
recode_wave2_4_missing <- function(x) {
  case_when(
    is.na(x) ~ -3,
    x == -998 ~ -2,  # Interviewer missed question
    x == -997 ~ -2,  # Script error
    x == -995 ~ -2,  # Missing history section data
    x == -99 ~ -3,   # YP not interviewed
    x == -97 ~ -2,   # YP refused self completion
    x == -96 ~ -3,   # YP using interpreter (not interviewed)
    x == -92 ~ -9,   # Refused
    x == -91 ~ -1,   # Not applicable
    x == -94 ~ -2,   # Insufficient information (if present)
    x == -1 ~ -8,    # Don't Know
    TRUE ~ x
  )
}

# Function to recode missing values for Wave 8/9 style missing codes
recode_wave8_9_missing <- function(x) {
  case_when(
    is.na(x) ~ -3,
    x == -9 ~ -9,   # Refused
    x == -8 ~ -8,   # Don't know
    x == -3 ~ -3,   # Not asked at fieldwork stage
    x == -1 ~ -1,   # Not applicable
    TRUE ~ x
  )
}

# Create helper function to recode a single GHQ item for Likert scoring
# All items use 1-4 scale where higher = worse mental health
# For positively worded items, we reverse the scoring
# For negatively worded items, the scoring is direct

# Define positively worded items (indices in 1-12 list): 1,3,4,6,7,9,10,11,12
# These need to be reversed for scoring

# Wave 2 (Age 15) - GHQ items
w2_raw <- all_data %>% select(NSID, W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, 
                               W2strainYP, W2difficYP, W2activYP, W2probsYP, W2depressYP, 
                               W2noconfYP, W2wthlessYP, W2happyYP)

# Recode missing values
w2_raw <- w2_raw %>% mutate(across(-NSID, recode_wave2_4_missing))

# Recode for scoring: positively worded items need reverse scoring
# Positively worded items: W2concenYP(1), W2usefulYP(3), W2decideYP(4), W2activYP(7), W2happyYP(12)
# Wait, let me check the labels again:
# W2concenYP: "Better than usual" (1), "Same as usual" (2), "Less than usual" (3), "Much less than usual" (4)
#   This is positively worded - higher = worse, so 1->0, 2->0, 3->1, 4->1 for caseness
#   For Likert sum, we keep as 1-4
# Actually for Likert sum, we sum the raw 1-4 values. The instruction says "only summing if all values are non negative"
# So we just sum the recoded values (which are 1-4 for valid responses)

# Let me recalculate - for Likert sum we just sum the 1-4 values
# For caseness, we use 0-0-1-1 threshold

# Wave 2 (Age 15) - Likert sum
w2_likert <- w2_raw %>%
  rowwise() %>%
  mutate(ghqtl15 = {
    items <- c_across(-NSID)
    if (all(items >= 1 & items <= 4)) {
      sum(items)
    } else {
      NA_real_
    }
  }) %>%
  ungroup() %>%
  select(NSID, ghqtl15)

# Wave 2 (Age 15) - Caseness
w2_caseness <- w2_raw %>%
  rowwise() %>%
  mutate(ghq15 = {
    items <- c_across(-NSID)
    if (all(items >= 1 & items <= 4)) {
      sum(ifelse(items >= 3, 1, 0))
    } else {
      NA_real_
    }
  }) %>%
  ungroup() %>%
  select(NSID, ghq15)

cat("Age 15 scores computed\n")

# Wave 4 (Age 17) - GHQ items
w4_raw <- all_data %>% select(NSID, W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, 
                               W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP, 
                               W4NoConfYP, W4WthlessYP, W4HappyYP)

# Recode missing values
w4_raw <- w4_raw %>% mutate(across(-NSID, recode_wave2_4_missing))

# Wave 4 (Age 17) - Likert sum
w4_likert <- w4_raw %>%
  rowwise() %>%
  mutate(ghqtl17 = {
    items <- c_across(-NSID)
    if (all(items >= 1 & items <= 4)) {
      sum(items)
    } else {
      NA_real_
    }
  }) %>%
  ungroup() %>%
  select(NSID, ghqtl17)

# Wave 4 (Age 17) - Caseness
w4_caseness <- w4_raw %>%
  rowwise() %>%
  mutate(ghq17 = {
    items <- c_across(-NSID)
    if (all(items >= 1 & items <= 4)) {
      sum(ifelse(items >= 3, 1, 0))
    } else {
      NA_real_
    }
  }) %>%
  ungroup() %>%
  select(NSID, ghq17)

cat("Age 17 scores computed\n")

# Wave 8 (Age 25) - GHQ items
w8_raw <- all_data %>% select(NSID, W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4, 
                               W8GHQ12_5, W8GHQ12_6, W8GHQ12_7, W8GHQ12_8, W8GHQ12_9, 
                               W8GHQ12_10, W8GHQ12_11, W8GHQ12_12)

# Recode missing values
w8_raw <- w8_raw %>% mutate(across(-NSID, recode_wave8_9_missing))

# Wave 8 (Age 25) - Likert sum
w8_likert <- w8_raw %>%
  rowwise() %>%
  mutate(ghqtl25 = {
    items <- c_across(-NSID)
    if (all(items >= 1 & items <= 4)) {
      sum(items)
    } else {
      NA_real_
    }
  }) %>%
  ungroup() %>%
  select(NSID, ghqtl25)

# Wave 8 (Age 25) - Caseness
w8_caseness <- w8_raw %>%
  rowwise() %>%
  mutate(ghq25 = {
    items <- c_across(-NSID)
    if (all(items >= 1 & items <= 4)) {
      sum(ifelse(items >= 3, 1, 0))
    } else {
      NA_real_
    }
  }) %>%
  ungroup() %>%
  select(NSID, ghq25)

cat("Age 25 scores computed\n")

# Wave 9 (Age 32) - GHQ items
w9_raw <- all_data %>% select(NSID, W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4, 
                               W9GHQ12_5, W9GHQ12_6, W9GHQ12_7, W9GHQ12_8, W9GHQ12_9, 
                               W9GHQ12_10, W9GHQ12_11, W9GHQ12_12)

# Recode missing values
w9_raw <- w9_raw %>% mutate(across(-NSID, recode_wave8_9_missing))

# Wave 9 (Age 32) - Likert sum
w9_likert <- w9_raw %>%
  rowwise() %>%
  mutate(ghqtl32 = {
    items <- c_across(-NSID)
    if (all(items >= 1 & items <= 4)) {
      sum(items)
    } else {
      NA_real_
    }
  }) %>%
  ungroup() %>%
  select(NSID, ghqtl32)

# Wave 9 (Age 32) - Caseness
w9_caseness <- w9_raw %>%
  rowwise() %>%
  mutate(ghq32 = {
    items <- c_across(-NSID)
    if (all(items >= 1 & items <= 4)) {
      sum(ifelse(items >= 3, 1, 0))
    } else {
      NA_real_
    }
  }) %>%
  ungroup() %>%
  select(NSID, ghq32)

cat("Age 32 scores computed\n")

# Combine all results
final_data <- w1 %>%
  left_join(w2_likert, by = "NSID") %>%
  left_join(w4_likert, by = "NSID") %>%
  left_join(w8_likert, by = "NSID") %>%
  left_join(w9_likert, by = "NSID") %>%
  left_join(w2_caseness, by = "NSID") %>%
  left_join(w4_caseness, by = "NSID") %>%
  left_join(w8_caseness, by = "NSID") %>%
  left_join(w9_caseness, by = "NSID")

# Convert NA to -3 for missing values
final_data <- final_data %>%
  mutate(
    ghqtl15 = ifelse(is.na(ghqtl15), -3, ghqtl15),
    ghqtl17 = ifelse(is.na(ghqtl17), -3, ghqtl17),
    ghqtl25 = ifelse(is.na(ghqtl25), -3, ghqtl25),
    ghqtl32 = ifelse(is.na(ghqtl32), -3, ghqtl32),
    ghq15 = ifelse(is.na(ghq15), -3, ghq15),
    ghq17 = ifelse(is.na(ghq17), -3, ghq17),
    ghq25 = ifelse(is.na(ghq25), -3, ghq25),
    ghq32 = ifelse(is.na(ghq32), -3, ghq32)
  )

# Set value labels for missing codes using attr
miss_labels <- c(`-3` = "Not asked at fieldwork stage / not interviewed", 
                 `-1` = "Item not applicable", 
                 `-8` = "Don't know / insufficient information", 
                 `-9` = "Refusal")

for (var in c("ghqtl15", "ghqtl17", "ghqtl25", "ghqtl32", "ghq15", "ghq17", "ghq25", "ghq32")) {
  attr(final_data[[var]], "labels") <- miss_labels
  attr(final_data[[var]], "label") <- paste0("GHQ-12 ", ifelse(grepl("ghqtl", var), "Likert Score", "Caseness Score"), " (Age ", substr(var, 5, 6), ")")
}

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Final data dimensions:", dim(final_data), "\n")
cat("Output columns:", names(final_data), "\n")

# Print summary statistics
cat("\nSummary statistics:\n")
cat("ghqtl15: mean=", round(mean(final_data$ghqtl15, na.rm=TRUE), 2), "\n")
cat("ghqtl17: mean=", round(mean(final_data$ghqtl17, na.rm=TRUE), 2), "\n")
cat("ghqtl25: mean=", round(mean(final_data$ghqtl25, na.rm=TRUE), 2), "\n")
cat("ghqtl32: mean=", round(mean(final_data$ghqtl32, na.rm=TRUE), 2), "\n")
cat("ghq15: mean=", round(mean(final_data$ghq15, na.rm=TRUE), 2), "\n")
cat("ghq17: mean=", round(mean(final_data$ghq17, na.rm=TRUE), 2), "\n")
cat("ghq25: mean=", round(mean(final_data$ghq25, na.rm=TRUE), 2), "\n")
cat("ghq32: mean=", round(mean(final_data$ghq32, na.rm=TRUE), 2), "\n")

library(dplyr)
library(readr)
library(tidyr)

# Create output directory if needed
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Load all files from metadata using readr::read_delim()
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8sc <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", show_col_types = FALSE)
w8d <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
w9m <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)
w9d <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all files by NSID
data <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w4, by = "NSID") %>%
  full_join(w8sc, by = "NSID") %>%
  full_join(w8d, by = "NSID") %>%
  full_join(w9m, by = "NSID") %>%
  full_join(w9d, by = "NSID")

cat("Data dimensions after merge:", dim(data), "\n")

# --- Helper function for item-summed Likert scores ---
# Dichotomize items: 1,2 -> 0; 3,4 -> 1
# If all 12 items are NA -> -3
# If any item has a negative value -> -8
# Otherwise -> sum of dichotomized items (valid range 0-12)
compute_ghq_sum <- function(items) {
  items_mat <- as.matrix(items)
  
  # Check if all items are NA
  all_na <- apply(items_mat, 1, function(x) all(is.na(x)))
  
  # Check if any item has a negative value (excluding NA)
  has_negative <- apply(items_mat, 1, function(x) any(x < 0, na.rm = TRUE))
  
  # Start with -3 for all
  result <- rep(-3, nrow(items_mat))
  
  # For rows with negative values but not all NA, assign -8
  neg_mask <- has_negative & !all_na
  result[neg_mask] <- -8
  
  # For rows with no negative values and not all NA, dichotomize and sum
  valid_mask <- !has_negative & !all_na
  if (any(valid_mask)) {
    valid_items <- items_mat[valid_mask, , drop = FALSE]
    # Dichotomize: 1,2 -> 0; 3,4 -> 1
    dichotomized <- ifelse(valid_items >= 3, 1, 0)
    result[valid_mask] <- rowSums(dichotomized)
  }
  
  return(result)
}

# --- Helper function to harmonise pre-derived caseness scores ---
harmonise_caseness <- function(x, wave) {
  result <- as.numeric(x)
  
  # Map by label meaning to standard codes
  if (wave == 2 || wave == 4) {
    # At waves 2 and 4, -97 and -92 both map to -9
    result[result == -97] <- -9
    result[result == -92] <- -9
  }
  
  # Other negative codes follow defaults
  result[result == -999] <- -2
  result[result == -998] <- -2
  result[result == -997] <- -2
  result[result == -995] <- -2
  result[result == -99] <- -3
  result[result == -96] <- -2
  result[result == -91] <- -1
  result[result == -1] <- -8
  
  return(result)
}

# --- Wave 2 (Age 15) GHQ items - extract from merged data ---
w2_items <- data %>%
  select(W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP,
         W2strainYP, W2difficYP, W2activYP, W2probsYP,
         W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP)

data$ghqtl15 <- compute_ghq_sum(as.matrix(w2_items))
data$ghq15 <- harmonise_caseness(data$W2ghq12scr, 2)

# --- Wave 4 (Age 17) GHQ items - extract from merged data ---
w4_items <- data %>%
  select(W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP,
         W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP,
         W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP)

data$ghqtl17 <- compute_ghq_sum(as.matrix(w4_items))
data$ghq17 <- harmonise_caseness(data$W4ghq12scr, 4)

# --- Wave 8 (Age 25) GHQ items - extract from merged data ---
w8_items <- data %>%
  select(W8GHQ12_1, W8GHQ12_2, W8GHQ12_3, W8GHQ12_4, W8GHQ12_5,
         W8GHQ12_6, W8GHQ12_7, W8GHQ12_8, W8GHQ12_9, W8GHQ12_10,
         W8GHQ12_11, W8GHQ12_12)

data$ghqtl25 <- compute_ghq_sum(as.matrix(w8_items))
data$ghq25 <- harmonise_caseness(data$W8DGHQSC, 8)

# --- Wave 9 (Age 32) GHQ items - extract from merged data ---
w9_items <- data %>%
  select(W9GHQ12_1, W9GHQ12_2, W9GHQ12_3, W9GHQ12_4, W9GHQ12_5,
         W9GHQ12_6, W9GHQ12_7, W9GHQ12_8, W9GHQ12_9, W9GHQ12_10,
         W9GHQ12_11, W9GHQ12_12)

data$ghqtl32 <- compute_ghq_sum(as.matrix(w9_items))
data$ghq32 <- harmonise_caseness(data$W9DGHQSC, 9)

# --- Keep only NSID and final derived variables ---
output <- data %>%
  select(NSID, ghqtl15, ghq15, ghqtl17, ghq17, ghqtl25, ghq25, ghqtl32, ghq32)

cat("Output dimensions:", dim(output), "\n")
cat("Variable names:", names(output), "\n")

# Write output
write_csv(output, "data/output/cleaned_data.csv")
cat("Output written to data/output/cleaned_data.csv\n")

# Print summary
cat("\nSummary of item-summed scores:\n")
for (v in c("ghqtl15", "ghqtl17", "ghqtl25", "ghqtl32")) {
  cat(v, ": min=", min(output[[v]], na.rm=TRUE), 
      " max=", max(output[[v]], na.rm=TRUE), 
      " mean=", round(mean(output[[v]], na.rm=TRUE), 2), "\n")
}

cat("\nSummary of caseness scores:\n")
for (v in c("ghq15", "ghq17", "ghq25", "ghq32")) {
  cat(v, ":\n")
  print(table(output[[v]], useNA = "ifany"))
}
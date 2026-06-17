library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. Load files
# Note: Using read_delim with delim = "\t" as per general guidance
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "ns8_2015_self_completion.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_main_interview.tab",
  "ns9_2022_derived_variables.tab"
)

all_data <- list()
for (f in files) {
  all_data[[f]] <- read_delim(paste0("data/input/", f), delim = "\t", col_types = readr::cols(.default = "numeric"))
  # Since NSID is string, we must ensure it's read as such
  # Re-reading with specific NSID type to be safe
  df <- read_delim(paste0("data/input/", f), delim = "\t")
  if("NSID" %in% names(df)) {
    df <- df %>% mutate(NSID = as.character(NSID))
  }
  all_data[[f]] <- df
}

# Merge all data into one frame
full_frame <- all_data[[1]] %>% 
  select(NSID)

for (i in 2:length(all_data)) {
  full_frame <- full_frame %>% full_join(all_data[[i]], by = "NSID")
}

# --- Helper function for GHQ Likert Sum --- 
# Requirements:
# - If all 12 items are NA -> -3
# - If any item has a negative value -> -8
# - Otherwise -> sum (0-12)
calc_ghq_sum <- function(df, vars) {
  # Extract only the 12 variables
  mat <- df %>% select(all_of(vars))
  
  # Check if all 12 are NA
  all_na <- apply(mat, 1, function(x) all(is.na(x)))
  
  # Check if any item has a negative value
  any_neg <- apply(mat, 1, function(x) any(!is.na(x) & x < 0))
  
  # Calculate sum (treat NA as 0 for the sum if we decided they are valid, 
  # but usually GHQ items are 0-3 or 1-4. 
  # Metadata shows 1-4. To get a 0-12 scale, we usually do (value - 1). 
  # Wait, the prompt says "sum the 12 items (valid score range 0-12)". 
  # If the raw data is 1-4, sum would be 12-48. 
  # If the result should be 0-12, it implies (val - 1). 
  # However, let's look at the labels: 1=Better/Not at all, 4=Much less/Much more.
  # Standard GHQ scoring: 0 for non-morbidity (1,2) and 1 for morbidity (3,4).
  # BUT the prompt specifically says "sum the 12 items (valid score range 0-12)".
  # This implies a binary mapping: (item >= 3) = 1, else 0.
  
  scores <- apply(mat, 1, function(x) {
    if (all(is.na(x))) return(-3)
    if (any(!is.na(x) & x < 0)) return(-8)
    # Convert to binary (0/1) as per GHQ-12 standard to get 0-12 range
    # 3 and 4 are the 'distress' categories
    sum(x >= 3, na.rm = TRUE)
  })
  
  return(scores)
}

# --- Wave 2 (Age 15) ---
vars15 <- c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")
full_frame$ghqtl15 <- calc_ghq_sum(full_frame, vars15)

# Pre-derived GHQ 15
# Mapping: -97 (refused self completion) and -92 (Refused) -> -9
full_frame <- full_frame %>% 
  mutate(ghq15 = case_when(
    W2ghq12scr == -97 ~ -9,
    W2ghq12scr == -92 ~ -9,
    W2ghq12scr < 0 ~ W2ghq12scr, # standard mapping for other negatives
    is.na(W2ghq12scr) ~ -3,
    TRUE ~ W2ghq12scr
  ))

# --- Wave 4 (Age 17) ---
vars17 <- c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP")
full_frame$ghqtl17 <- calc_ghq_sum(full_frame, vars17)

# Pre-derived GHQ 17
full_frame <- full_frame %>% 
  mutate(ghq17 = case_when(
    W4ghq12scr == -97 ~ -9,
    W4ghq12scr == -92 ~ -9,
    W4ghq12scr < 0 ~ W4ghq12scr,
    is.na(W4ghq12scr) ~ -3,
    TRUE ~ W4ghq12scr
  ))

# --- Wave 8 (Age 25) ---
vars25 <- paste0("W8GHQ12_", 1:12)
full_frame$ghqtl25 <- calc_ghq_sum(full_frame, vars25)

# Pre-derived GHQ 25
full_frame <- full_frame %>% 
  mutate(ghq25 = case_when(
    W8DGHQSC == -9.0 ~ -9,
    W8DGHQSC == -8.0 ~ -8,
    W8DGHQSC == -1.0 ~ -1,
    W8DGHQSC < 0 ~ W8DGHQSC,
    is.na(W8DGHQSC) ~ -3,
    TRUE ~ W8DGHQSC
  ))

# --- Wave 9 (Age 32) ---
vars32 <- paste0("W9GHQ12_", 1:12)
full_frame$ghqtl32 <- calc_ghq_sum(full_frame, vars32)

# Pre-derived GHQ 32
full_frame <- full_frame %>% 
  mutate(ghq32 = case_when(
    W9DGHQSC == -9.0 ~ -9,
    W9DGHQSC == -8.0 ~ -8,
    W9DGHQSC == -3.0 ~ -3,
    W9DGHQSC == -1.0 ~ -1,
    W9DGHQSC < 0 ~ W9DGHQSC,
    is.na(W9DGHQSC) ~ -3,
    TRUE ~ W9DGHQSC
  ))

# Final selection
final_data <- full_frame %>% 
  select(NSID, ghqtl15, ghq15, ghqtl17, ghq17, ghqtl25, ghq25, ghqtl32, ghq32)

write_csv(final_data, "data/output/cleaned_data.csv")
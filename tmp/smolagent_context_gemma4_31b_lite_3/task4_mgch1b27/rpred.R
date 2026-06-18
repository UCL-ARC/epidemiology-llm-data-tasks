library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Load datasets
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
wave8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', col_types = readr::cols(.default = 'c'))

# Merge datasets
data_merged <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave6, by = 'NSID') %>%
  full_join(wave7, by = 'NSID') %>%
  full_join(wave8, by = 'NSID') %>%
  full_join(wave9, by = 'NSID')

# Function to harmonize missing values and categories
harmonize_sori <- function(var_vec, labels_map) {
  # Convert to numeric
  val <- as.numeric(var_vec)
  
  # Map based on labels provided in metadata
  # Standard missing codes: 
  # -9 Refusal, -8 Don't know, -7 Prefer not to say, -3 Not asked, -2 Schedule NA/Error, -1 Not applicable
  
  res <- rep(NA, length(val))
  
  # We iterate through the unique values and apply the mapping based on the logic derived from metadata labels
  for (i in seq_along(val)) {
    v <- val[i]
    if (is.na(v)) {
      res[i] <- -3
      next
    }
    
    # This part depends on the specific wave mapping
    # Since the function is generic, we handle the mapping externally or via a passed map
    # But for this specific task, we will apply the logic per variable in the main block.
  }
  return(res)
}

# --- Wave 6 (Age 19) ---
# W6SexualityYP: 
# -97 Refused self completion -> -9 (Refusal)
# -92 Refused -> -9
# -91 Not applicable -> -1
# -1 Don't know -> -8
# 1-4 Substantive
sori19 <- data_merged %>%
  mutate(sori19 = case_when(
    W6SexualityYP == "1" ~ 1,
    W6SexualityYP == "2" ~ 2,
    W6SexualityYP == "3" ~ 3,
    W6SexualityYP == "4" ~ 4,
    W6SexualityYP == "-97" ~ -9,
    W6SexualityYP == "-92" ~ -9,
    W6SexualityYP == "-91" ~ -1,
    W6SexualityYP == "-1" ~ -8,
    is.na(W6SexualityYP) ~ -3,
    TRUE ~ -3
  )) %>%
  pull(sori19)

# --- Wave 7 (Age 20) ---
# W7SexualityYP:
# -100 Respondent declined sexual experience questions -> -9 (Refusal)
# -97 Refused self completion -> -9
# -92 Refused -> -9
# -91 Not applicable -> -1
# -1 Don't know -> -8
# 1-4 Substantive
sori20 <- data_merged %>%
  mutate(sori20 = case_when(
    W7SexualityYP == "1" ~ 1,
    W7SexualityYP == "2" ~ 2,
    W7SexualityYP == "3" ~ 3,
    W7SexualityYP == "4" ~ 4,
    W7SexualityYP == "-100" ~ -9,
    W7SexualityYP == "-97" ~ -9,
    W7SexualityYP == "-92" ~ -9,
    W7SexualityYP == "-91" ~ -1,
    W7SexualityYP == "-1" ~ -8,
    is.na(W7SexualityYP) ~ -3,
    TRUE ~ -3
  )) %>%
  pull(sori20)

# --- Wave 8 (Age 25) ---
# W8SEXUALITY:
# -9 Refused -> -9
# -8 Don't know -> -8
# -1 Not applicable -> -1
# 1-4 Substantive
sori25 <- data_merged %>%
  mutate(sori25 = case_when(
    W8SEXUALITY == "1" ~ 1,
    W8SEXUALITY == "2" ~ 2,
    W8SEXUALITY == "3" ~ 3,
    W8SEXUALITY == "4" ~ 4,
    W8SEXUALITY == "-9" ~ -9,
    W8SEXUALITY == "-8" ~ -8,
    W8SEXUALITY == "-1" ~ -1,
    is.na(W8SEXUALITY) ~ -3,
    TRUE ~ -3
  )) %>%
  pull(sori25)

# --- Wave 9 (Age 32) ---
# W9SORI:
# -9 Refused -> -9
# -8 Don't know -> -8
# -3 Not asked -> -3
# -1 Not applicable -> -1
# 1-4 Substantive
# 5 Prefer not to say -> -7
sori32 <- data_merged %>%
  mutate(sori32 = case_when(
    W9SORI == "1" ~ 1,
    W9SORI == "2" ~ 2,
    W9SORI == "3" ~ 3,
    W9SORI == "4" ~ 4,
    W9SORI == "-9" ~ -9,
    W9SORI == "-8" ~ -8,
    W9SORI == "-3" ~ -3,
    W9SORI == "-1" ~ -1,
    W9SORI == "5" ~ -7,
    is.na(W9SORI) ~ -3,
    TRUE ~ -3
  )) %>%
  pull(sori32)

# Create final dataframe
final_df <- data.frame(NSID = data_merged$NSID, sori19, sori20, sori25, sori32)

# Apply factor labels
sori_labels <- c("1" = "Heterosexual / Straight", "2" = "Gay / Lesbian", "3" = "Bisexual", "4" = "Other", 
                 "-9" = "Refusal", "-8" = "Don't know", "-7" = "Prefer not to say", "-3" = "Not asked", 
                 "-2" = "Schedule not applicable", "-1" = "Not applicable")

# Convert to factors and apply labels
# Since we need labels for the final output, we'll use the labelled package or factor
final_df <- final_df %>%
  mutate(across(starts_with("sori"), ~ factor(.x, levels = as.numeric(names(sori_labels)), labels = sori_labels)))

# Write to CSV
write_csv(final_df, 'data/output/cleaned_data.csv')
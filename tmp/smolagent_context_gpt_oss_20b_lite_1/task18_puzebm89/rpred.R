# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Helper: Recode missing values according to a mapping
recode_missing <- function(x, mapping) {
  vals <- as.character(x)
  out <- x
  for (i in seq_along(mapping)) {
    old_val <- names(mapping)[i]
    new_val <- mapping[i]
    out[vals == old_val] <- new_val
  }
  out
}

# Load all input files
wave_one   <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols())
wave_two   <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", col_types = cols())
wave_four   <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols())
ns8_self   <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", col_types = cols())
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = cols())
ns9_main   <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", col_types = cols())
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = cols())

# Merge all data frames by NSID (full join)
merged_df <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8_self, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

# Recode missing values for summary GHQ-12 scores
merged_df <- merged_df %>%
  mutate(
    W2ghq12scr_rec = recode_missing(W2ghq12scr, mapping = c("-99" = -3, "-97" = -9, "-96" = -8, "-92" = -9, "-91" = -1, "-1" = -8)),
    W4ghq12scr_rec = recode_missing(W4ghq12scr, mapping = c("-99" = -3, "-97" = -9, "-96" = -8, "-92" = -9, "-91" = -1, "-1" = -8)),
    W8DGHQSC_rec  = recode_missing(W8DGHQSC, mapping = c("-9" = -9, "-8" = -8, "-1" = -1)),
    W9DGHQSC_rec  = recode_missing(W9DGHQSC, mapping = c("-9" = -9, "-8" = -8, "-3" = -3, "-1" = -1))
  )

# Caseness variables (>=4 is case)
merged_df <- merged_df %>%
  mutate(
    ghq15 = case_when(
      is.na(W2ghq12scr_rec) ~ NA_real_,
      W2ghq12scr_rec >= 4 ~ 1,
      TRUE ~ 0
    ),
    ghq17 = case_when(
      is.na(W4ghq12scr_rec) ~ NA_real_,
      W4ghq12scr_rec >= 4 ~ 1,
      TRUE ~ 0
    ),
    ghq25 = case_when(
      is.na(W8DGHQSC_rec) ~ NA_real_,
      W8DGHQSC_rec >= 4 ~ 1,
      TRUE ~ 0
    ),
    ghq32 = case_when(
      is.na(W9DGHQSC_rec) ~ NA_real_,
      W9DGHQSC_rec >= 4 ~ 1,
      TRUE ~ 0
    )
  )

# Recode missing for GHQ-12 items (waves 8 & 9)
item_cols_w8 <- paste0("W8GHQ12_", 1:12)
item_cols_w9 <- paste0("W9GHQ12_", 1:12)

recode_items <- function(df, cols, mapping) {
  df %>% mutate(across(all_of(cols), ~ recode_missing(.x, mapping)))
}

merged_df <- merged_df %>%
  recode_items(item_cols_w8, mapping = c("-9" = -9, "-8" = -8, "-1" = -1)) %>%
  recode_items(item_cols_w9, mapping = c("-9" = -9, "-8" = -8, "-3" = -3, "-1" = -1))

# Compute item-summed GHQ-12 scores if all items present
merged_df <- merged_df %>%
  mutate(
    ghqtl25 = rowSums(across(all_of(item_cols_w8)), na.rm = FALSE),
    ghqtl32 = rowSums(across(all_of(item_cols_w9)), na.rm = FALSE)
  )
# ghqtl15 & ghqtl17 cannot be computed from available item data
merged_df <- merged_df %>% mutate(ghqtl15 = NA_real_, ghqtl17 = NA_real_)

# Convert NAs in derived variables to standard missing code -3
derived_vars <- c("ghq15", "ghq17", "ghq25", "ghq32", "ghqtl15", "ghqtl17", "ghqtl25", "ghqtl32")
for (var in derived_vars) {
  merged_df[[var]] <- ifelse(is.na(merged_df[[var]]), -3, merged_df[[var]])
}

# Keep only required columns and write output CSV
output_df <- merged_df %>% select(NSID, all_of(derived_vars))
write_csv(output_df, "data/output/cleaned_data.csv")

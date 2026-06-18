# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Define file names
file_names <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "ns8_2015_self_completion.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_main_interview.tab",
  "ns9_2022_derived_variables.tab"
)

# Function to read a single file (tab delimited, read all as character)
read_file <- function(fname) {
  read_delim(file.path("data", "input", fname), delim = "\t", col_types = cols(.default = "c"))
}

# Read all files
raw_dfs <- lapply(file_names, read_file)
names(raw_dfs) <- file_names

# Merge all by NSID
merged_df <- reduce(raw_dfs, ~ full_join(.x, .y, by = "NSID"))

# Convert relevant GHQ total columns to numeric
merged_df <- merged_df %>%
  mutate(
    W2ghq12scr = as.numeric(W2ghq12scr),
    W4ghq12scr = as.numeric(W4ghq12scr),
    W8DGHQSC   = as.numeric(W8DGHQSC),
    W9DGHQSC   = as.numeric(W9DGHQSC)
  )

# Harmonise pre‑derived totals for waves 2 & 4
harmonise_total_2_4 <- function(x) {
  case_when(
    x %in% c(-97, -92) ~ -9,   # Refusal or YP refused self completion
    x == -99 ~ -3,              # Not asked at fieldwork stage
    x == -96 ~ -2,              # Using interpreter
    is.na(x) ~ x,
    TRUE ~ x
  )
}

# Compute caseness columns
merged_df <- merged_df %>%
  mutate(
    ghq15 = harmonise_total_2_4(W2ghq12scr),
    ghq17 = harmonise_total_2_4(W4ghq12scr),
    ghq25 = W8DGHQSC,
    ghq32 = W9DGHQSC
  )

# Helper to compute item‑summed GHQ12 score
compute_item_sum <- function(df, prefix, start, end) {
  item_names <- paste0(prefix, "_", start:end)
  items_mat <- df %>% select(all_of(item_names)) %>% as.matrix()
  items_num <- apply(items_mat, 2, function(x) as.numeric(as.character(x)))
  all_na_row <- apply(items_num, 1, function(r) all(is.na(r)))
  any_neg_row <- apply(items_num, 1, function(r) any(r < 0, na.rm = TRUE))
  sum_rows <- rowSums(items_num, na.rm = TRUE)
  out <- ifelse(all_na_row, -3, ifelse(any_neg_row, -8, sum_rows))
  return(out)
}

# Wave 25 (age 25) GHQ items
merged_df <- merged_df %>%
  mutate(ghqtl25 = compute_item_sum(., "W8GHQ12", 1, 12))

# Wave 32 (age 32) GHQ items
merged_df <- merged_df %>%
  mutate(ghqtl32 = compute_item_sum(., "W9GHQ12", 1, 12))

# Ages 15 and 17: no item data, assign -3 (did not participate)
merged_df <- merged_df %>%
  mutate(
    ghqtl15 = -3,
    ghqtl17 = -3
  )

# Final variable selection
final_df <- merged_df %>%
  select(NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32)

# Write CSV
write_csv(final_df, file.path("data", "output", "cleaned_data.csv"))

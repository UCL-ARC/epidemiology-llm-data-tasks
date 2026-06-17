# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# Helper to read a .tab file with NSID as character
read_tab <- function(file) {
  read_delim(file, delim = "\t",
             col_types = cols(NSID = col_character(), .default = col_double()),
             show_col_types = FALSE)
}

# Paths
base_dir <- "data/input/"

# Load files
wave1    <- read_tab(paste0(base_dir, "wave_one_lsype_young_person_2020.tab"))
wave2    <- read_tab(paste0(base_dir, "wave_two_lsype_young_person_2020.tab"))
wave4    <- read_tab(paste0(base_dir, "wave_four_lsype_young_person_2020.tab"))

ns8_self  <- read_tab(paste0(base_dir, "ns8_2015_self_completion.tab"))
ns8_derived <- read_tab(paste0(base_dir, "ns8_2015_derived.tab"))

ns9_main  <- read_tab(paste0(base_dir, "ns9_2022_main_interview.tab"))
ns9_derived <- read_tab(paste0(base_dir, "ns9_2022_derived_variables.tab"))

# Merge all data frames by NSID (full joins)
merged <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(., wave4, by = "NSID") %>%
  full_join(., ns8_self, by = "NSID") %>%
  full_join(., ns8_derived, by = "NSID") %>%
  full_join(., ns9_main, by = "NSID") %>%
  full_join(., ns9_derived, by = "NSID")

# Recoding missing for GHQ totals at age 15 and 17
recode_wave2_4 <- function(x) {
  ifelse(x %in% c(-97, -92), -9,
         ifelse(x == -99, -3,
                ifelse(x == -96, -8,
                       ifelse(x %in% c(-998, -997, -995), -2,
                              ifelse(x == -91, -1, x)))))
}

# GHQ totals
merged <- merged %>%
  mutate(ghq15_raw = recode_wave2_4(W2ghq12scr),
         ghq17_raw = recode_wave2_4(W4ghq12scr),
         ghq25_raw = W8DGHQSC,
         ghq32_raw = W9DGHQSC)

# GHQ caseness variables (same as raw totals after recoding)
merged <- merged %>%
  mutate(ghq15 = ghq15_raw,
         ghq17 = ghq17_raw,
         ghq25 = ghq25_raw,
         ghq32 = ghq32_raw)

# GHQ item-summed variables for ages 25 and 32
items_25 <- paste0("W8GHQ12_", 1:12)
items_32 <- paste0("W9GHQ12_", 1:12)

# Function to compute item-summed score
compute_item_sum <- function(df, items) {
  vals <- df[ , items, drop = FALSE]
  has_neg <- apply(vals, 1, function(row) any(row < 0, na.rm = TRUE))
  all_na  <- apply(vals, 1, function(row) all(is.na(row)))
  sum_vals <- rowSums(vals, na.rm = TRUE)
  result <- ifelse(all_na, -3,
                   ifelse(has_neg, -8,
                          sum_vals))
  return(result)
}

merged <- merged %>%
  mutate(ghqtl25 = compute_item_sum(., items_25),
         ghqtl32 = compute_item_sum(., items_32))

# For ages 15 and 17, use the pre-derived totals as item‑summed scores
merged <- merged %>%
  mutate(ghqtl15 = ghq15_raw,
         ghqtl17 = ghq17_raw)

# Select final variables
final_df <- merged %>%
  select(NSID,
         ghqtl15, ghq15,
         ghqtl17, ghq17,
         ghqtl25, ghq25,
         ghqtl32, ghq32)

# Write output
write_csv(final_df, "data/output/cleaned_data.csv")

print("Cleaning complete: output written to data/output/cleaned_data.csv")
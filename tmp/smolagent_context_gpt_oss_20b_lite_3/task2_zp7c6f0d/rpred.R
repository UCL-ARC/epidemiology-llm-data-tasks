library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# Function to map missing codes to NA for a given vector and mapping


def_missing_to_na <- function(vec, missing_vals){
  vec %>% replace(vec %in% missing_vals, NA_real_)
}

# Load datasets
# Wave 1
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim="\t", show_col_types = FALSE) %>%
  select(NSID, w1_eth = W1ethnic2YP)

# Wave 2
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim="\t", show_col_types = FALSE) %>%
  select(NSID, w2_eth = W2ethnicYP)

# Wave 4
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim="\t", show_col_types = FALSE) %>%
  select(NSID, w4_eth = w4ethnic2YP)

# Wave 8 (derived)
w8 <- read_delim("data/input/ns8_2015_derived.tab", delim="\t", show_col_types = FALSE) %>%
  select(NSID, w8_eth = W8DETHN15)

# Wave 9 (derived)
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim="\t", show_col_types = FALSE) %>%
  select(NSID, w9_eth = W9DETHN15)

# Merge all by NSID (full join to keep all participants)
merged <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w4, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9, by = "NSID")

# Convert missing codes to NA for each wave
merged <- merged %>%
  mutate(
    w1_eth = def_missing_to_na(w1_eth, c(-999, -94, -92, -91, -1)),
    w2_eth = def_missing_to_na(w2_eth, c(-998, -997, -995, -99, -92, -91, -1)),
    w4_eth = def_missing_to_na(w4_eth, c(-94, -1)),
    w8_eth = def_missing_to_na(w8_eth, c(-9, -8, -1)),
    w9_eth = def_missing_to_na(w9_eth, c(-8))
  )

# Earliest valid ethnicity
merged <- merged %>%
  mutate(eth = coalesce(w1_eth, w2_eth, w4_eth, w8_eth, w9_eth))

# Map remaining NA to standard missing code -9 (refusal)
merged <- merged %>%
  mutate(eth = replace_na(eth, -9))

# Keep only NSID and eth
final_df <- merged %>% select(NSID, eth)

# Write to CSV
write_csv(final_df, "data/output/cleaned_data.csv", na = "")

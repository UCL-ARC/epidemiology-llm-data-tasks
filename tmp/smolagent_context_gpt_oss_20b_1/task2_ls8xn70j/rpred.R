library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Function to convert source missing codes to NA
convert_missing <- function(x) {
  replace_vals <- c(-999.0, -94.0, -92.0, -91.0, -1.0,
                    -998.0, -997.0, -995.0, -99.0,
                    -9.0, -8.0)
  case_when(
    x %in% 1:16 ~ x,
    x %in% replace_vals ~ NA_real_,
    TRUE ~ NA_real_
  )
}

# Read files
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols())
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", col_types = cols())
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols())
w8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = cols())
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = cols())

# Select relevant columns
w1_sel <- w1 %>% select(NSID, W1ethnic2YP)
w2_sel <- w2 %>% select(NSID, W2ethnicYP)
w4_sel <- w4 %>% select(NSID, w4ethnic2YP)
w8_sel <- w8 %>% select(NSID, W8DETHN15)
w9_sel <- w9 %>% select(NSID, W9DETHN15)

# Merge all datasets by NSID
merged <- w1_sel %>%
  full_join(w2_sel, by = "NSID") %>%
  full_join(w4_sel, by = "NSID") %>%
  full_join(w8_sel, by = "NSID") %>%
  full_join(w9_sel, by = "NSID")

# Convert source missing codes to NA for ethnicity variables
merged <- merged %>%
  mutate(
    eth1 = convert_missing(W1ethnic2YP),
    eth2 = convert_missing(W2ethnicYP),
    eth4 = convert_missing(w4ethnic2YP),
    eth8 = convert_missing(W8DETHN15),
    eth9 = convert_missing(W9DETHN15)
  )

# Derive consolidated ethnicity variable (eth) using earliest valid response order
merged <- merged %>%
  mutate(
    eth = case_when(
      !is.na(eth1) ~ eth1,
      !is.na(eth2) ~ eth2,
      !is.na(eth4) ~ eth4,
      !is.na(eth8) ~ eth8,
      !is.na(eth9) ~ eth9,
      TRUE ~ NA_real_
    ),
    eth = replace_na(eth, -3)
  ) %>%
  select(NSID, eth)

# Write output CSV
write_csv(merged, "data/output/cleaned_data.csv")
# Load required libraries
library(readr)
library(dplyr)
library(labelled)

# Helper function to standardise missing codes
standardise_eth <- function(x, missing_vals){
  x_num <- as.numeric(x)
  if(length(missing_vals) > 0){
    x_num[match(x_num, missing_vals)] <- NA
  }
  return(x_num)
}

# File paths
path1 <- "data/input/wave_one_lsype_young_person_2020.tab"
path2 <- "data/input/wave_two_lsype_young_person_2020.tab"
path4 <- "data/input/wave_four_lsype_young_person_2020.tab"
path8 <- "data/input/ns8_2015_derived.tab"
path9 <- "data/input/ns9_2022_derived_variables.tab"

# Read files (tab delimited). Only load columns of interest for memory efficiency
# Use read_delim and then select relevant columns
w1 <- read_delim(path1, delim = "\t", col_types = cols(), progress = FALSE)
w2 <- read_delim(path2, delim = "\t", col_types = cols(), progress = FALSE)
w4 <- read_delim(path4, delim = "\t", col_types = cols(), progress = FALSE)
w8 <- read_delim(path8, delim = "\t", col_types = cols(), progress = FALSE)
w9 <- read_delim(path9, delim = "\t", col_types = cols(), progress = FALSE)

# Extract and standardise ethnicity columns
w1_eth <- w1 %>% select(NSID, W1ethnic2YP) %>% mutate(
  W1ethnic2YP_std = standardise_eth(W1ethnic2YP, c(-999, -94, -92, -91, -1))
)

w2_eth <- w2 %>% select(NSID, W2ethnicYP) %>% mutate(
  W2ethnicYP_std = standardise_eth(W2ethnicYP, c(-998, -997, -995, -99, -92, -91, -1))
)

w4_eth <- w4 %>% select(NSID, w4ethnic2YP) %>% mutate(
  w4ethnic2YP_std = standardise_eth(w4ethnic2YP, c(-94, -1))
)

w8_eth <- w8 %>% select(NSID, W8DETHN15) %>% mutate(
  W8DETHN15_std = standardise_eth(W8DETHN15, c(-9, -8, -1))
)

w9_eth <- w9 %>% select(NSID, W9DETHN15) %>% mutate(
  W9DETHN15_std = standardise_eth(W9DETHN15, c(-8))
)

# Merge all data frames by NSID
merged <- w1_eth %>%
  full_join(w2_eth, by = "NSID") %>%
  full_join(w4_eth, by = "NSID") %>%
  full_join(w8_eth, by = "NSID") %>%
  full_join(w9_eth, by = "NSID")

# Create consolidated ethnicity variable 'eth'
merged <- merged %>%
  mutate(eth = coalesce(W1ethnic2YP_std, W2ethnicYP_std, w4ethnic2YP_std, W8DETHN15_std, W9DETHN15_std)) %>%
  mutate(eth = ifelse(is.na(eth), -3, eth))

# Select final columns
final_df <- merged %>% select(NSID, eth)

# Write output CSV
write_csv(final_df, "data/output/cleaned_data.csv")

# End of script
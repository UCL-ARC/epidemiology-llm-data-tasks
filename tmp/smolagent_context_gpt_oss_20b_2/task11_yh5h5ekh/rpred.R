library(readr)
library(dplyr)

# Function to recode missing values according to specification
recode_missing <- function(x) {
  x <- as.numeric(x)
  # Map specific codes
  x <- case_when(
    is.na(x) ~ -3,
    x == -99 ~ -3,
    x == -98 ~ -3,
    x == -996 ~ -3,
    x == -94 ~ -8,   # insufficient information
    x == -92 ~ -9,   # refusal
    TRUE ~ x
  )
  return(x)
}

# Read Wave 1 (Age 14)
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab",
                     delim = "\t", show_col_types = FALSE) %>%
  select(NSID, W1empsmum, W1empsdad) %>%
  rename(ecoactma14 = W1empsmum, ecoactpa14 = W1empsdad) %>%
  mutate(across(starts_with("ecoact"), recode_missing))

# Read Wave 2 (Age 15)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab",
                     delim = "\t", show_col_types = FALSE) %>%
  select(NSID, W2empsmum, W2empsdad) %>%
  rename(ecoactma15 = W2empsmum, ecoactpa15 = W2empsdad) %>%
  mutate(across(starts_with("ecoact"), recode_missing))

# Read Wave 3 (Age 16)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab",
                     delim = "\t", show_col_types = FALSE) %>%
  select(NSID, W3empsmum, W3empsdad) %>%
  rename(ecoactma16 = W3empsmum, ecoactpa16 = W3empsdad) %>%
  mutate(across(starts_with("ecoact"), recode_missing))

# Read Wave 4 (Age 17)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab",
                     delim = "\t", show_col_types = FALSE) %>%
  select(NSID, w4empsmum, w4empsdad) %>%
  rename(ecoactma17 = w4empsmum, ecoactpa17 = w4empsdad) %>%
  mutate(across(starts_with("ecoact"), recode_missing))

# Merge all waves
full_df <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Select only required columns
output_df <- full_df %>%
  select(NSID,
         ecoactma14, ecoactpa14,
         ecoactma15, ecoactpa15,
         ecoactma16, ecoactpa16,
         ecoactma17, ecoactpa17)

# Write to CSV
write_csv(output_df, "data/output/cleaned_data.csv")
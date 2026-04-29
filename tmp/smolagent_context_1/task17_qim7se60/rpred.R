library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load files and select only needed columns to avoid name collisions
file1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols(NSID = "c")) %>% select(NSID)
file2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = cols(NSID = "c", IMDRSCORE = "d")) %>% select(NSID, IMDRSCORE)
file3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = cols(NSID = "c", IMDRSCORE = "d")) %>% select(NSID, IMDRSCORE)
file4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols(NSID = "c")) %>% select(NSID)
file9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = cols(NSID = "c", W9DIMDD = "d")) %>% select(NSID, W9DIMDD)

# Rename source variables before joining to keep them distinct
file2 <- file2 %>% rename(IMDRSCORE_15 = IMDRSCORE)
file3 <- file3 %>% rename(IMDRSCORE_16 = IMDRSCORE)

# Merge datasets
merged_df <- file1 %>%
  full_join(file2, by = "NSID") %>%
  full_join(file3, by = "NSID") %>%
  full_join(file4, by = "NSID") %>%
  full_join(file9, by = "NSID")

# Processing IMD variables
# Wave 2 (Age 15)
merged_df <- merged_df %>%
  mutate(imd15 = case_when(
    is.na(IMDRSCORE_15) ~ -3,
    IMDRSCORE_15 == -94.0 ~ -8,
    IMDRSCORE_15 >= -999.0 & IMDRSCORE_15 <= -1.0 ~ -2,
    TRUE ~ IMDRSCORE_15
  ))

# Wave 3 (Age 16)
merged_df <- merged_df %>%
  mutate(imd16 = case_when(
    is.na(IMDRSCORE_16) ~ -3,
    IMDRSCORE_16 == -94.0 ~ -8,
    IMDRSCORE_16 >= -999.0 & IMDRSCORE_16 <= -1.0 ~ -2,
    TRUE ~ IMDRSCORE_16
  ))

# Wave 9 (Age 32)
merged_df <- merged_df %>%
  mutate(imd32 = case_when(
    is.na(W9DIMDD) ~ -3,
    W9DIMDD == -8.0 ~ -8,
    TRUE ~ W9DIMDD
  ))

# Create labels for categorical variable imd32
merged_df <- merged_df %>%
  mutate(imd32 = factor(imd32, 
                        levels = c(1:10, -8, -3), 
                        labels = c(paste("Decile", 1:10), "Don't know / insufficient information", "Not asked at the fieldwork stage / not interviewed")))

# Final selection
final_df <- merged_df %>% select(NSID, imd15, imd16, imd32)

# Write to CSV
write_csv(final_df, "data/output/cleaned_data.csv")
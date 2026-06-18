library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files with explicit names as requested
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols(.default = "c"))

# Select relevant variables first to avoid naming collisions during merge
wave2_sub <- wave2 %>% select(NSID, IMDRSCORE_15 = IMDRSCORE)
wave3_sub <- wave3 %>% select(NSID, IMDRSCORE_16 = IMDRSCORE)
wave9_sub <- wave9 %>% select(NSID, W9DIMDD)

# Merge all files to preserve full cohort frame
final_df <- wave1 %>%
  full_join(wave2_sub, by = "NSID") %>%
  full_join(wave3_sub, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave9_sub, by = "NSID")

# Process imd15 (Wave 2)
# Metadata: IMDRSCORE, -94.0 = Insufficient Information, -999.0 thru -1.0 = missing
final_df <- final_df %>%
  mutate(
    imd15 = as.numeric(IMDRSCORE_15),
    imd15 = case_when(
      is.na(imd15) ~ -3,
      imd15 == -94 ~ -8,
      imd15 <= -1 ~ -2,
      TRUE ~ imd15
    )
  )

# Process imd16 (Wave 3)
# Metadata: IMDRSCORE, -94.0 = Insufficient Information, -999.0 thru -1.0 = missing
final_df <- final_df %>%
  mutate(
    imd16 = as.numeric(IMDRSCORE_16),
    imd16 = case_when(
      is.na(imd16) ~ -3,
      imd16 == -94 ~ -8,
      imd16 <= -1 ~ -2,
      TRUE ~ imd16
    )
  )

# Process imd32 (Wave 9)
# Metadata: W9DIMDD, -8.0 = Insufficient information
final_df <- final_df %>%
  mutate(
    imd32 = as.numeric(W9DIMDD),
    imd32 = case_when(
      is.na(imd32) ~ -3,
      imd32 == -8 ~ -8,
      TRUE ~ imd32
    )
  )

# IMD 32 is nominal (deciles), apply factor labels
final_df$imd32 <- factor(final_df$imd32, 
                        levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, -8, -3),
                        labels = c("Most deprived decile", "2", "3", "4", "5", "6", "7", "8", "9", "Least deprived decile", "Insufficient information", "Not asked/NA"))

# Keep only ID and final derived variables
output_df <- final_df %>% select(NSID, imd15, imd16, imd32)

# Write to CSV
write_csv(output_df, "data/output/cleaned_data.csv")
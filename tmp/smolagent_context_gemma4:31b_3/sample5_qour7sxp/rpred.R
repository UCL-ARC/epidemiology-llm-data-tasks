library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Using read_delim without col_types shortcut to avoid the 'Unknown shortcut' error
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")

# Merge datasets
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(x, mapping) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  for (old_val in names(mapping)) {
    x[x == as.numeric(old_val)] <- mapping[[old_val]]
  }
  return(x)
}

# 4 & 5. Marital Status Harmonization (Time-Varying)
# Wave 6: W6MarStatYP (Age 19)
map6 <- list("-997" = -2, "-97" = -7, "-92" = -9, "-91" = -1, "-1" = -8)
merged_data$partnr19_detailed <- map_missing(merged_data$W6MarStatYP, map6)

# Wave 8: W8DMARSTAT (Age 25 approx)
map8 <- list("-9" = -9, "-8" = -8, "-1" = -1)
merged_data$partnr25_detailed <- map_missing(merged_data$W8DMARSTAT, map8)

# Wave 9: W9DMARSTAT (Age 32)
map9 <- list("-9" = -9, "-8" = -8)
merged_data$partnr32_detailed <- map_missing(merged_data$W9DMARSTAT, map9)

# Harmonization logic for 'partnr' (Collapsed)
merged_data <- merged_data %>%
  mutate(partnr19 = case_when(
    partnr19_detailed == 1 ~ 1, 
    partnr19_detailed == 2 ~ 2, 
    partnr19_detailed == 3 ~ 3, 
    partnr19_detailed == 4 ~ 4, 
    partnr19_detailed == 5 ~ 5, 
    partnr19_detailed < 0 ~ partnr19_detailed, 
    TRUE ~ -3
  )) %>%
  mutate(partnr25 = case_when(
    partnr25_detailed == 1 ~ 1, 
    partnr25_detailed == 2 ~ 2, 
    partnr25_detailed == 3 ~ 3, 
    partnr25_detailed == 4 ~ 4, 
    partnr25_detailed == 5 ~ 5, 
    partnr25_detailed == 6 ~ 6, 
    partnr25_detailed == 7 ~ 3, 
    partnr25_detailed == 8 ~ 4, 
    partnr25_detailed == 9 ~ 5, 
    partnr25_detailed < 0 ~ partnr25_detailed, 
    TRUE ~ -3
  )) %>%
  mutate(partnr32 = case_when(
    partnr32_detailed == 1 ~ 1, 
    partnr32_detailed == 2 ~ 2, 
    partnr32_detailed == 3 ~ 4, 
    partnr32_detailed == 4 ~ 3, 
    partnr32_detailed == 5 ~ 5, 
    partnr32_detailed == 6 ~ 6, 
    partnr32_detailed == 7 ~ 4, 
    partnr32_detailed == 8 ~ 5, 
    partnr32_detailed < 0 ~ partnr32_detailed,
    TRUE ~ -3
  ))

# Final selection: NSID and derived variables
final_df <- merged_data %>%
  select(NSID, partnr19, partnr19_detailed, partnr25, partnr25_detailed, partnr32, partnr32_detailed)

write_csv(final_df, "data/output/cleaned_data.csv")

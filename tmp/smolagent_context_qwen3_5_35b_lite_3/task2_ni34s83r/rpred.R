library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", 
                     delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", 
                     delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", 
                     delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", 
                     delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", 
                     delim = "\t", show_col_types = FALSE)

# Merge all files by NSID using full_join
df <- full_join(wave1, wave2, by = "NSID")
df <- full_join(df, wave4, by = "NSID")
df <- full_join(df, wave8, by = "NSID")
df <- full_join(df, wave9, by = "NSID")

# Function to harmonize ethnicity codes for each wave
harmonize_ethnicity <- function(var, wave_name) {
  # Convert to numeric
  var <- as.numeric(var)
  
  # Create new variable with harmonized codes
  eth <- var
  
  if (wave_name == "wave1") {
    # W1ethnic2YP: -999->-2, -94->-8, -92->-9, -91->-1, -1->-8
    eth[!is.na(eth)] <- recode(eth[!is.na(eth)],
      `1` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = 6, `7` = 7,
      `8` = 8, `9` = 9, `10` = 10, `11` = 11, `12` = 12, `13` = 13,
      `14` = 14, `15` = 15, `16` = 16,
      `-999` = -2, `-94` = -8, `-92` = -9, `-91` = -1, `-1` = -8
    )
  } else if (wave_name == "wave2") {
    # W2ethnicYP: -998->-2, -997->-2, -995->-2, -99->-3, -92->-9, -91->-1, -1->-8
    eth[!is.na(eth)] <- recode(eth[!is.na(eth)],
      `1` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = 6, `7` = 7,
      `8` = 8, `9` = 9, `10` = 10, `11` = 11, `12` = 12, `13` = 13,
      `14` = 14, `15` = 15, `16` = 16,
      `-998` = -2, `-997` = -2, `-995` = -2, `-99` = -3, `-92` = -9, `-91` = -1, `-1` = -8
    )
  } else if (wave_name == "wave4") {
    # w4ethnic2YP: -94->-8, -1->-8
    eth[!is.na(eth)] <- recode(eth[!is.na(eth)],
      `1` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = 6, `7` = 7,
      `8` = 8, `9` = 9, `10` = 10, `11` = 11, `12` = 12, `13` = 13,
      `14` = 14, `15` = 15, `16` = 16,
      `-94` = -8, `-1` = -8
    )
  } else if (wave_name == "wave8") {
    # W8DETHN15: -9->-9, -8->-8, -1->-1
    # Already uses standard codes mostly, just need to check
    eth[!is.na(eth)] <- recode(eth[!is.na(eth)],
      `1` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = 6, `7` = 7,
      `8` = 8, `9` = 9, `10` = 10, `11` = 11, `12` = 12, `13` = 13,
      `14` = 14, `15` = 15, `16` = 16,
      `-9` = -9, `-8` = -8, `-1` = -1
    )
  } else if (wave_name == "wave9") {
    # W9DETHN15: -8->-8
    eth[!is.na(eth)] <- recode(eth[!is.na(eth)],
      `1` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = 6, `7` = 7,
      `8` = 8, `9` = 9, `10` = 10, `11` = 11, `12` = 12, `13` = 13,
      `14` = 14, `15` = 15, `16` = 16,
      `-8` = -8
    )
  }
  
  return(eth)
}

# Apply harmonization to each wave
df$eth_w1 <- harmonize_ethnicity(df$W1ethnic2YP, "wave1")
df$eth_w2 <- harmonize_ethnicity(df$W2ethnicYP, "wave2")
df$eth_w4 <- harmonize_ethnicity(df$w4ethnic2YP, "wave4")
df$eth_w8 <- harmonize_ethnicity(df$W8DETHN15, "wave8")
df$eth_w9 <- harmonize_ethnicity(df$W9DETHN15, "wave9")

# Create consolidated eth variable: earliest-valid-first
# Order of waves: 14, 15, 17, 25, 32
df$eth <- coalesce(
  df$eth_w1,
  df$eth_w2,
  df$eth_w4,
  df$eth_w8,
  df$eth_w9
)

# Select only NSID and eth
df <- df %>% select(NSID, eth)

# Write output - labels are not needed for CSV export
write_csv(df, "data/output/cleaned_data.csv")

print("Cleaned data saved to data/output/cleaned_data.csv")
print(paste("Number of rows:", nrow(df)))
print(table(df$eth, useNA = "ifany"))
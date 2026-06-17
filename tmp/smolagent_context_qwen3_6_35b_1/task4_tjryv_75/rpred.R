library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(labelled)
library(haven)

# Load all files from data/input/
# Wave 1 (Age 14) - only has NSID
df1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)

# Wave 4 (Age 17) - only has NSID
df4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)

# Wave 6 (Age 19) - has W6SexualityYP
df6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)

# Wave 7 (Age 20) - has W7SexualityYP
df7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)

# Wave 8 (Age 25) - has W8SEXUALITY
df8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", show_col_types = FALSE)

# Wave 9 (Age 32) - has W9SORI
df9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID using full_join
df <- df1 %>%
  full_join(df4, by = "NSID") %>%
  full_join(df6, by = "NSID") %>%
  full_join(df7, by = "NSID") %>%
  full_join(df8, by = "NSID") %>%
  full_join(df9, by = "NSID")

cat("Merged dataset dimensions:", dim(df), "\n")

# Function to recode W6SexualityYP (wave 6, age 19)
recode_sori_w6 <- function(x) {
  recoded <- case_when(
    x == -97 ~ -9,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 4,
    is.na(x) ~ -3,
    TRUE ~ NA_real_
  )
  return(recoded)
}

# Function to recode W7SexualityYP (wave 7, age 20)
recode_sori_w7 <- function(x) {
  recoded <- case_when(
    x == -100 ~ -9,
    x == -97 ~ -9,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 4,
    is.na(x) ~ -3,
    TRUE ~ NA_real_
  )
  return(recoded)
}

# Function to recode W8SEXUALITY (wave 8, age 25)
recode_sori_w8 <- function(x) {
  recoded <- case_when(
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 4,
    is.na(x) ~ -3,
    TRUE ~ NA_real_
  )
  return(recoded)
}

# Function to recode W9SORI (wave 9, age 32)
recode_sori_w9 <- function(x) {
  recoded <- case_when(
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -3 ~ -3,
    x == -1 ~ -1,
    x == 5 ~ -7,
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 4,
    is.na(x) ~ -3,
    TRUE ~ NA_real_
  )
  return(recoded)
}

# Create the four sori variables
df <- df %>%
  mutate(sori19 = recode_sori_w6(W6SexualityYP)) %>%
  mutate(sori20 = recode_sori_w7(W7SexualityYP)) %>%
  mutate(sori25 = recode_sori_w8(W8SEXUALITY)) %>%
  mutate(sori32 = recode_sori_w9(W9SORI))

# Keep only NSID and the four sori variables
df_out <- df %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Check for any unexpected values
cat("Unique values in sori19:", sort(unique(df_out$sori19)), "\n")
cat("Unique values in sori20:", sort(unique(df_out$sori20)), "\n")
cat("Unique values in sori25:", sort(unique(df_out$sori25)), "\n")
cat("Unique values in sori32:", sort(unique(df_out$sori32)), "\n")

# Write output
dir.create("data/output", showWarnings = FALSE)
write_csv(df_out, "data/output/cleaned_data.csv")

cat("Output written successfully.\n")
cat("Final dataset dimensions:", dim(df_out), "\n")

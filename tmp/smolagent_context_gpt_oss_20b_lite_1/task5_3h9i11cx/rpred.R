# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Define file paths
file_paths <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  ns8 = "data/input/ns8_2015_derived.tab",
  ns9 = "data/input/ns9_2022_derived_variables.tab"
)

# Function to read a tab-delimited file
read_tab <- function(path){
  read_delim(path, delim="\t", col_types=cols(.default = col_character()), trim_ws=TRUE)
}

# Read files
wave_one_df <- read_tab(file_paths$wave_one)
wave_four_df <- read_tab(file_paths$wave_four)
wave_six_df <- read_tab(file_paths$wave_six)
ns8_df <- read_tab(file_paths$ns8)
ns9_df <- read_tab(file_paths$ns9)

# Harmonisation functions
harmonise_marital <- function(x){
  x_num <- as.numeric(x)
  # Replace NA with -3
  x_num[is.na(x_num)] <- -3
  # Map specific negative codes
  x_num[x_num %in% c(-997,-97)] <- -2
  x_num[x_num == -92] <- -9
  x_num[x_num == -91] <- -1
  x_num
}
adult_map <- function(x){
  x_num <- as.numeric(x)
  x_num[is.na(x_num)] <- -3
  # Collapse civil partnership categories (6-9) to 6
  x_num[x_num %in% c(6,7,8,9)] <- 6
  x_num
}

# Derive variables for each wave
partnr19_df <- wave_six_df %>%
  select(NSID, W6MarStatYP) %>%
  mutate(partnr19 = harmonise_marital(W6MarStatYP)) %>%
  select(NSID, partnr19)

partnr25_df <- ns8_df %>%
  select(NSID, W8DMARSTAT) %>%
  mutate(partnr25 = harmonise_marital(W8DMARSTAT)) %>%
  select(NSID, partnr25)

partnr32_df <- ns9_df %>%
  select(NSID, W9DMARSTAT) %>%
  mutate(partnr32 = harmonise_marital(W9DMARSTAT)) %>%
  select(NSID, partnr32)

partnradu25_df <- ns8_df %>%
  select(NSID, W8DMARSTAT) %>%
  mutate(partnradu25 = adult_map(W8DMARSTAT)) %>%
  select(NSID, partnradu25)

partnradu32_df <- ns9_df %>%
  select(NSID, W9DMARSTAT) %>%
  mutate(partnradu32 = adult_map(W9DMARSTAT)) %>%
  select(NSID, partnradu32)

# Combine all IDs from all waves
all_ids <- bind_rows(
  wave_one_df %>% select(NSID),
  wave_four_df %>% select(NSID),
  wave_six_df %>% select(NSID),
  ns8_df %>% select(NSID),
  ns9_df %>% select(NSID)
) %>% distinct(NSID)

# Merge derived variables into one data frame
clean_df <- all_ids %>%
  full_join(partnr19_df, by="NSID") %>%
  full_join(partnr25_df, by="NSID") %>%
  full_join(partnr32_df, by="NSID") %>%
  full_join(partnradu25_df, by="NSID") %>%
  full_join(partnradu32_df, by="NSID")

# Reorder columns
clean_df <- clean_df %>% select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Write to CSV
write_csv(clean_df, "data/output/cleaned_data.csv")

cat("Cleaning completed. File written to data/output/cleaned_data.csv\n")

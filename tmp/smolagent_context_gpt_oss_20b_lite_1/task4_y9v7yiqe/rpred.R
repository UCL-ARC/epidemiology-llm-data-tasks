library(readr)
library(dplyr)

# File paths
wave_one_path <- "data/input/wave_one_lsype_young_person_2020.tab"
wave_four_path <- "data/input/wave_four_lsype_young_person_2020.tab"
wave_six_path <- "data/input/wave_six_lsype_young_person_2020.tab"
wave_seven_path <- "data/input/wave_seven_lsype_young_person_2020.tab"
wave_eight_path <- "data/input/ns8_2015_self_completion.tab"
wave_nine_path <- "data/input/ns9_2022_main_interview.tab"

# Load datasets
wave_one <- read_delim(wave_one_path, delim = "\t", show_col_types = FALSE)
wave_four <- read_delim(wave_four_path, delim = "\t", show_col_types = FALSE)
wave_six <- read_delim(wave_six_path, delim = "\t", show_col_types = FALSE)
wave_seven <- read_delim(wave_seven_path, delim = "\t", show_col_types = FALSE)
wave_eight <- read_delim(wave_eight_path, delim = "\t", show_col_types = FALSE)
wave_nine <- read_delim(wave_nine_path, delim = "\t", show_col_types = FALSE)

# Merge all by NSID (full join to keep all IDs)
merged_all <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Functions to harmonise missing codes for each wave
convert_w6 <- function(x) {
  case_when(
    x %in% c(-97, -92) ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    is.na(x) ~ -3,
    TRUE ~ x
  )
}
convert_w7 <- function(x) {
  case_when(
    x %in% c(-100, -97, -92) ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    is.na(x) ~ -3,
    TRUE ~ x
  )
}
convert_w8 <- function(x) {
  case_when(
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    is.na(x) ~ -3,
    TRUE ~ x
  )
}
convert_w9 <- function(x) {
  case_when(
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -3 ~ -3,
    x == -1 ~ -1,
    x == 5 ~ -7,
    is.na(x) ~ -3,
    TRUE ~ x
  )
}

# Create cleaned dataframe with only required variables
clean_df <- data.frame(
  NSID = merged_all$NSID,
  sori19 = convert_w6(merged_all$W6SexualityYP),
  sori20 = convert_w7(merged_all$W7SexualityYP),
  sori25 = convert_w8(merged_all$W8SEXUALITY),
  sori32 = convert_w9(merged_all$W9SORI)
)

# Write to CSV
write_csv(clean_df, "data/output/cleaned_data.csv")

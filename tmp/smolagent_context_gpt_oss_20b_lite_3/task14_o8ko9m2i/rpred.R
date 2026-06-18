library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Helper function to harmonise missing codes for a given vector and mapping
harmonise_missing <- function(vec, mapping) {
  # mapping is a named vector: source_value -> target_value
  # Use case_when for efficiency
  out <- vec
  for (src in names(mapping)) {
    out[vec == as.numeric(src)] <- as.numeric(mapping[[src]])
  }
  return(out)
}

# Read all files
input_path <- "data/input/"

# Define filename to variable names mapping
files <- list(
  "wave_one_lsype_family_background_2020.tab" = "df_wave1",
  "wave_two_lsype_family_background_2020.tab" = "df_wave2",
  "wave_three_lsype_family_background_2020.tab" = "df_wave3",
  "wave_four_lsype_family_background_2020.tab" = "df_wave4",
  "wave_five_lsype_family_background_2020.tab" = "df_wave5",
  "wave_six_lsype_young_person_2020.tab" = "df_wave6",
  "wave_seven_lsype_young_person_2020.tab" = "df_wave7",
  "ns8_2015_main_interview.tab" = "df_wave8",
  "ns9_2022_derived_variables.tab" = "df_wave9"
)

# Read each file into a list
file_list <- map(names(files), ~ read_delim(file.path(input_path, .x), delim = "\t", col_types = cols()))
names(file_list) <- files

# Merge all datasets by NSID using full_join
merged_df <- reduce(file_list, function(x, y) full_join(x, y, by = "NSID"))

# Create harmonised missing value maps for each wave
# 14 - wave1
map14 <- c("-999" = "-2", "-92" = "-9", "-91" = "-1", "-1" = "-8")
# 15 - wave2
map15 <- c("-998" = "-2", "-997" = "-2", "-995" = "-2", "-99" = "-3", "-92" = "-9", "-91" = "-1", "-1" = "-8")
# 16 - wave3
map16 <- c("-999" = "-2", "-99" = "-3", "-92" = "-9", "-91" = "-1", "-1" = "-8")
# 17 - wave4
map17 <- c("-999" = "-2", "-997" = "-2", "-92" = "-9", "-91" = "-1", "-1" = "-8")
# 18 - wave5
map18 <- c("-999" = "-2", "-92" = "-9", "-91" = "-1", "-1" = "-8")
# 19 - wave6
map19 <- c("-92" = "-9", "-91" = "-1", "-1" = "-8")
# 20 - wave7
map20 <- c("-92" = "-9", "-91" = "-1", "-1" = "-8")
# 25 - wave8
map25 <- c("-9" = "-9", "-8" = "-8", "-1" = "-1")
# 32 - wave9
map32 <- c("-8" = "-8", "-1" = "-1")

# Derived detailed variables hownteen14-20
merged_df <- merged_df %>%
  mutate(
    hownteen14 = case_when(
      W1hous12HH == -999 ~ -2,
      W1hous12HH == -92 ~ -9,
      W1hous12HH == -91 ~ -1,
      W1hous12HH == -1 ~ -8,
      TRUE ~ W1hous12HH
    ),
    hownteen15 = case_when(
      W2Hous12HH == -998 ~ -2,
      W2Hous12HH == -997 ~ -2,
      W2Hous12HH == -995 ~ -2,
      W2Hous12HH == -99 ~ -3,
      W2Hous12HH == -92 ~ -9,
      W2Hous12HH == -91 ~ -1,
      W2Hous12HH == -1 ~ -8,
      TRUE ~ W2Hous12HH
    ),
    hownteen16 = case_when(
      W3hous12HH == -999 ~ -2,
      W3hous12HH == -99 ~ -3,
      W3hous12HH == -92 ~ -9,
      W3hous12HH == -91 ~ -1,
      W3hous12HH == -1 ~ -8,
      TRUE ~ W3hous12HH
    ),
    hownteen17 = case_when(
      W4Hous12HH == -999 ~ -2,
      W4Hous12HH == -997 ~ -2,
      W4Hous12HH == -92 ~ -9,
      W4Hous12HH == -91 ~ -1,
      W4Hous12HH == -1 ~ -8,
      TRUE ~ W4Hous12HH
    ),
    hownteen18 = case_when(
      W5Hous12HH == -999 ~ -2,
      W5Hous12HH == -92 ~ -9,
      W5Hous12HH == -91 ~ -1,
      W5Hous12HH == -1 ~ -8,
      TRUE ~ W5Hous12HH
    ),
    hownteen19 = case_when(
      W6Hous12YP == -92 ~ -9,
      W6Hous12YP == -91 ~ -1,
      W6Hous12YP == -1 ~ -8,
      TRUE ~ W6Hous12YP
    ),
    hownteen20 = case_when(
      W7Hous12YP == -92 ~ -9,
      W7Hous12YP == -91 ~ -1,
      W7Hous12YP == -1 ~ -8,
      TRUE ~ W7Hous12YP
    )
  )

# Derived collapsed variables hown14-32
merged_df <- merged_df %>%
  mutate(
    # Age 14
    hown14 = case_when(
      W1hous12HH == -999 ~ -2,
      W1hous12HH == -92 ~ -9,
      W1hous12HH == -91 ~ -1,
      W1hous12HH == -1 ~ -8,
      W1hous12HH %in% c(4,5,6) ~ 4,
      W1hous12HH == 1 ~ 1,
      W1hous12HH == 2 ~ 2,
      W1hous12HH == 3 ~ 3,
      W1hous12HH == 7 ~ 5,
      W1hous12HH == 8 ~ 7,
      TRUE ~ NA_real_
    ),
    # Age 15
    hown15 = case_when(
      W2Hous12HH == -998 ~ -2,
      W2Hous12HH == -997 ~ -2,
      W2Hous12HH == -995 ~ -2,
      W2Hous12HH == -99 ~ -3,
      W2Hous12HH == -92 ~ -9,
      W2Hous12HH == -91 ~ -1,
      W2Hous12HH == -1 ~ -8,
      W2Hous12HH %in% c(4,5,6) ~ 4,
      W2Hous12HH == 1 ~ 1,
      W2Hous12HH == 2 ~ 2,
      W2Hous12HH == 3 ~ 3,
      W2Hous12HH == 7 ~ 5,
      W2Hous12HH == 8 ~ 7,
      TRUE ~ NA_real_
    ),
    # Age 16
    hown16 = case_when(
      W3hous12HH == -999 ~ -2,
      W3hous12HH == -99 ~ -3,
      W3hous12HH == -92 ~ -9,
      W3hous12HH == -91 ~ -1,
      W3hous12HH == -1 ~ -8,
      W3hous12HH %in% c(4,5,6) ~ 4,
      W3hous12HH == 1 ~ 1,
      W3hous12HH == 2 ~ 2,
      W3hous12HH == 3 ~ 3,
      W3hous12HH == 7 ~ 5,
      W3hous12HH == 8 ~ 7,
      TRUE ~ NA_real_
    ),
    # Age 17
    hown17 = case_when(
      W4Hous12HH == -999 ~ -2,
      W4Hous12HH == -997 ~ -2,
      W4Hous12HH == -92 ~ -9,
      W4Hous12HH == -91 ~ -1,
      W4Hous12HH == -1 ~ -8,
      W4Hous12HH %in% c(4,5,6) ~ 4,
      W4Hous12HH == 1 ~ 1,
      W4Hous12HH == 2 ~ 2,
      W4Hous12HH == 3 ~ 3,
      W4Hous12HH == 7 ~ 5,
      W4Hous12HH == 8 ~ 7,
      TRUE ~ NA_real_
    ),
    # Age 18
    hown18 = case_when(
      W5Hous12HH == -999 ~ -2,
      W5Hous12HH == -92 ~ -9,
      W5Hous12HH == -91 ~ -1,
      W5Hous12HH == -1 ~ -8,
      # collapsed rent categories: 2 maps to rent it
      W5Hous12HH == 2 ~ 4,
      W5Hous12HH == 1 ~ 1,
      W5Hous12HH == 3 ~ 7,
      TRUE ~ NA_real_
    ),
    # Age 19
    hown19 = case_when(
      W6Hous12YP == -92 ~ -9,
      W6Hous12YP == -91 ~ -1,
      W6Hous12YP == -1 ~ -8,
      W6Hous12YP == 2 ~ 4,
      W6Hous12YP == 1 ~ 1,
      W6Hous12YP == 3 ~ 7,
      TRUE ~ NA_real_
    ),
    # Age 20
    hown20 = case_when(
      W7Hous12YP == -92 ~ -9,
      W7Hous12YP == -91 ~ -1,
      W7Hous12YP == -1 ~ -8,
      W7Hous12YP == 2 ~ 4,
      W7Hous12YP == 1 ~ 1,
      W7Hous12YP == 3 ~ 7,
      TRUE ~ NA_real_
    ),
    # Age 25
    hown25 = case_when(
      W8TENURE == -9 ~ -9,
      W8TENURE == -8 ~ -8,
      W8TENURE == -1 ~ -1,
      W8TENURE %in% c(4,5,6) ~ 4,
      W8TENURE == 1 ~ 1,
      W8TENURE == 2 ~ 2,
      W8TENURE == 3 ~ 3,
      W8TENURE == 7 ~ 7,
      TRUE ~ NA_real_
    ),
    # Age 32
    hown32 = case_when(
      W9DTENURE == -8 ~ -8,
      W9DTENURE == -1 ~ -1,
      W9DTENURE %in% c(4,5,6) ~ 4,
      W9DTENURE == 1 ~ 1,
      W9DTENURE == 2 ~ 2,
      W9DTENURE == 3 ~ 3,
      W9DTENURE == 7 ~ 7,
      TRUE ~ NA_real_
    )
  )

# Select final columns
final_df <- merged_df %>% select(NSID, starts_with("hownteen"), starts_with("hown"))

# Write CSV
output_path <- "data/output/cleaned_data.csv"
write_csv(final_df, output_path)

cat("Finished writing to", output_path, "\n")

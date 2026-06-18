library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Helper functions to map missing codes
map_codes_w1_7 <- function(x) {
  x_num <- as.numeric(x)
  case_when(
    x_num %in% 1:8 ~ x_num,
    x_num == -1 ~ -8,
    x_num == -92 ~ -9,
    x_num == -91 ~ -1,
    x_num %in% c(-999,-998,-997,-995) ~ -2,
    x_num == -99 ~ -3,
    TRUE ~ NA_real_
  )
}

map_codes_w8_9 <- function(x) {
  x_num <- as.numeric(x)
  case_when(
    x_num %in% 1:7 ~ x_num,
    x_num == -1 ~ -1,
    x_num == -9 ~ -9,
    x_num == -8 ~ -8,
    x_num == -92 ~ -9,
    x_num == -91 ~ -1,
    x_num %in% c(-999,-998,-997,-995) ~ -2,
    x_num == -99 ~ -3,
    TRUE ~ NA_real_
  )
}

# Load all datasets
input_path <- "data/input/"
files <- c(
  ns9_2022_derived_variables = "ns9_2022_derived_variables.tab",
  wave_four_lsype_family_background_2020 = "wave_four_lsype_family_background_2020.tab",
  wave_three_lsype_family_background_2020 = "wave_three_lsype_family_background_2020.tab",
  wave_two_lsype_family_background_2020 = "wave_two_lsype_family_background_2020.tab",
  wave_one_lsype_family_background_2020 = "wave_one_lsype_family_background_2020.tab",
  ns8_2015_main_interview = "ns8_2015_main_interview.tab",
  wave_five_lsype_family_background_2020 = "wave_five_lsype_family_background_2020.tab",
  wave_six_lsype_young_person_2020 = "wave_six_lsype_young_person_2020.tab",
  wave_seven_lsype_young_person_2020 = "wave_seven_lsype_young_person_2020.tab"
)
raw_data <- lapply(files, function(fname) {
  read_delim(file.path(input_path, fname), delim = "\t", col_types = cols(), na = c("", "NA"))
})
names(raw_data) <- names(files)

# Merge all datasets by NSID
cohort <- reduce(raw_data, function(x, y) full_join(x, y, by = "NSID"))

# Detailed variables for waves 1-4
cohort <- cohort %>% mutate(
  hownteen14 = map_codes_w1_7(W1hous12HH),
  hownteen15 = map_codes_w1_7(W2Hous12HH),
  hownteen16 = map_codes_w1_7(W3hous12HH),
  hownteen17 = map_codes_w1_7(W4Hous12HH)
)

# Helper to derive detailed from subtypes for waves 5-7
derive_hownteen <- function(type, owned, rented) {
  owned_mapped <- map_codes_w1_7(owned)
  rented_mapped <- map_codes_w1_7(rented)
  case_when(
    !is.na(owned_mapped) & owned_mapped %in% 1:3 ~ owned_mapped,
    !is.na(owned_mapped) & owned_mapped == 4 ~ 8,
    is.na(owned_mapped) & !is.na(rented_mapped) & rented_mapped %in% 1:4 ~ rented_mapped,
    is.na(owned_mapped) & !is.na(rented_mapped) & rented_mapped == 5 ~ 8,
    !is.na(owned_mapped) & owned_mapped %in% c(-9,-8,-1,-2,-3) ~ owned_mapped,
    is.na(owned_mapped) & !is.na(rented_mapped) & rented_mapped %in% c(-9,-8,-1,-2,-3) ~ rented_mapped,
    TRUE ~ NA_real_
  )
}

# Waves 5-7
cohort <- cohort %>% mutate(
  hownteen18 = derive_hownteen(W5Hous12HH, W5Hous12BHH, W5Hous12CHH),
  hownteen19 = derive_hownteen(W6Hous12YP, W6Hous12bYP, W6Hous12cYP),
  hownteen20 = derive_hownteen(W7Hous12YP, W7Hous12bYP, W7Hous12cYP)
)

# Collapsed 6-category mapping
collapse_to_rent_it <- function(x) {
  case_when(
    x %in% 1:3 ~ x,
    x %in% 4:6 ~ 4,
    x == 7 ~ 7,
    x == 8 ~ 8,
    TRUE ~ x
  )
}

# Collapsed for waves 1-4
cohort <- cohort %>% mutate(
  hown14 = collapse_to_rent_it(hownteen14),
  hown15 = collapse_to_rent_it(hownteen15),
  hown16 = collapse_to_rent_it(hownteen16),
  hown17 = collapse_to_rent_it(hownteen17)
)

# Collapsed for waves 5-7
cohort <- cohort %>% mutate(
  hown18 = collapse_to_rent_it(hownteen18),
  hown19 = collapse_to_rent_it(hownteen19),
  hown20 = collapse_to_rent_it(hownteen20)
)

# Waves 8-9
cohort <- cohort %>% mutate(
  hown25_raw = map_codes_w8_9(W8TENURE),
  hown25 = case_when(
    hown25_raw %in% 1:5 ~ hown25_raw,
    hown25_raw %in% c(6,7) ~ 6,
    TRUE ~ hown25_raw
  ),
  hown32_raw = map_codes_w8_9(W9DTENURE),
  hown32 = case_when(
    hown32_raw %in% 1:5 ~ hown32_raw,
    hown32_raw %in% c(6,7) ~ 6,
    TRUE ~ hown32_raw
  )
)

# Final selection
final_vars <- cohort %>% select(
  NSID,
  hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20,
  hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32
)

# Write CSV
write_csv(final_vars, "data/output/cleaned_data.csv", na = "")

cat("Cleaning complete. File written to data/output/cleaned_data.csv\n")

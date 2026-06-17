library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# Helper to map missing codes
convert_missing <- function(x, sweep_type) {
  x <- replace(x, !is.na(x) & x == -92, -9)   # Refused
  x <- replace(x, !is.na(x) & x == -91, -1)   # Not applicable
  x <- replace(x, !is.na(x) & x == -99, -3)   # Missing / not asked
  x <- replace(x, !is.na(x) & x %in% c(-999,-998,-997,-995), -2)  # Script error / not applicable
  if (sweep_type <= 7) {
    x <- replace(x, !is.na(x) & x == -1, -8)   # Don't know (adolescents)
  }
  x[is.na(x)] <- -3
  return(x)
}

# Collapse raw to 6 categories
collapse_from_raw <- function(x, sweep_type) {
  x_clean <- convert_missing(x, sweep_type)
  case_when(
    x_clean == 1 ~ 1,
    x_clean == 2 ~ 2,
    x_clean == 3 ~ 3,
    x_clean %in% c(4,5,6) ~ 4,      # Rent it
    x_clean == 7 ~ 5,                # Rent free
    x_clean == 8 ~ 6,                # Other
    TRUE ~ x_clean
  )
}

# Map detailed 8 to collapsed 6
collap_map <- function(detail) {
  case_when(
    detail == 1 ~ 1,
    detail == 2 ~ 2,
    detail == 3 ~ 3,
    detail %in% c(4,5,6) ~ 4,
    detail == 7 ~ 5,
    detail == 8 ~ 6,
    TRUE ~ detail
  )
}

# File paths
files <- list(
  wave_one    = 'data/input/wave_one_lsype_family_background_2020.tab',
  wave_two    = 'data/input/wave_two_lsype_family_background_2020.tab',
  wave_three  = 'data/input/wave_three_lsype_family_background_2020.tab',
  wave_four   = 'data/input/wave_four_lsype_family_background_2020.tab',
  wave_five   = 'data/input/wave_five_lsype_family_background_2020.tab',
  wave_six    = 'data/input/wave_six_lsype_young_person_2020.tab',
  wave_seven  = 'data/input/wave_seven_lsype_young_person_2020.tab',
  wave_eight  = 'data/input/ns8_2015_main_interview.tab',
  wave_nine   = 'data/input/ns9_2022_derived_variables.tab'
)

# Load all
list_df <- lapply(files, function(p) read_delim(p, delim = '\t', col_types = cols()))

# Merge
merged <- list_df[[1]]
for (i in 2:length(list_df)) {
  merged <- full_join(merged, list_df[[i]], by = 'NSID')
}

# Detailed for waves 1-4
merged <- merged %>%
  mutate(
    hownteen14 = convert_missing(W1hous12HH, 1),
    hownteen15 = convert_missing(W2Hous12HH, 2),
    hownteen16 = convert_missing(W3hous12HH, 3),
    hownteen17 = convert_missing(W4Hous12HH, 4)
  )

# Wave 5 mapping
merged <- merged %>%
  mutate(
    owned5_raw = convert_missing(W5Hous12BHH, 5),
    rented5_raw = convert_missing(W5Hous12CHH, 5),
    type5_raw = convert_missing(W5Hous12HH, 5)
  )
merged <- merged %>%
  mutate(
    owned5_map = case_when(
      owned5_raw %in% 1:3 ~ owned5_raw,
      owned5_raw == 4 ~ 8,
      TRUE ~ NA_real_
    ),
    rented5_map = case_when(
      rented5_raw == 1 ~ 4,
      rented5_raw == 2 ~ 5,
      rented5_raw == 3 ~ 6,
      rented5_raw == 4 ~ 7,
      rented5_raw == 5 ~ 8,
      TRUE ~ NA_real_
    ),
    hownteen18 = case_when(
      !is.na(owned5_map) ~ owned5_map,
      !is.na(rented5_map) ~ rented5_map,
      type5_raw == 3 ~ 8,
      !is.na(owned5_raw) ~ owned5_raw,
      !is.na(rented5_raw) ~ rented5_raw,
      TRUE ~ NA_real_
    )
  )

# Wave 6 mapping
merged <- merged %>%
  mutate(
    owned6_raw = convert_missing(W6Hous12bYP, 6),
    rented6_raw = convert_missing(W6Hous12cYP, 6),
    type6_raw = convert_missing(W6Hous12YP, 6)
  )
merged <- merged %>%
  mutate(
    owned6_map = case_when(
      owned6_raw %in% 1:3 ~ owned6_raw,
      owned6_raw == 4 ~ 8,
      TRUE ~ NA_real_
    ),
    rented6_map = case_when(
      rented6_raw == 1 ~ 4,
      rented6_raw == 2 ~ 5,
      rented6_raw == 3 ~ 6,
      rented6_raw == 4 ~ 7,
      rented6_raw == 5 ~ 8,
      TRUE ~ NA_real_
    ),
    hownteen19 = case_when(
      !is.na(owned6_map) ~ owned6_map,
      !is.na(rented6_map) ~ rented6_map,
      type6_raw == 3 ~ 8,
      !is.na(owned6_raw) ~ owned6_raw,
      !is.na(rented6_raw) ~ rented6_raw,
      TRUE ~ NA_real_
    )
  )

# Wave 7 mapping
merged <- merged %>%
  mutate(
    owned7_raw = convert_missing(W7Hous12bYP, 7),
    rented7_raw = convert_missing(W7Hous12cYP, 7),
    type7_raw = convert_missing(W7Hous12YP, 7)
  )
merged <- merged %>%
  mutate(
    owned7_map = case_when(
      owned7_raw %in% 1:3 ~ owned7_raw,
      owned7_raw == 4 ~ 8,
      TRUE ~ NA_real_
    ),
    rented7_map = case_when(
      rented7_raw == 1 ~ 4,
      rented7_raw == 2 ~ 5,
      rented7_raw == 3 ~ 6,
      rented7_raw == 4 ~ 7,
      rented7_raw == 5 ~ 8,
      TRUE ~ NA_real_
    ),
    hownteen20 = case_when(
      !is.na(owned7_map) ~ owned7_map,
      !is.na(rented7_map) ~ rented7_map,
      type7_raw == 3 ~ 8,
      !is.na(owned7_raw) ~ owned7_raw,
      !is.na(rented7_raw) ~ rented7_raw,
      TRUE ~ NA_real_
    )
  )

# Collapsed for waves 1-4
merged <- merged %>%
  mutate(
    hown14 = collapse_from_raw(W1hous12HH, 1),
    hown15 = collapse_from_raw(W2Hous12HH, 2),
    hown16 = collapse_from_raw(W3hous12HH, 3),
    hown17 = collapse_from_raw(W4Hous12HH, 4)
  )

# Collapsed for waves 5-7 from detailed
merged <- merged %>%
  mutate(
    hown18 = collap_map(hownteen18),
    hown19 = collap_map(hownteen19),
    hown20 = collap_map(hownteen20)
  )

# Collapsed for waves 8-9
merged <- merged %>%
  mutate(
    hown25 = collapse_from_raw(W8TENURE, 8),
    hown32 = collapse_from_raw(W9DTENURE, 9)
  )

# Final selection
final_df <- merged %>%
  select(NSID,
         hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20,
         hown14, hown15, hown16, hown17, hown18, hown19, hown20,
         hown25, hown32)

# Write
write_csv(final_df, 'data/output/cleaned_data.csv')

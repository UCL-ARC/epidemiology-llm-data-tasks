library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
path <- "data/input/"
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab",
  "wave_three_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_self_completion.tab",
  "ns9_2022_main_interview.tab"
)

full_df <- NULL
for (f in files) {
  df <- read_delim(paste0(path, f), delim = "\t", show_col_types = FALSE)
  if (is.null(full_df)) {
    full_df <- df
  } else {
    full_df <- full_join(full_df, df, by = "NSID")
  }
}

# 2. Target Variables Identification and Logic
# Wave-to-age mapping: S1->14, S2->15, S3->16, S4->17, S6->19, S7->20, S8->25, S9->32

full_df <- full_df %>%
  mutate(
    drink_14 = case_when(
      W1alceverYP == 1 & W1alcmonYP == 1 ~ 1,
      W1alceverYP == 2 | W1alcmonYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    drink_15 = case_when(
      W2alceverYP == 1 ~ 1,
      W2alceverYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    drink_16 = case_when(
      W3alceverYP == 1 ~ 1,
      W3alceverYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    drink_17 = case_when(
      W4AlcEverYP == 1 ~ 1,
      W4AlcEverYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    drink_19 = case_when(
      W6AlcEverYP == 1 ~ 1,
      W6AlcEverYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    drink_20 = case_when(
      W7AlcEverYP == 1 ~ 1,
      W7AlcEverYP == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    drink_25 = case_when(
      W8AUDIT1 > 1 ~ 1,
      W8AUDIT1 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    drink_32 = case_when(
      W9AUDIT1 > 1 ~ 1,
      W9AUDIT1 == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

ages <- c(14, 15, 16, 17, 19, 20, 25, 32)
indicator_cols <- paste0("drink_", ages)

# Find the earliest age
full_df$earliest_age <- NA_real_
for (age in ages) {
  col <- paste0("drink_", age)
  full_df$earliest_age <- ifelse(!is.na(full_df[[col]]) & full_df[[col]] == 1 & is.na(full_df$earliest_age), age, full_df$earliest_age)
}

# Never drinker flag
full_df <- full_df %>%
  rowwise() %>%
  mutate(
    obs_vals = list(c_across(all_of(indicator_cols))),
    never_drinker = case_when(
      any(unlist(obs_vals) == 1, na.rm = TRUE) ~ 0,
      all(!is.na(unlist(obs_vals)) & unlist(obs_vals) == 0) ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# Final alcfst derivation
full_df <- full_df %>%
  mutate(
    alcfst_val = case_when(
      !is.na(earliest_age) ~ earliest_age,
      never_drinker == 1 ~ 99,
      TRUE ~ -8
    )
  )

# Factor conversion
levels_vals <- c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8)
levels_labs <- c("Age 14", "Age 15", "Age 16", "Age 17", "Age 19", "Age 20", "Age 25", "Age 32", "Never had alcohol", "Don't know/insufficient information")

full_df$alcfst <- factor(full_df$alcfst_val, levels = levels_vals, labels = levels_labs)

# Output
final_output <- full_df %>%
  select(NSID, alcfst)

write_csv(final_output, "data/output/cleaned_data.csv")
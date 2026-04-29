library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# The previous error (many-to-many join) suggests NSID is being read as numeric
# or has issues. Since NSIDs should be unique per person, we must ensure they are
# read as characters to avoid precision loss and then check for duplicates.

vars_needed <- list(
  'wave_one_lsype_family_background_2020.tab' = c('NSID', 'W1hous12HH'),
  'wave_two_lsype_family_background_2020.tab' = c('NSID', 'W2Hous12HH'),
  'wave_three_lsype_family_background_2020.tab' = c('NSID', 'W3hous12HH'),
  'wave_four_lsype_family_background_2020.tab' = c('NSID', 'W4Hous12HH'),
  'wave_five_lsype_family_background_2020.tab' = c('NSID', 'W5Hous12HH', 'W5Hous12BHH', 'W5Hous12CHH'),
  'wave_six_lsype_young_person_2020.tab' = c('NSID', 'W6Hous12YP', 'W6Hous12bYP', 'W6Hous12cYP'),
  'wave_seven_lsype_young_person_2020.tab' = c('NSID', 'W7Hous12YP', 'W7Hous12bYP', 'W7Hous12cYP'),
  'ns8_2015_main_interview.tab' = c('NSID', 'W8TENURE'),
  'ns9_2022_derived_variables.tab' = c('NSID', 'W9DTENURE')
)

load_subset <- function(file, cols) {
  # Explicitly read NSID as character to prevent numeric overflow/rounding issues
  df <- read_delim(paste0('data/input/', file), delim = '\t', col_types = readr::cols(NSID = col_character(), .default = 'numeric')) %>%
    select(all_of(cols))
  # Remove duplicates to prevent many-to-many join explosion
  df <- df %>% distinct(NSID, .keep_all = TRUE)
  return(df)
}

cleaned_data <- data.frame(NSID = character())
first_file <- names(vars_needed)[1]
cleaned_data <- load_subset(first_file, vars_needed[[first_file]])

for (i in 2:length(vars_needed)) {
  file_name <- names(vars_needed)[i]
  cols <- vars_needed[[file_name]]
  temp_df <- load_subset(file_name, cols)
  cleaned_data <- full_join(cleaned_data, temp_df, by = 'NSID')
  rm(temp_df)
  gc()
}

recode_missing <- function(x) {
  case_when(
    is.na(x) ~ -3,
    x == -999.0 ~ -3,
    x == -92.0 ~ -9,
    x == -91.0 ~ -1,
    x == -1.0 ~ -8,
    x == -997.0 ~ -2,
    x == -998.0 ~ -2,
    x == -995.0 ~ -2,
    x == -99.0 ~ -3,
    TRUE ~ x
  )
}

cleaned_data <- cleaned_data %>%
  mutate(
    hownteen14 = recode_missing(W1hous12HH),
    hown14 = case_when(
      hownteen14 == 1 ~ 1, hownteen14 == 2 ~ 2, hownteen14 == 3 ~ 3,
      hownteen14 %in% c(4, 5, 6) ~ 4, hownteen14 == 7 ~ 5, hownteen14 == 8 ~ 6,
      hownteen14 == -1 ~ 7, hownteen14 == -2 ~ 8, hownteen14 == -3 ~ 9,
      hownteen14 == -8 ~ 10, hownteen14 == -9 ~ 11, TRUE ~ -3
    ),
    hownteen15 = recode_missing(W2Hous12HH),
    hown15 = case_when(
      hownteen15 == 1 ~ 1, hownteen15 == 2 ~ 2, hownteen15 == 3 ~ 3,
      hownteen15 %in% c(4, 5, 6) ~ 4, hownteen15 == 7 ~ 5, hownteen15 == 8 ~ 6,
      hownteen15 == -1 ~ 7, hownteen15 == -2 ~ 8, hownteen15 == -3 ~ 9,
      hownteen15 == -8 ~ 10, hownteen15 == -9 ~ 11, TRUE ~ -3
    ),
    hownteen16 = recode_missing(W3hous12HH),
    hown16 = case_when(
      hownteen16 == 1 ~ 1, hownteen16 == 2 ~ 2, hownteen16 == 3 ~ 3,
      hownteen16 %in% c(4, 5, 6) ~ 4, hownteen16 == 7 ~ 5, hownteen16 == 8 ~ 6,
      hownteen16 == -1 ~ 7, hownteen16 == -2 ~ 8, hownteen16 == -3 ~ 9,
      hownteen16 == -8 ~ 10, hownteen16 == -9 ~ 11, TRUE ~ -3
    ),
    hownteen17 = recode_missing(W4Hous12HH),
    hown17 = case_when(
      hownteen17 == 1 ~ 1, hownteen17 == 2 ~ 2, hownteen17 == 3 ~ 3,
      hownteen17 %in% c(4, 5, 6) ~ 4, hownteen17 == 7 ~ 5, hownteen17 == 8 ~ 6,
      hownteen17 == -1 ~ 7, hownteen17 == -2 ~ 8, hownteen17 == -3 ~ 9,
      hownteen17 == -8 ~ 10, hownteen17 == -9 ~ 11, TRUE ~ -3
    ),
    hownteen18 = case_when(
      W5Hous12HH == 1 ~ case_when(
        W5Hous12BHH == 1 ~ 1, W5Hous12BHH == 2 ~ 2, W5Hous12BHH == 3 ~ 3, W5Hous12BHH == 4 ~ 8,
        TRUE ~ recode_missing(W5Hous12BHH)
      ), 
      W5Hous12HH == 2 ~ case_when(
        W5Hous12CHH == 1 ~ 4, W5Hous12CHH == 2 ~ 5, W5Hous12CHH == 3 ~ 6, W5Hous12CHH == 4 ~ 7, W5Hous12CHH == 5 ~ 8,
        TRUE ~ recode_missing(W5Hous12CHH)
      ), 
      W5Hous12HH == 3 ~ 8,
      W5Hous12HH == 6 ~ -3,
      TRUE ~ recode_missing(W5Hous12HH)
    ),
    hown18 = case_when(
      hownteen18 == 1 ~ 1, hownteen18 == 2 ~ 2, hownteen18 == 3 ~ 3,
      hownteen18 %in% c(4, 5, 6) ~ 4, hownteen18 == 7 ~ 5, hownteen18 == 8 ~ 6,
      hownteen18 == -1 ~ 7, hownteen18 == -2 ~ 8, hownteen18 == -3 ~ 9,
      hownteen18 == -8 ~ 10, hownteen18 == -9 ~ 11, TRUE ~ -3
    ),
    hownteen19 = case_when(
      W6Hous12YP == 1 ~ case_when(
        W6Hous12bYP == 1 ~ 1, W6Hous12bYP == 2 ~ 2, W6Hous12bYP == 3 ~ 3, W6Hous12bYP == 4 ~ 8,
        TRUE ~ recode_missing(W6Hous12bYP)
      ), 
      W6Hous12YP == 2 ~ case_when(
        W6Hous12cYP == 1 ~ 4, W6Hous12cYP == 2 ~ 5, W6Hous12cYP == 3 ~ 6, W6Hous12cYP == 4 ~ 7, W6Hous12cYP == 5 ~ 8,
        TRUE ~ recode_missing(W6Hous12cYP)
      ), 
      W6Hous12YP == 3 ~ 8,
      TRUE ~ recode_missing(W6Hous12YP)
    ),
    hown19 = case_when(
      hownteen19 == 1 ~ 1, hownteen19 == 2 ~ 2, hownteen19 == 3 ~ 3,
      hownteen19 %in% c(4, 5, 6) ~ 4, hownteen19 == 7 ~ 5, hownteen19 == 8 ~ 6,
      hownteen19 == -1 ~ 7, hownteen19 == -2 ~ 8, hownteen19 == -3 ~ 9,
      hownteen19 == -8 ~ 10, hownteen19 == -9 ~ 11, TRUE ~ -3
    ),
    hownteen20 = case_when(
      W7Hous12YP == 1 ~ case_when(
        W7Hous12bYP == 1 ~ 1, W7Hous12bYP == 2 ~ 2, W7Hous12bYP == 3 ~ 3, W7Hous12bYP == 4 ~ 8,
        TRUE ~ recode_missing(W7Hous12bYP)
      ), 
      W7Hous12YP == 2 ~ case_when(
        W7Hous12cYP == 1 ~ 4, W7Hous12cYP == 2 ~ 5, W7Hous12cYP == 3 ~ 6, W7Hous12cYP == 4 ~ 7, W7Hous12cYP == 5 ~ 8,
        TRUE ~ recode_missing(W7Hous12cYP)
      ), 
      W7Hous12YP == 3 ~ 8,
      TRUE ~ recode_missing(W7Hous12YP)
    ),
    hown20 = case_when(
      hownteen20 == 1 ~ 1, hownteen20 == 2 ~ 2, hownteen20 == 3 ~ 3,
      hownteen20 %in% c(4, 5, 6) ~ 4, hownteen20 == 7 ~ 5, hownteen20 == 8 ~ 6,
      hownteen20 == -1 ~ 7, hownteen20 == -2 ~ 8, hownteen20 == -3 ~ 9,
      hownteen20 == -8 ~ 10, hownteen20 == -9 ~ 11, TRUE ~ -3
    ),
    hown25 = case_when(
      W8TENURE == 1 ~ 1, W8TENURE == 2 ~ 2, W8TENURE == 3 ~ 3,
      W8TENURE == 4 ~ 4, W8TENURE == 5 ~ 5, W8TENURE == 6 ~ 4, W8TENURE == 7 ~ 6,
      W8TENURE == -1 ~ 7, W8TENURE == -8 ~ 10, W8TENURE == -9 ~ 11, TRUE ~ 9
    ),
    hown32 = case_when(
      W9DTENURE == 1 ~ 1, W9DTENURE == 2 ~ 2, W9DTENURE == 3 ~ 3,
      W9DTENURE == 4 ~ 4, W9DTENURE == 5 ~ 5, W9DTENURE == 6 ~ 4, W9DTENURE == 7 ~ 6,
      W9DTENURE == -8 ~ 10, TRUE ~ 9
    )
  )

final_vars <- c("NSID", 
                "hown14", "hown15", "hown16", "hown17", "hown18", "hown19", "hown20", "hown25", "hown32",
                "hownteen14", "hownteen15", "hownteen16", "hownteen17", "hownteen18", "hownteen19", "hownteen20")

cleaned_data <- cleaned_data %>% select(all_of(final_vars))

write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)

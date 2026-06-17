library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab',
  'wave_five_lsype_family_background_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns9_2022_derived_variables.tab'
)

load_file_fixed <- function(f) {
  readr::read_delim(paste0('data/input/', f), delim = '\t', show_col_types = FALSE) %>%
    mutate(NSID = as.character(NSID))
}

data_list <- lapply(files, load_file_fixed)

# Merge all using purrr::reduce
full_frame <- data_list %>% 
  reduce(full_join, by = 'NSID')

# Helper for missing values
standardize_missing <- function(val, is_sweep_8_9 = FALSE) {
  if (is.na(val)) return(-3)
  if (!is_sweep_8_9 && val == -1) return(-8)
  if (val == -92) return(-9)
  if (val == -91) return(-1)
  if (val %in% c(-99, -999, -998, -997, -995)) return(-2)
  return(val)
}

# Function to process sweeps 1-4
process_teen_sweep <- function(var_name) {
  vals <- full_frame[[var_name]]
  detailed <- sapply(vals, function(x) {
    if (is.na(x)) return(-3)
    if (x >= 1 && x <= 8) return(x)
    standardize_missing(x)
  })
  collapsed <- sapply(vals, function(x) {
    if (is.na(x)) return(-3)
    if (x >= 1 && x <= 3) return(x)
    if (x >= 4 && x <= 6) return(4)
    if (x == 7) return(5)
    if (x == 8) return(6)
    standardize_missing(x)
  })
  return(list(detailed = detailed, collapsed = collapsed))
}

res14 <- process_teen_sweep('W1hous12HH')
res15 <- process_teen_sweep('W2Hous12HH')
res16 <- process_teen_sweep('W3hous12HH')
res17 <- process_teen_sweep('W4Hous12HH')

# Process Sweeps 5-7
process_adult_teen_sweep <- function(type_var, own_var, rent_var) {
  type_vals <- full_frame[[type_var]]
  own_vals <- full_frame[[own_var]]
  rent_vals <- full_frame[[rent_var]]
  res_detailed <- numeric(nrow(full_frame))
  res_collapsed <- numeric(nrow(full_frame))
  for (i in 1:nrow(full_frame)) {
    t <- type_vals[i]; o <- own_vals[i]; r <- rent_vals[i]
    val_det <- NA
    if (!is.na(o) && o == 4) { val_det <- 8
    } else if (!is.na(r) && r == 5) { val_det <- 8
    } else if (!is.na(o) && o >= 1 && o <= 3) { val_det <- o
    } else if (!is.na(r) && r >= 1 && r <= 4) { val_det <- r + 3
    } else {
      m_val <- if (!is.na(o)) o else if (!is.na(r)) r else t
      val_det <- standardize_missing(m_val)
    }
    val_col <- NA
    if (!is.na(val_det) && val_det >= 1 && val_det <= 8) {
      if (val_det >= 1 && val_det <= 3) val_col <- val_det
      else if (val_det >= 4 && val_det <= 6) val_col <- 4
      else if (val_det == 7) val_col <- 5
      else if (val_det == 8) val_col <- 6
    } else { val_col <- val_det }
    res_detailed[i] <- val_det; res_collapsed[i] <- val_col
  }
  return(list(detailed = res_detailed, collapsed = res_collapsed))
}

res18 <- process_adult_teen_sweep('W5Hous12HH', 'W5Hous12BHH', 'W5Hous12CHH')
res19 <- process_adult_teen_sweep('W6Hous12YP', 'W6Hous12bYP', 'W6Hous12cYP')
res20 <- process_adult_teen_sweep('W7Hous12YP', 'W7Hous12bYP', 'W7Hous12cYP')

# Process Sweeps 8-9
process_final_sweep <- function(var_name) {
  vals <- full_frame[[var_name]]
  sapply(vals, function(x) {
    if (is.na(x)) return(-3)
    if (x >= 1 && x <= 5) return(x)
    if (x == 6 || x == 7) return(6)
    standardize_missing(x, is_sweep_8_9 = TRUE)
  })
}

res25 <- process_final_sweep('W8TENURE')
res32 <- process_final_sweep('W9DTENURE')

final_df <- full_frame %>% select(NSID)
final_df$hownteen14 <- res14$detailed; final_df$hown14 <- res14$collapsed
final_df$hownteen15 <- res15$detailed; final_df$hown15 <- res15$collapsed
final_df$hownteen16 <- res16$detailed; final_df$hown16 <- res16$collapsed
final_df$hownteen17 <- res17$detailed; final_df$hown17 <- res17$collapsed
final_df$hownteen18 <- res18$detailed; final_df$hown18 <- res18$collapsed
final_df$hownteen19 <- res19$detailed; final_df$hown19 <- res19$collapsed
final_df$hownteen20 <- res20$detailed; final_df$hown20 <- res20$collapsed
final_df$hown25 <- res25; final_df$hown32 <- res32

readr::write_csv(final_df, 'data/output/cleaned_data.csv')
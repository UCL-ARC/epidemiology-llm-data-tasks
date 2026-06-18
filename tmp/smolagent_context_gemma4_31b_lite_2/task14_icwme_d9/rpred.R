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

data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character()))) 
names(data_list) <- files

# Full cohort frame construction
full_frame <- data_list[[1]] %>% 
  select(NSID) %>% 
  full_join(data_list[[2]], by = 'NSID') %>% 
  full_join(data_list[[3]], by = 'NSID') %>% 
  full_join(data_list[[4]], by = 'NSID') %>% 
  full_join(data_list[[5]], by = 'NSID') %>% 
  full_join(data_list[[6]], by = 'NSID') %>% 
  full_join(data_list[[7]], by = 'NSID') %>% 
  full_join(data_list[[8]], by = 'NSID') %>% 
  full_join(data_list[[9]], by = 'NSID')

# Helper for missing values
harmonise_missing <- function(x) {
  if (is.null(x)) return(rep(-3, nrow(full_frame)))
  res <- x
  res[x == -92] <- -9
  res[x == -91] <- -1
  res[x == -1] <- -8
  res[x == -999] <- -2
  res[x == -998] <- -2
  res[x == -997] <- -2
  res[x == -995] <- -2
  res[x == -99] <- -3
  res[is.na(res)] <- -3
  return(res)
}

# 14-17 Logic
process_simple <- function(df, var) {
  if (!var %in% names(df)) return(rep(-3, nrow(df)))
  val <- df[[var]]
  return(harmonise_missing(val))
}

# 18-20 Logic
process_nested <- function(df, main_var, owned_var, rented_var) {
  if (!main_var %in% names(df)) return(rep(-3, nrow(df)))
  
  main <- df[[main_var]]
  owned <- if (owned_var %in% names(df)) df[[owned_var]] else rep(NA, length(main))
  rented <- if (rented_var %in% names(df)) df[[rented_var]] else rep(NA, length(main))
  
  res <- rep(NA, length(main))
  
  owned_idx <- which(main == 1)
  res[owned_idx] <- owned[owned_idx]
  
  rented_idx <- which(main == 2)
  res[rented_idx] <- rented[rented_idx] + 3
  
  else_idx <- which(main == 3)
  res[else_idx] <- 8
  
  missing_idx <- which(main <= 0 | is.na(main))
  res[missing_idx] <- main[missing_idx]
  
  return(harmonise_missing(res))
}

# Adult Logic
process_adult <- function(df, var) {
  if (!var %in% names(df)) return(rep(-3, nrow(df)))
  val <- df[[var]]
  res <- val
  res[val == -9] <- -9
  res[val == -8] <- -8
  res[val == -1] <- -1
  res[is.na(res)] <- -3
  return(res)
}

# Generate Detailed variables
hownteen14 <- process_simple(full_frame, 'W1hous12HH')
hownteen15 <- process_simple(full_frame, 'W2Hous12HH')
hownteen16 <- process_simple(full_frame, 'W3hous12HH')
hownteen17 <- process_simple(full_frame, 'W4hous12HH')
hownteen18 <- process_nested(full_frame, 'W5Hous12HH', 'W5Hous12BHH', 'W5Hous12CHH')
hownteen19 <- process_nested(full_frame, 'W6Hous12YP', 'W6Hous12bYP', 'W6Hous12cYP')
hownteen20 <- process_nested(full_frame, 'W7Hous12YP', 'W7Hous12bYP', 'W7Hous12cYP')
hownteen25 <- process_adult(full_frame, 'W8TENURE')
hownteen32 <- process_adult(full_frame, 'W9DTENURE')

# Collapsed Logic
collapse_tenure <- function(detailed) {
  res <- detailed
  res[detailed == 4 | detailed == 5 | detailed == 6] <- 4
  res[detailed == 7] <- 5
  res[detailed == 8] <- 6
  return(res)
}

collapse_adult <- function(detailed) {
  res <- detailed
  res[detailed == 4] <- 4
  res[detailed == 5] <- 5
  res[detailed == 6] <- 4 # Squatting as Rent
  res[detailed == 7] <- 6 # Other
  return(res)
}

hown14 <- collapse_tenure(hownteen14)
hown15 <- collapse_tenure(hownteen15)
hown16 <- collapse_tenure(hownteen16)
hown17 <- collapse_tenure(hownteen17)
hown18 <- collapse_tenure(hownteen18)
hown19 <- collapse_tenure(hownteen19)
hown20 <- collapse_tenure(hownteen20)
hown25 <- collapse_adult(hownteen25)
hown32 <- collapse_adult(hownteen32)

# Assemble Final Dataframe
final_df <- data.frame(NSID = full_frame$NSID)
final_df$hownteen14 <- hownteen14
final_df$hownteen15 <- hownteen15
final_df$hownteen16 <- hownteen16
final_df$hownteen17 <- hownteen17
final_df$hownteen18 <- hownteen18
final_df$hownteen19 <- hownteen19
final_df$hownteen20 <- hownteen20
final_df$hownteen25 <- hownteen25
final_df$hownteen32 <- hownteen32
final_df$hown14 <- hown14
final_df$hown15 <- hown15
final_df$hown16 <- hown16
final_df$hown17 <- hown17
final_df$hown18 <- hown18
final_df$hown19 <- hown19
final_df$hown20 <- hown20
final_df$hown25 <- hown25
final_df$hown32 <- hown32

write_csv(final_df, 'data/output/cleaned_data.csv')

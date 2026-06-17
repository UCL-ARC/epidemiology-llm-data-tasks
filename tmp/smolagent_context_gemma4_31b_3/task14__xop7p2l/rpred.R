library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
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

data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t', col_types = readr::cols()))
names(data_list) <- files

# Merge datasets
full_data <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_data <- full_join(full_data, data_list[[i]], by = 'NSID')
}

# Helper for missing value mapping (Sweeps 1-7)
map_missing_17 <- function(x) {
  x <- as.numeric(x)
  x[x == -1] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -99] <- -3
  x[x == -999 | x == -998 | x == -997 | x == -995] <- -2
  x[is.na(x)] <- -3
  return(x)
}

# Helper for missing value mapping (Sweeps 8-9)
map_missing_89 <- function(x) {
  x <- as.numeric(x)
  x[x == -9] <- -9
  x[x == -8] <- -8
  x[x == -1] <- -1
  x[is.na(x)] <- -3
  return(x)
}

# Harmonisation Logic
# Detailed: 1: Owned outright, 2: Mortgage, 3: Shared, 4: Council, 5: HA, 6: Private, 7: Rent free, 8: Other
# Collapsed: 1: Owned outright, 2: Mortgage, 3: Shared, 4: Rent it, 5: Rent free, 6: Other

process_sweep_14 <- function(val) {
  det <- map_missing_17(val)
  col <- det
  col[det >= 4 & det <= 6] <- 4
  col[det == 8] <- 6
  return(list(detailed = det, collapsed = col))
}

process_sweep_15 <- function(val) {
  process_sweep_14(val)
}

process_sweep_16 <- function(val) {
  process_sweep_14(val)
}

process_sweep_17 <- function(val) {
  process_sweep_14(val)
}

process_complex_sweep <- function(type, owned, rented) {
  # detailed 8-category scheme based on labels
  # type: 1=Owned, 2=Rented, 3=Something else
  # owned: 1:Outright, 2:Mortgage, 3:Shared, 4:Other
  # rented: 1:Council, 2:HA, 3:Private, 4:Rent free, 5:Other
  
  type <- map_missing_17(type)
  owned <- map_missing_17(owned)
  rented <- map_missing_17(rented)
  
  res_det <- rep(NA, length(type))
  res_col <- rep(NA, length(type))
  
  for(i in seq_along(type)) {
    val_det <- NA
    if (type[i] == 1) {
      val_det <- owned[i]
    } else if (type[i] == 2) {
      # rented 1-3 -> 4-6, 4 -> 7, 5 -> 8
      if (rented[i] >= 1 && rented[i] <= 3) val_det <- rented[i] + 3
      else if (rented[i] == 4) val_det <- 7
      else if (rented[i] == 5) val_det <- 8
      else val_det <- rented[i] # keep missing
    } else if (type[i] == 3) {
      val_det <- 8
    } else {
      # Handle missing based on subtype priority
      val_det <- if(!is.na(owned[i]) && owned[i] < 0) owned[i] else if(!is.na(rented[i]) && rented[i] < 0) rented[i] else type[i]
    }
    
    # If we found 'Some other arrangement' (8) in any source
    if ((type[i] == 3) || (type[i] == 1 && owned[i] == 4) || (type[i] == 2 && rented[i] == 5)) {
      val_det <- 8
    }

    res_det[i] <- val_det
    
    # Collapsed
    if (is.na(res_det[i]) || res_det[i] < 0) {
      res_col[i] <- res_det[i]
    } else if (res_det[i] >= 4 && res_det[i] <= 6) {
      res_col[i] <- 4
    } else if (res_det[i] == 8) {
      res_col[i] <- 6
    } else {
      res_col[i] <- res_det[i]
    }
  }
  
  # Final missing cleanup
  res_det[is.na(res_det)] <- -3
  res_col[is.na(res_col)] <- -3
  
  return(list(detailed = res_det, collapsed = res_col))
}

process_sweep_89 <- function(val) {
  det <- map_missing_89(val)
  col <- det
  # 6: Squatting, 7: Other -> both map to 6 in collapsed
  col[det == 6 | det == 7] <- 6
  return(list(collapsed = col))
}

# Apply logic
res14 <- process_sweep_14(full_data$W1hous12HH)
res15 <- process_sweep_15(full_data$W2Hous12HH)
res16 <- process_sweep_16(full_data$W3hous12HH)
res17 <- process_sweep_17(full_data$W4Hous12HH)
res18 <- process_complex_sweep(full_data$W5Hous12HH, full_data$W5Hous12BHH, full_data$W5Hous12CHH)
res19 <- process_complex_sweep(full_data$W6Hous12YP, full_data$W6Hous12bYP, full_data$W6Hous12cYP)
res20 <- process_complex_sweep(full_data$W7Hous12YP, full_data$W7Hous12bYP, full_data$W7Hous12cYP)
res25 <- process_sweep_89(full_data$W8TENURE)
res32 <- process_sweep_89(full_data$W9DTENURE)

# Create final dataframe
final_df <- data.frame(NSID = full_data$NSID)

# Detailed (14-20)
final_df$hownteen14 <- res14$detailed
final_df$hownteen15 <- res15$detailed
final_df$hownteen16 <- res16$detailed
final_df$hownteen17 <- res17$detailed
final_df$hownteen18 <- res18$detailed
final_df$hownteen19 <- res19$detailed
final_df$hownteen20 <- res20$detailed

# Collapsed (14-32)
final_df$hown14 <- res14$collapsed
final_df$hown15 <- res15$collapsed
final_df$hown16 <- res16$collapsed
final_df$hown17 <- res17$collapsed
final_df$hown18 <- res18$collapsed
final_df$hown19 <- res19$collapsed
final_df$hown20 <- res20$collapsed
final_df$hown25 <- res25$collapsed
final_df$hown32 <- res32$collapsed

# Factors and Labels
labels_det <- c(
  '1' = 'Owned outright',
  '2' = 'Being bought on a mortgage/ bank loan',
  '3' = 'Shared ownership (owns & rents property)',
  '4' = 'Rented from a Council or New Town',
  '5' = 'Rented from a Housing Association',
  '6' = 'Rented privately',
  '7' = 'Rent free',
  '8' = 'Some other arrangement',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know / insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

labels_col <- c(
  '1' = 'Owned outright',
  '2' = 'Being bought on a mortgage/ bank loan',
  '3' = 'Shared ownership (owns & rents property)',
  '4' = 'Rent it',
  '5' = 'Rent free',
  '6' = 'Other',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know / insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

# Apply labels as factors
teen_vars <- paste0('hownteen', c('14', '15', '16', '17', '18', '19', '20'))
for (v in teen_vars) {
  final_df[[v]] <- factor(final_df[[v]], levels = as.numeric(names(labels_det)), labels = labels_det)
}

all_vars <- paste0('hown', c('14', '15', '16', '17', '18', '19', '20', '25', '32'))
for (v in all_vars) {
  final_df[[v]] <- factor(final_df[[v]], levels = as.numeric(names(labels_col)), labels = labels_col)
}

write_csv(final_df, 'data/output/cleaned_data.csv')

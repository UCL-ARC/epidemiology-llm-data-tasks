library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. Load all files
files <- list(
  w1 = 'data/input/wave_one_lsype_family_background_2020.tab',
  w2 = 'data/input/wave_two_lsype_family_background_2020.tab',
  w3 = 'data/input/wave_three_lsype_family_background_2020.tab',
  w4 = 'data/input/wave_four_lsype_family_background_2020.tab',
  w5 = 'data/input/wave_five_lsype_family_background_2020.tab',
  w6 = 'data/input/wave_six_lsype_young_person_2020.tab',
  w7 = 'data/input/wave_seven_lsype_young_person_2020.tab',
  w8 = 'data/input/ns8_2015_main_interview.tab',
  w9 = 'data/input/ns9_2022_derived_variables.tab'
)

load_data <- function(path) {
  readr::read_delim(path, delim = "\t", col_types = readr::cols(.default = "numeric"), guess = 0)
}

# We need to handle NSID as string
load_data_with_nsid <- function(path) {
  readr::read_delim(path, delim = "\t", col_types = readr::cols(NSID = readr::col_character(), .default = "numeric"), guess = 0)
}

data_list <- map(files, load_data_with_nsid)

# Merge datasets
full_df <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_df <- full_join(full_df, data_list[[i]], by = "NSID")
}

# Standard Missing Value Mapping
# -9 Refusal, -8 Don't know, -7 Prefer not to say, -3 Not asked, -2 Schedule not applicable, -1 Not applicable
map_missing <- function(val, wave_group) {
  if (is.na(val)) return(-3)
  
  # Wave group specific: in waves 1-7, -1 (Don't know) -> -8
  if (wave_group == "1-7" && val == -1) return(-8)
  
  # Common patterns
  if (val == -92) return(-9)
  if (val == -91) return(-1)
  if (val == -99) return(-3)
  if (val %in% c(-999, -998, -997, -995)) return(-2)
  
  # Specific to wave 8-9: -1 remains -1
  if (wave_group == "8-9" && val == -1) return(-1)
  
  return(val)
}

# Processing function for Waves 1-4
process_early_wave <- function(df, var_name, age) {
  detailed_name <- paste0("hownteen", age)
  collapsed_name <- paste0("hown", age)
  
  vals <- df[[var_name]]
  
  # Detailed (8-cat)
  detailed <- map_dbl(vals, ~map_missing(.x, "1-7"))
  
  # Collapsed (6-cat)
  # 1: Own outright, 2: Mortgage, 3: Shared, 4-6: Rent it, 7: Rent free, 8: Other
  collapsed <- detailed
  collapsed[detailed %in% c(4, 5, 6)] <- 4
  collapsed[detailed == 8] <- 6
  
  return(list(detailed = detailed, collapsed = collapsed))
}

# Processing function for Waves 5-7
process_mid_wave <- function(df, type_var, own_var, rent_var, age) {
  collapsed_name <- paste0("hown", age)
  
  type_vals <- df[[type_var]]
  own_vals <- df[[own_var]]
  rent_vals <- df[[rent_var]]
  
  res_collapsed <- numeric(length(type_vals))
  
  for (i in 1:length(type_vals)) {
    t <- type_vals[i]
    o <- own_vals[i]
    r <- rent_vals[i]
    
    # Logic: priority owned-subtype before rented-subtype
    # "Some other arrangement" from any source -> 8 (which collapses to 6)
    # Check subtypes first for "Some other arrangement"
    if (!is.na(o) && o == 4) { res_collapsed[i] <- 6; next }
    if (!is.na(r) && r == 5) { res_collapsed[i] <- 6; next }
    if (!is.na(t) && t == 3) { res_collapsed[i] <- 6; next }
    
    # Route by type
    if (!is.na(t) && t == 1) {
      val <- o
      if (is.na(val)) { res_collapsed[i] <- -3 } else {
        # Mapping owned subtypes: 1: Outright, 2: Mortgage, 3: Shared
        res_collapsed[i] <- val
      }
    } else if (!is.na(t) && t == 2) {
      val <- r
      if (is.na(val)) { res_collapsed[i] <- -3 } else {
        # Mapping rented subtypes: 1,2,3 -> Rent it (4), 4 -> Rent free (5)
        if (val %in% c(1, 2, 3)) res_collapsed[i] <- 4
        else if (val == 4) res_collapsed[i] <- 5
        else res_collapsed[i] <- 6
      }
    } else {
      # Preserve missing code from subtypes
      val <- if (!is.na(o)) o else if (!is.na(r)) r else t
      res_collapsed[i] <- map_missing(val, "1-7")
    }
  }
  return(list(collapsed = res_collapsed))
}

# Processing function for Waves 8-9
process_late_wave <- function(df, var_name, age) {
  collapsed_name <- paste0("hown", age)
  vals <- df[[var_name]]
  
  collapsed <- map_dbl(vals, ~map_missing(.x, "8-9"))
  # Squatting (6) and Other (7) collapse to 6
  collapsed[collapsed == 7] <- 6
  # Note: 6 is already Squatting, and 7 is Other. Both become 6.
  
  return(list(collapsed = collapsed))
}

# Apply logic
# Wave 1 (14)
res1 <- process_early_wave(full_df, "W1hous12HH", 14)
full_df$hownteen14 <- res1$detailed
full_df$hown14 <- res1$collapsed

# Wave 2 (15)
res2 <- process_early_wave(full_df, "W2Hous12HH", 15)
full_df$hownteen15 <- res2$detailed
full_df$hown15 <- res2$collapsed

# Wave 3 (16)
res3 <- process_early_wave(full_df, "W3hous12HH", 16)
full_df$hownteen16 <- res3$detailed
full_df$hown16 <- res3$collapsed

# Wave 4 (17)
res4 <- process_early_wave(full_df, "W4Hous12HH", 17)
full_df$hownteen17 <- res4$detailed
full_df$hown17 <- res4$collapsed

# Wave 5 (18)
res5 <- process_mid_wave(full_df, "W5Hous12HH", "W5Hous12BHH", "W5Hous12CHH", 18)
full_df$hown18 <- res5$collapsed

# Wave 6 (19)
res6 <- process_mid_wave(full_df, "W6Hous12YP", "W6Hous12bYP", "W6Hous12cYP", 19)
full_df$hown19 <- res6$collapsed

# Wave 7 (20)
res7 <- process_mid_wave(full_df, "W7Hous12YP", "W7Hous12bYP", "W7Hous12cYP", 20)
full_df$hown20 <- res7$collapsed

# Wave 8 (25)
res8 <- process_late_wave(full_df, "W8TENURE", 25)
full_df$hown25 <- res8$collapsed

# Wave 9 (32)
res9 <- process_late_wave(full_df, "W9DTENURE", 32)
full_df$hown32 <- res9$collapsed

# Final Selection
final_vars <- c("NSID", 
                "hownteen14", "hownteen15", "hownteen16", "hownteen17",
                "hown14", "hown15", "hown16", "hown17", "hown18", "hown19", "hown20", "hown25", "hown32")

final_df <- full_df %>% select(all_of(final_vars))

# Labels for collapsed (6-cat)
# 1: Own outright, 2: Own mortgage, 3: Shared, 4: Rent it, 5: Rent free, 6: Other
collapsed_labels <- c("-9" = "Refusal", "-8" = "Don't know", "-7" = "Prefer not to say", 
                       "-3" = "Not asked", "-2" = "Schedule not applicable", "-1" = "Not applicable",
                       "1" = "Own outright", "2" = "Own mortgage", "3" = "Shared", 
                       "4" = "Rent it", "5" = "Rent free", "6" = "Other")

# Labels for detailed (8-cat)
detailed_labels <- c("-9" = "Refusal", "-8" = "Don't know", "-7" = "Prefer not to say", 
                     "-3" = "Not asked", "-2" = "Schedule not applicable", "-1" = "Not applicable",
                     "1" = "Owned outright", "2" = "Being bought on a mortgage/ bank loan", "3" = "Shared ownership", 
                     "4" = "Rented Council", "5" = "Rented HA", "6" = "Rented privately", "7" = "Rent free", "8" = "Other")

# Apply labels
for (v in final_vars) {
  if (grepl("hownteen", v)) {
    final_df[[v]] <- factor(final_df[[v]], levels = as.numeric(names(detailed_labels)), labels = detailed_labels)
  } else if (v != "NSID") {
    final_df[[v]] <- factor(final_df[[v]], levels = as.numeric(names(collapsed_labels)), labels = collapsed_labels)
  }
}

readr::write_csv(final_df, "data/output/cleaned_data.csv")

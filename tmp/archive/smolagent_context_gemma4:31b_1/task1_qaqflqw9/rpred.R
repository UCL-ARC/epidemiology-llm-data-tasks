library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Variable mapping from metadata
files_meta <- list(
  'wave_one_lsype_young_person_2020.tab' = 'W1sexYP',
  'wave_two_lsype_young_person_2020.tab' = 'W2SexYP',
  'wave_three_lsype_young_person_2020.tab' = 'W3sexYP',
  'wave_four_lsype_young_person_2020.tab' = 'W4SexYP',
  'wave_five_lsype_young_person_2020.tab' = 'W5SexYP',
  'wave_six_lsype_young_person_2020.tab' = 'W6Sex',
  'wave_seven_lsype_young_person_2020.tab' = 'W7Sex',
  'ns8_2015_main_interview.tab' = 'W8CMSEX',
  'ns9_2022_main_interview.tab' = 'W9DSEX'
)

harmonize_missing <- function(x) {
  x <- as.numeric(x)
  # Mapping based on instruction 3 and 5
  x[x == -99] <- -3
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -998] <- -8
  x[x == -997] <- -8
  x[x == -995] <- -8
  x[x == -9] <- -9
  x[x == -8] <- -8
  x[is.na(x)] <- -3
  return(x)
}

# Helper to load and clean
load_and_clean <- function(fname, vname) {
  temp <- readr::read_delim(paste0('data/input/', fname), delim = '\t', col_types = readr::cols(.default = 'c')) %>%
    select(NSID, all_of(vname)) %>%
    mutate(NSID = as.character(NSID)) %>%
    filter(!is.na(NSID))
  
  temp[[vname]] <- harmonize_missing(temp[[vname]])
  return(temp)
}

# Process files
all_data_list <- map2(names(files_meta), files_meta, load_and_clean)

# Merge
df <- all_data_list %>% reduce(full_join, by = 'NSID')

# Priority logic for 'sex'
sex_vars <- unlist(files_meta)
sex_ordered <- rev(sex_vars)

get_best_sex <- function(row_vals) {
  # row_vals is a named vector of the sex variables for one observation
  for(var in sex_ordered) {
    val <- row_vals[var]
    if(!is.na(val) && val > 0) return(val)
  }
  for(var in sex_ordered) {
    val <- row_vals[var]
    if(!is.na(val)) return(val)
  }
  return(-3)
}

# Use t() to ensure the apply function passes a named vector to the helper
sex_matrix <- as.matrix(df[, sex_vars])
df$sex_val <- apply(sex_matrix, 1, get_best_sex)

df$sex <- factor(df$sex_val, 
                 levels = c(1, 2, -9, -8, -1, -3),
                 labels = c('Male', 'Female', 'Refusal', 'Don\'t know', 'Not applicable', 'Not asked'))

final_data <- df %>% select(NSID, sex)
write.csv(final_data, 'data/output/cleaned_data.csv', row.names = FALSE)

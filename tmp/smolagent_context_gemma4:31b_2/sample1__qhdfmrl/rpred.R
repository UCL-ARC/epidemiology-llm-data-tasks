library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define standard missing codes
# -9 = Refusal
# -8 = Don't know/insufficient information
# -1 = Item not applicable
# -3 = Not asked/interviewed/Null

harmonize_missing <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  res <- x
  res[x == -99] <- -3
  res[x == -92] <- -9
  res[x == -91] <- -1
  res[x == -1] <- -8
  res[x <= -995] <- -3
  return(res)
}

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

load_minimal <- function(f, var_name) {
  # Read with col_types = 'd' might be causing issues if NSID is not numeric
  # Use read_delim and then explicitly convert
  df <- read_delim(paste0('data/input/', f), delim = '\t')
  # Select NSID and the target variable, then ensure NSID is treated as a key
  df <- df %>% select(NSID, all_of(var_name))
  # Remove duplicates of NSID to avoid many-to-many join explosion
  df <- df %>% distinct(NSID, .keep_all = TRUE)
  return(df)
}

full_df <- NULL
for (f in names(files_meta)) {
  var_name <- files_meta[[f]]
  temp_df <- load_minimal(f, var_name)
  if (is.null(full_df)) {
    full_df <- temp_df
  } else {
    full_df <- full_join(full_df, temp_df, by = 'NSID')
  }
}

sex_vars <- unlist(files_meta)
for(v in sex_vars) {
  if(v %in% names(full_df)) {
    full_df[[v]] <- harmonize_missing(full_df[[v]])
  }
}

sex_mat <- as.matrix(full_df[, sex_vars, drop = FALSE])
sex_final <- apply(sex_mat, 1, function(row) {
  for(i in length(row):1) {
    if(!is.na(row[i]) && row[i] > 0) return(row[i])
  }
  for(i in length(row):1) {
    if(!is.na(row[i])) return(row[i])
  }
  return(-3)
})

full_df$sex <- sex_final
full_df$sex <- factor(full_df$sex, 
                    levels = c(1, 2, -1, -3, -8, -9),
                    labels = c('Male', 'Female', 'Not applicable', 'Not asked/interviewed', 'Don\'t know', 'Refusal'))

final_df <- full_df %>% select(NSID, sex)
if(!dir.exists('data/output')) dir.create('data/output', recursive = TRUE)
write_csv(final_df, 'data/output/cleaned_data.csv')

library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
library(stringr)

# Define file paths and variable mappings
files_meta <- list(
  wave1 = list(file = 'wave_one_lsype_young_person_2020.tab', age = 14, vars = c('NSID', 'W1alceverYP', 'W1alcmonYP', 'W1alcfreqYP'),
               rename = c('ever14' = 'W1alceverYP', 'mon14' = 'W1alcmonYP', 'freq14' = 'W1alcfreqYP')),
  wave2 = list(file = 'wave_two_lsype_young_person_2020.tab', age = 15, vars = c('NSID', 'W2alceverYP', 'W2alcfreqYP'),
               rename = c('ever15' = 'W2alceverYP', 'freq15' = 'W2alcfreqYP')),
  wave3 = list(file = 'wave_three_lsype_young_person_2020.tab', age = 16, vars = c('NSID', 'W3alceverYP', 'W3alcfreqYP'),
               rename = c('ever16' = 'W3alceverYP', 'freq16' = 'W3alcfreqYP')),
  wave4 = list(file = 'wave_four_lsype_young_person_2020.tab', age = 17, vars = c('NSID', 'W4AlcEverYP', 'W4AlcFreqYP'),
               rename = c('ever17' = 'W4AlcEverYP', 'freq17' = 'W4AlcFreqYP')),
  wave6 = list(file = 'wave_six_lsype_young_person_2020.tab', age = 19, vars = c('NSID', 'W6AlcEverYP', 'W6AlcFreqYP'),
               rename = c('ever19' = 'W6AlcEverYP', 'freq19' = 'W6AlcFreqYP')),
  wave7 = list(file = 'wave_seven_lsype_young_person_2020.tab', age = 20, vars = c('NSID', 'W7AlcEverYP', 'W7AlcFreqYP'),
               rename = c('ever20' = 'W7AlcEverYP', 'freq20' = 'W7AlcFreqYP')),
  wave8 = list(file = 'ns8_2015_self_completion.tab', age = 25, vars = c('NSID', 'W8AUDIT1'),
               rename = c('freq25' = 'W8AUDIT1')),
  wave9 = list(file = 'ns9_2022_main_interview.tab', age = 32, vars = c('NSID', 'W9AUDIT1'),
               rename = c('freq32' = 'W9AUDIT1'))
)

# Loading and merging
merged_df <- NULL

for (wave in files_meta) {
  path <- paste0('data/input/', wave$file)
  if (file.exists(path)) {
    df <- read_delim(path, delim = "\t", col_types = cols(NSID = "character", .default = "d"))
    
    # Keep only needed variables
    available_vars <- intersect(names(df), wave$vars)
    df <- df %>% select(all_of(available_vars))
    
    # Remove duplicates of NSID to prevent join explosion
    df <- df %>% distinct(NSID, .keep_all = TRUE)
    
    # Rename variables
    rename_map <- wave$rename[wave$rename %in% names(df)]
    df <- df %>% rename(!!!rename_map)
    
    if (is.null(merged_df)) {
      merged_df <- df
    } else {
      merged_df <- full_join(merged_df, df, by = 'NSID')
    }
  } else {
    if (is.null(merged_df)) {
       merged_df <- data.frame(NSID = character())
    }
    for (new_name in names(wave$rename)) {
      merged_df[[new_name]] <- NA_real_
    }
  }
}

# Cleaning values
clean_val <- function(x) {
  if(is.numeric(x)) {
    x[x <= -1] <- NA
  }
  return(x)
}

merged_df <- merged_df %>% mutate(across(-NSID, clean_val))

# Logic for drinking
get_col <- function(df, colname) {
  if (colname %in% names(df)) return(df[[colname]]) else return(rep(NA_real_, nrow(df)))
}

drink14 <- (get_col(merged_df, 'ever14') == 1 & get_col(merged_df, 'mon14') == 1)

check_drinker <- function(ever, freq) {
  (ever == 1) | (!is.na(freq))
}

drink15 <- check_drinker(get_col(merged_df, 'ever15'), get_col(merged_df, 'freq15'))
drink16 <- check_drinker(get_col(merged_df, 'ever16'), get_col(merged_df, 'freq16'))
drink17 <- check_drinker(get_col(merged_df, 'ever17'), get_col(merged_df, 'freq17'))
drink19 <- check_drinker(get_col(merged_df, 'ever19'), get_col(merged_df, 'freq19'))
drink20 <- check_drinker(get_col(merged_df, 'ever20'), get_col(merged_df, 'freq20'))
drink25 <- (!is.na(get_col(merged_df, 'freq25')) & get_col(merged_df, 'freq25') > 1)
drink32 <- (!is.na(get_col(merged_df, 'freq32')) & get_col(merged_df, 'freq32') > 1)

ages <- c(14, 15, 16, 17, 19, 20, 25, 32)
results <- cbind(drink14, drink15, drink16, drink17, drink19, drink20, drink25, drink32)
colnames(results) <- ages

first_age <- apply(results, 1, function(row) {
  drinkers <- which(row == TRUE)
  if (length(drinkers) > 0) return(ages[drinkers[1]]) else return(NA)
})

final_alcfst <- numeric(length(first_age))

for (i in 1:nrow(merged_df)) {
  if (!is.na(first_age[i])) {
    final_alcfst[i] <- first_age[i]
  } else {
    cols_to_check <- c('ever14', 'mon14', 'freq14', 'ever15', 'freq15', 'ever16', 'freq16', 
                      'ever17', 'freq17', 'ever19', 'freq19', 'ever20', 'freq20', 'freq25', 'freq32')
    existing_cols <- intersect(cols_to_check, names(merged_df))
    row_vals <- merged_df[i, existing_cols]
    if (any(is.na(row_vals))) {
      final_alcfst[i] <- -8
    } else {
      final_alcfst[i] <- 99
    }
  }
}

output_df <- data.frame(NSID = merged_df$NSID, alcfst = final_alcfst)

levs <- c("14", "15", "16", "17", "19", "20", "25", "32", "99", "-8")
labls <- c("14", "15", "16", "17", "19", "20", "25", "32", "Never had alcohol", "Don't know/insufficient information")
output_df$alcfst <- factor(output_df$alcfst, levels = as.numeric(levs), labels = labls)

write_csv(output_df, "data/output/cleaned_data.csv")

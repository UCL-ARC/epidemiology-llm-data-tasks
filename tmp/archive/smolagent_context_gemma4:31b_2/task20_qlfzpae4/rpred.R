library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
library(stringr)

# Define file paths and relevant variables
files_meta <- list(
  wave_one = list(file = "wave_one_lsype_young_person_2020.tab", age = 14, vars = c("NSID", "W1alceverYP", "W1alcmonYP", "W1alcfreqYP")),
  wave_two = list(file = "wave_two_lsype_young_person_2020.tab", age = 15, vars = c("NSID", "W2alceverYP", "W2alcfreqYP")),
  wave_three = list(file = "wave_three_lsype_young_person_2020.tab", age = 16, vars = c("NSID", "W3alceverYP", "W3alcfreqYP")),
  wave_four = list(file = "wave_four_lsype_young_person_2020.tab", age = 17, vars = c("NSID", "W4AlcEverYP", "W4AlcFreqYP")),
  wave_six = list(file = "wave_six_lsype_young_person_2020.tab", age = 19, vars = c("NSID", "W6AlcEverYP", "W6AlcFreqYP")),
  wave_seven = list(file = "wave_seven_lsype_young_person_2020.tab", age = 20, vars = c("NSID", "W7AlcEverYP", "W7AlcFreqYP")),
  wave_eight = list(file = "ns8_2015_self_completion.tab", age = 25, vars = c("NSID", "W8AUDIT1", "W8AUDIT2", "W8AUDIT6")),
  wave_nine = list(file = "ns9_2022_main_interview.tab", age = 32, vars = c("NSID", "W9AUDIT1", "W9AUDIT2", "W9AUDIT3"))
)

# Load and preprocess each wave
load_wave <- function(name, meta) {
  path <- paste0("data/input/", meta$file)
  df <- read_delim(path, delim = "\t", col_types = cols(NSID = "c", .default = "d"))
  existing_vars <- intersect(names(df), meta$vars)
  df <- df %>% select(all_of(existing_vars))
  df <- df %>% rename_with(~ paste0(., "_", meta$age), .cols = -NSID)
  return(df)
}

data_list <- map2(names(files_meta), files_meta, load_wave)

# Merge datasets safely
merged_df <- reduce(data_list, function(acc, x) {
  x <- x %>% distinct(NSID, .keep_all = TRUE)
  full_join(acc, x, by = "NSID")
}, .init = data_list[[1]] %>% distinct(NSID, .keep_all = TRUE))

ages <- c(14, 15, 16, 17, 19, 20, 25, 32)

# Function to determine drinking status for a specific age
get_drinker_status <- function(df, age) {
  col_names <- names(df)
  if (age == 14) {
    c_ev <- "W1alceverYP_14"; c_mo <- "W1alcmonYP_14"
    if (!(c_ev %in% col_names && c_mo %in% col_names)) return(rep(NA, nrow(df)))
    ev <- df[[c_ev]]; mo <- df[[c_mo]]
    return(ifelse(!is.na(ev) & !is.na(mo) & ev == 1 & mo == 1, 1, ifelse(!is.na(ev) & !is.na(mo), 0, NA)))
  } else if (age == 15) {
    c_ev <- "W2alceverYP_15"; c_fr <- "W2alcfreqYP_15"
    if (!(c_ev %in% col_names || c_fr %in% col_names)) return(rep(NA, nrow(df)))
    ev <- if (c_ev %in% col_names) df[[c_ev]] else rep(NA, nrow(df))
    fr <- if (c_fr %in% col_names) df[[c_fr]] else rep(NA, nrow(df))
    return(ifelse((!is.na(ev) & ev == 1) | (!is.na(fr) & fr >= 1 & fr <= 6), 1, ifelse(!is.na(ev) | !is.na(fr), 0, NA)))
  } else if (age == 16) {
    c_ev <- "W3alceverYP_16"; c_fr <- "W3alcfreqYP_16"
    if (!(c_ev %in% col_names || c_fr %in% col_names)) return(rep(NA, nrow(df)))
    ev <- if (c_ev %in% col_names) df[[c_ev]] else rep(NA, nrow(df))
    fr <- if (c_fr %in% col_names) df[[c_fr]] else rep(NA, nrow(df))
    return(ifelse((!is.na(ev) & ev == 1) | (!is.na(fr) & fr >= 1 & fr <= 6), 1, ifelse(!is.na(ev) | !is.na(fr), 0, NA)))
  } else if (age == 17) {
    c_ev <- "W4AlcEverYP_17"; c_fr <- "W4AlcFreqYP_17"
    if (!(c_ev %in% col_names || c_fr %in% col_names)) return(rep(NA, nrow(df)))
    ev <- if (c_ev %in% col_names) df[[c_ev]] else rep(NA, nrow(df))
    fr <- if (c_fr %in% col_names) df[[c_fr]] else rep(NA, nrow(df))
    return(ifelse((!is.na(ev) & ev == 1) | (!is.na(fr) & fr >= 1 & fr <= 6), 1, ifelse(!is.na(ev) | !is.na(fr), 0, NA)))
  } else if (age == 19) {
    c_ev <- "W6AlcEverYP_19"; c_fr <- "W6AlcFreqYP_19"
    if (!(c_ev %in% col_names || c_fr %in% col_names)) return(rep(NA, nrow(df)))
    ev <- if (c_ev %in% col_names) df[[c_ev]] else rep(NA, nrow(df))
    fr <- if (c_fr %in% col_names) df[[c_fr]] else rep(NA, nrow(df))
    return(ifelse((!is.na(ev) & ev == 1) | (!is.na(fr) & fr >= 1 & fr <= 7), 1, ifelse(!is.na(ev) | !is.na(fr), 0, NA)))
  } else if (age == 20) {
    c_ev <- "W7AlcEverYP_20"; c_fr <- "W7AlcFreqYP_20"
    if (!(c_ev %in% col_names || c_fr %in% col_names)) return(rep(NA, nrow(df)))
    ev <- if (c_ev %in% col_names) df[[c_ev]] else rep(NA, nrow(df))
    fr <- if (c_fr %in% col_names) df[[c_fr]] else rep(NA, nrow(df))
    return(ifelse((!is.na(ev) & ev == 1) | (!is.na(fr) & fr >= 1 & fr <= 7), 1, ifelse(!is.na(ev) | !is.na(fr), 0, NA)))
  } else if (age == 25) {
    c_a1 <- "W8AUDIT1_25"; c_a2 <- "W8AUDIT2_25"; c_a6 <- "W8AUDIT6_25"
    if (!(c_a1 %in% col_names || c_a2 %in% col_names || c_a6 %in% col_names)) return(rep(NA, nrow(df)))
    a1 <- if (c_a1 %in% col_names) df[[c_a1]] else rep(NA, nrow(df))
    a2 <- if (c_a2 %in% col_names) df[[c_a2]] else rep(NA, nrow(df))
    a6 <- if (c_a6 %in% col_names) df[[c_a6]] else rep(NA, nrow(df))
    return(ifelse((!is.na(a1) & a1 > 1) | (!is.na(a2) & a2 >= 1) | (!is.na(a6) & a6 > 1), 1, ifelse(!is.na(a1) | !is.na(a2) | !is.na(a6), 0, NA)))
  } else if (age == 32) {
    c_a1 <- "W9AUDIT1_32"; c_a2 <- "W9AUDIT2_32"; c_a3 <- "W9AUDIT3_32"
    if (!(c_a1 %in% col_names || c_a2 %in% col_names || c_a3 %in% col_names)) return(rep(NA, nrow(df)))
    a1 <- if (c_a1 %in% col_names) df[[c_a1]] else rep(NA, nrow(df))
    a2 <- if (c_a2 %in% col_names) df[[c_a2]] else rep(NA, nrow(df))
    a3 <- if (c_a3 %in% col_names) df[[c_a3]] else rep(NA, nrow(df))
    return(ifelse((!is.na(a1) & a1 > 1) | (!is.na(a2) & a2 >= 1) | (!is.na(a3) & a3 > 1), 1, ifelse(!is.na(a1) | !is.na(a2) | !is.na(a3), 0, NA)))
  }
}

status_list <- map(ages, ~ get_drinker_status(merged_df, .x))
status_matrix <- do.call(cbind, status_list)
colnames(status_matrix) <- as.character(ages)

final_alcfst <- apply(status_matrix, 1, function(row) {
  drink_ages <- ages[which(row == 1)]
  if (length(drink_ages) > 0) return(min(drink_ages))
  if (all(!is.na(row)) && all(row == 0)) return(99)
  return(-8)
})

final_data <- data.frame(NSID = merged_df$NSID, alcfst = final_alcfst)
levels_alcfst <- c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8)
labels_alcfst <- c("14", "15", "16", "17", "19", "20", "25", "32", "Never had alcohol", "Don't know/insufficient information")
final_data$alcfst <- factor(final_data$alcfst, levels = levels_alcfst, labels = labels_alcfst)

write_csv(final_data, "data/output/cleaned_data.csv")

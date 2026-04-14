library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
library(stringr)

# Define file paths and variables to select
files_info <- list(
  wave1 = list(file = "data/input/wave_one_lsype_young_person_2020.tab", vars = c("NSID", "W1alceverYP", "W1alcmonYP", "W1alcfreqYP"), age = 14),
  wave2 = list(file = "data/input/wave_two_lsype_young_person_2020.tab", vars = c("NSID", "W2alceverYP", "W2alcfreqYP"), age = 15),
  wave3 = list(file = "data/input/wave_three_lsype_young_person_2020.tab", vars = c("NSID", "W3alceverYP", "W3alcfreqYP"), age = 16),
  wave4 = list(file = "data/input/wave_four_lsype_young_person_2020.tab", vars = c("NSID", "W4AlcEverYP", "W4AlcFreqYP"), age = 17),
  wave6 = list(file = "data/input/wave_six_lsype_young_person_2020.tab", vars = c("NSID", "W6AlcEverYP", "W6AlcFreqYP"), age = 19),
  wave7 = list(file = "data/input/wave_seven_lsype_young_person_2020.tab", vars = c("NSID", "W7AlcEverYP", "W7AlcFreqYP"), age = 20),
  wave8 = list(file = "data/input/ns8_2015_self_completion.tab", vars = c("NSID", "W8AUDIT1"), age = 25),
  wave9 = list(file = "data/input/ns9_2022_main_interview.tab", vars = c("NSID", "W9AUDIT1"), age = 32)
)

# Load and process data
data_list <- map(files_info, function(info) {
  # Use read_delim and ensure NSID is treated as character to avoid join issues
  df <- read_delim(info$file, delim = "\t", col_types = cols(NSID = col_character(), .default = "numeric"))
  
  # Select only variables that actually exist in the file
  available_vars <- intersect(names(df), info$vars)
  df <- df %>% select(all_of(available_vars))
  
  # Rename variables correctly
  current_names <- names(df)
  new_names <- map_chr(current_names, function(x) {
    if (x == "NSID") return("NSID")
    else return(paste0(x, "_", info$age))
  })
  names(df) <- new_names
  return(df)
})

# Merge all datasets using full_join on NSID
# To prevent the overflow error, ensure NSID is unique per file before joining
processed_list <- map(data_list, function(df) {
  df %>% distinct(NSID, .keep_all = TRUE)
})

merged_data <- reduce(processed_list, full_join, by = "NSID")

# Apply drinker flags based on rules
merged_data <- merged_data %>%
  mutate(
    d14 = if_else(W1alceverYP_14 == 1 & W1alcmonYP_14 == 1, 1, if_else(W1alceverYP_14 == 2 & W1alcmonYP_14 == 2, 0, NA_real_)),
    d15 = if_else(W2alceverYP_15 == 1 | (W2alcfreqYP_15 >= 1 & W2alcfreqYP_15 <= 6), 1, if_else(W2alceverYP_15 == 2, 0, NA_real_)),
    d16 = if_else(W3alceverYP_16 == 1 | (W3alcfreqYP_16 >= 1 & W3alcfreqYP_16 <= 6), 1, if_else(W3alceverYP_16 == 2, 0, NA_real_)),
    d17 = if_else(W4AlcEverYP_17 == 1 | (W4AlcFreqYP_17 >= 1 & W4AlcFreqYP_17 <= 6), 1, if_else(W4AlcEverYP_17 == 2, 0, NA_real_)),
    d19 = if_else(W6AlcEverYP_19 == 1 | (W6AlcFreqYP_19 >= 1 & W6AlcFreqYP_19 <= 7), 1, if_else(W6AlcEverYP_19 == 2 & (W6AlcFreqYP_19 == 8 | is.na(W6AlcFreqYP_19)), 0, NA_real_)),
    d20 = if_else(W7AlcEverYP_20 == 1 | (W7AlcFreqYP_20 >= 1 & W7AlcFreqYP_20 <= 7), 1, if_else(W7AlcEverYP_20 == 2 & (W7AlcFreqYP_20 == 8 | is.na(W7AlcFreqYP_20)), 0, NA_real_)),
    d25 = if_else(W8AUDIT1_25 > 1, 1, if_else(W8AUDIT1_25 == 1, 0, NA_real_)),
    d32 = if_else(W9AUDIT1_32 > 1, 1, if_else(W9AUDIT1_32 == 1, 0, NA_real_))
  )

# Determine alcfst
ages <- c(14, 15, 16, 17, 19, 20, 25, 32)
flags <- c("d14", "d15", "d16", "d17", "d19", "d20", "d25", "d32")

find_first_age <- function(row) {
  vals <- as.numeric(row[flags])
  if (any(vals == 1, na.rm = TRUE)) {
    first_idx <- which(vals == 1)[1]
    return(ages[first_idx])
  } else if (all(!is.na(vals)) && all(vals == 0)) {
    return(99)
  } else {
    return(-8)
  }
}

merged_data$alcfst_val <- apply(merged_data, 1, find_first_age)

# Convert to factor with labels
merged_data$alcfst <- factor(merged_data$alcfst_val, 
                             levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8), 
                             labels = c("14", "15", "16", "17", "19", "20", "25", "32", "Never had alcohol", "Don't know/insufficient information"))

# Output final dataset
final_output <- merged_data %>% select(NSID, alcfst)
write_csv(final_output, "data/output/cleaned_data.csv")
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
load_tab <- function(filename) {
  readr::read_delim(paste0('data/input/', filename), delim = '\t', col_types = readr::cols(.default = 'numeric', NSID = readr::col_character()))
}

files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_three_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_self_completion.tab',
  'ns9_2022_main_interview.tab'
)

data_list <- lapply(files, load_tab)
names(data_list) <- files

# Merge all datasets
full_df <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_df <- full_join(full_df, data_list[[i]], by = 'NSID')
}

# 2. Define drinking indicators for each age
# S1: 14, S2: 15, S3: 16, S4: 17, S6: 19, S7: 20, S8: 25, S9: 32

process_drinking <- function(df) {
  # Use df[[variable]] to avoid 'object not found' error in function scope
  
  # Wave 1: age 14
  w1_ever <- df[['W1alceverYP']]
  w1_mon  <- df[['W1alcmonYP']]
  w1_miss <- (w1_ever %in% c(-99, -97, -96, -92, -91, -1)) | (w1_mon %in% c(-99, -97, -96, -92, -91, -1))
  w1_drink <- (w1_ever == 1 & w1_mon == 1)
  w1_no <- (w1_ever == 2 | w1_mon == 2) & !w1_miss
  
  # Wave 2: age 15
  w2_ever <- df[['W2alceverYP']]
  w2_miss <- w2_ever %in% c(-998, -997, -995, -99, -97, -96, -92, -91, -1)
  w2_drink <- (w2_ever == 1)
  w2_no <- (w2_ever == 2) & !w2_miss
  
  # Wave 3: age 16
  w3_ever <- df[['W3alceverYP']]
  w3_miss <- w3_ever %in% c(-99, -97, -96, -92, -91, -1)
  w3_drink <- (w3_ever == 1)
  w3_no <- (w3_ever == 2) & !w3_miss
  
  # Wave 4: age 17
  w4_ever <- df[['W4AlcEverYP']]
  w4_miss <- w4_ever %in% c(-99, -97, -96, -92, -91, -1)
  w4_drink <- (w4_ever == 1)
  w4_no <- (w4_ever == 2) & !w4_miss
  
  # Wave 6: age 19
  w6_ever <- df[['W6AlcEverYP']]
  w6_miss <- w6_ever %in% c(-997, -97, -92, -91, -1)
  w6_drink <- (w6_ever == 1)
  w6_no <- (w6_ever == 2) & !w6_miss
  
  # Wave 7: age 20
  w7_ever <- df[['W7AlcEverYP']]
  w7_miss <- w7_ever %in% c(-996, -97, -92, -91, -1)
  w7_drink <- (w7_ever == 1)
  w7_no <- (w7_ever == 2) & !w7_miss
  
  # Wave 8: age 25 (AUDIT > 1)
  w8_audit <- df[['W8AUDIT1']]
  w8_miss <- w8_audit %in% c(-9, -8, -3, -1)
  w8_drink <- (w8_audit > 1)
  w8_no <- (w8_audit == 1) & !w8_miss
  
  # Wave 9: age 32 (AUDIT > 1)
  w9_audit <- df[['W9AUDIT1']]
  w9_miss <- w9_audit %in% c(-9, -8, -3, -1)
  w9_drink <- (w9_audit > 1)
  w9_no <- (w9_audit == 1) & !w9_miss
  
  res <- data.frame(
    age14 = ifelse(w1_drink, 1, ifelse(w1_no, 0, NA)),
    age15 = ifelse(w2_drink, 1, ifelse(w2_no, 0, NA)),
    age16 = ifelse(w3_drink, 1, ifelse(w3_no, 0, NA)),
    age17 = ifelse(w4_drink, 1, ifelse(w4_no, 0, NA)),
    age19 = ifelse(w6_drink, 1, ifelse(w6_no, 0, NA)),
    age20 = ifelse(w7_drink, 1, ifelse(w7_no, 0, NA)),
    age25 = ifelse(w8_drink, 1, ifelse(w8_no, 0, NA)),
    age32 = ifelse(w9_drink, 1, ifelse(w9_no, 0, NA))
  )
  return(res)
}

# Apply processing
indicators <- process_drinking(full_df)

# Derive alcfst
ages <- c(14, 15, 16, 17, 19, 20, 25, 32)

alcfst_vec <- apply(indicators, 1, function(row) {
  vals <- as.numeric(row)
  first_drink_idx <- which(vals == 1)
  if (length(first_drink_idx) > 0) {
    return(ages[first_drink_idx[1]])
  }
  if (any(is.na(vals))) {
    return(-8)
  } else {
    return(99)
  }
})

# Create final dataframe
final_df <- data.frame(NSID = full_df$NSID, alcfst = alcfst_vec)

# Convert alcfst to factor
level_vals <- c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8)
level_labs <- c("Age 14", "Age 15", "Age 16", "Age 17", "Age 19", "Age 20", "Age 25", "Age 32", "Never had alcohol", "Don't know/insufficient information")

final_df$alcfst <- factor(final_df$alcfst, levels = level_vals, labels = level_labs)

# Write output
readr::write_csv(final_df, 'data/output/cleaned_data.csv')

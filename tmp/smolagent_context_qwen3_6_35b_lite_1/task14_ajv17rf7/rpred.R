library(dplyr)
library(readr)
library(labelled)

# Create output directory if needed
dir.create('data/output', showWarnings = FALSE)

# Load all files from metadata
data_w1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
data_w2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
data_w3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
data_w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
data_w5 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
data_w6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
data_w7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
data_w8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t', show_col_types = FALSE)
data_w9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', show_col_types = FALSE)

# ============================================================
# Create derived tenure variables for each wave
# ============================================================

# --- Wave 1 (Age 14): W1hous12HH ---
w1_tenure <- suppressWarnings(as.numeric(as.character(data_w1$W1hous12HH)))
w1_tenure[w1_tenure == -999] <- -3
w1_tenure[w1_tenure == -92] <- -9
w1_tenure[w1_tenure == -91] <- -1
w1_tenure[w1_tenure == -1] <- -8
w1_tenure[is.na(w1_tenure)] <- -3

data_w1$hownteen14 <- w1_tenure
w1_collapsed <- w1_tenure
w1_collapsed[w1_tenure %in% c(4,5,6,7)] <- 4
data_w1$hown14 <- w1_collapsed

# --- Wave 2 (Age 15): W2Hous12HH ---
w2_tenure <- suppressWarnings(as.numeric(as.character(data_w2$W2Hous12HH)))
w2_tenure[w2_tenure == -998] <- -2
w2_tenure[w2_tenure == -997] <- -2
w2_tenure[w2_tenure == -995] <- -2
w2_tenure[w2_tenure == -99] <- -3
w2_tenure[w2_tenure == -92] <- -9
w2_tenure[w2_tenure == -91] <- -1
w2_tenure[w2_tenure == -1] <- -8
w2_tenure[is.na(w2_tenure)] <- -3

data_w2$hownteen15 <- w2_tenure
w2_collapsed <- w2_tenure
w2_collapsed[w2_tenure %in% c(4,5,6,7)] <- 4
data_w2$hown15 <- w2_collapsed

# --- Wave 3 (Age 16): W3hous12HH ---
w3_tenure <- suppressWarnings(as.numeric(as.character(data_w3$W3hous12HH)))
w3_tenure[w3_tenure == -999] <- -3
w3_tenure[w3_tenure == -99] <- -3
w3_tenure[w3_tenure == -92] <- -9
w3_tenure[w3_tenure == -91] <- -1
w3_tenure[w3_tenure == -1] <- -8
w3_tenure[is.na(w3_tenure)] <- -3

data_w3$hownteen16 <- w3_tenure
w3_collapsed <- w3_tenure
w3_collapsed[w3_tenure %in% c(4,5,6,7)] <- 4
data_w3$hown16 <- w3_collapsed

# --- Wave 4 (Age 17): W4Hous12HH ---
w4_tenure <- suppressWarnings(as.numeric(as.character(data_w4$W4Hous12HH)))
w4_tenure[w4_tenure == -999] <- -3
w4_tenure[w4_tenure == -997] <- -2
w4_tenure[w4_tenure == -92] <- -9
w4_tenure[w4_tenure == -91] <- -1
w4_tenure[w4_tenure == -1] <- -8
w4_tenure[is.na(w4_tenure)] <- -3

data_w4$hownteen17 <- w4_tenure
w4_collapsed <- w4_tenure
w4_collapsed[w4_tenure %in% c(4,5,6,7)] <- 4
data_w4$hown17 <- w4_collapsed

# --- Wave 5 (Age 18): W5Hous12HH ---
w5_tenure <- suppressWarnings(as.numeric(as.character(data_w5$W5Hous12HH)))
w5_tenure[w5_tenure == -999] <- -3
w5_tenure[w5_tenure == -92] <- -9
w5_tenure[w5_tenure == -91] <- -1
w5_tenure[w5_tenure == -1] <- -8
w5_tenure[is.na(w5_tenure)] <- -3

data_w5$hownteen18 <- w5_tenure
w5_collapsed <- w5_tenure
w5_collapsed[w5_tenure %in% c(4,5,6,7)] <- 4
data_w5$hown18 <- w5_collapsed

# --- Wave 6 (Age 19): W6Hous12YP ---
w6_tenure <- suppressWarnings(as.numeric(as.character(data_w6$W6Hous12YP)))
w6_tenure[w6_tenure == -999] <- -3
w6_tenure[w6_tenure == -92] <- -9
w6_tenure[w6_tenure == -91] <- -1
w6_tenure[w6_tenure == -1] <- -8
w6_tenure[is.na(w6_tenure)] <- -3

data_w6$hownteen19 <- w6_tenure
w6_collapsed <- w6_tenure
w6_collapsed[w6_tenure %in% c(4,5,6,7)] <- 4
data_w6$hown19 <- w6_collapsed

# --- Wave 7 (Age 20): W7Hous12YP ---
w7_tenure <- suppressWarnings(as.numeric(as.character(data_w7$W7Hous12YP)))
w7_tenure[w7_tenure == -999] <- -3
w7_tenure[w7_tenure == -92] <- -9
w7_tenure[w7_tenure == -91] <- -1
w7_tenure[w7_tenure == -1] <- -8
w7_tenure[is.na(w7_tenure)] <- -3

data_w7$hownteen20 <- w7_tenure
w7_collapsed <- w7_tenure
w7_collapsed[w7_tenure %in% c(4,5,6,7)] <- 4
data_w7$hown20 <- w7_collapsed

# --- Wave 8 (Age 25): W8TENURE ---
w8_tenure <- suppressWarnings(as.numeric(as.character(data_w8$W8TENURE)))
w8_tenure[w8_tenure == -9] <- -9
w8_tenure[w8_tenure == -8] <- -8
w8_tenure[w8_tenure == -1] <- -1
w8_tenure[is.na(w8_tenure)] <- -3

data_w8$hownteen25 <- w8_tenure
data_w8$hown25 <- w8_tenure

# --- Wave 9 (Age 32): W9DTENURE ---
w9_tenure <- suppressWarnings(as.numeric(as.character(data_w9$W9DTENURE)))
w9_tenure[w9_tenure == -8] <- -8
w9_tenure[is.na(w9_tenure)] <- -3

data_w9$hownteen32 <- w9_tenure
data_w9$hown32 <- w9_tenure

# ============================================================
# Merge all data
# ============================================================

cleaned <- data_w1 %>%
  select(NSID, hownteen14, hown14) %>%
  full_join(data_w2 %>% select(NSID, hownteen15, hown15), by = 'NSID') %>%
  full_join(data_w3 %>% select(NSID, hownteen16, hown16), by = 'NSID') %>%
  full_join(data_w4 %>% select(NSID, hownteen17, hown17), by = 'NSID') %>%
  full_join(data_w5 %>% select(NSID, hownteen18, hown18), by = 'NSID') %>%
  full_join(data_w6 %>% select(NSID, hownteen19, hown19), by = 'NSID') %>%
  full_join(data_w7 %>% select(NSID, hownteen20, hown20), by = 'NSID') %>%
  full_join(data_w8 %>% select(NSID, hownteen25, hown25), by = 'NSID') %>%
  full_join(data_w9 %>% select(NSID, hownteen32, hown32), by = 'NSID')

# ============================================================
# Write output
# ============================================================

write_csv(cleaned, 'data/output/cleaned_data.csv')

cat('Output written to data/output/cleaned_data.csv\n')
cat('Number of rows:', nrow(cleaned), '\n')
cat('Columns:', paste(names(cleaned), collapse = ', '), '\n')
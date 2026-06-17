library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files
w1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w5 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
w7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
w8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t', show_col_types = FALSE)
w9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', show_col_types = FALSE)

# === DETAILED TIME-VARYING VARIABLES (ages 14-20, prefix hownteen) ===

# Age 14 (W1)
hownteen_14 <- w1$W1hous12HH
hownteen_14[hownteen_14 %in% c(-999, -92, -91)] <- -3
hownteen_14[hownteen_14 == -1] <- -8
w1$hownteen14 <- hownteen_14

# Age 15 (W2)
hownteen_15 <- w2$W2Hous12HH
hownteen_15[hownteen_15 %in% c(-999, -99, -998, -997, -995)] <- -3
hownteen_15[hownteen_15 %in% c(-92, -91)] <- c(-9, -1)
hownteen_15[hownteen_15 == -1] <- -8
w2$hownteen15 <- hownteen_15

# Age 16 (W3)
hownteen_16 <- w3$W3hous12HH
hownteen_16[hownteen_16 == -999] <- -3
hownteen_16[hownteen_16 %in% c(-92, -91)] <- c(-9, -1)
hownteen_16[hownteen_16 == -1] <- -8
w3$hownteen16 <- hownteen_16

# Age 17 (W4)
hownteen_17 <- w4$W4Hous12HH
hownteen_17[hownteen_17 %in% c(-999, -997)] <- -3
hownteen_17[hownteen_17 %in% c(-92, -91)] <- c(-9, -1)
hownteen_17[hownteen_17 == -1] <- -8
w4$hownteen17 <- hownteen_17

# Age 18 (W5)
hownteen_18 <- w5$W5Hous12HH
hownteen_18[hownteen_18 %in% c(-999, -92, -91, -100, -6)] <- -3
hownteen_18[hownteen_18 == -1] <- -8
w5$hownteen18 <- hownteen_18

# Age 19 (W6)
hownteen_19 <- w6$W6Hous12YP
hownteen_19[hownteen_19 %in% c(-92, -91)] <- -3
hownteen_19[hownteen_19 == -1] <- -8
w6$hownteen19 <- hownteen_19

# Age 20 (W7)
hownteen_20 <- w7$W7Hous12YP
hownteen_20[hownteen_20 %in% c(-92, -91)] <- -3
hownteen_20[hownteen_20 == -1] <- -8
w7$hownteen20 <- hownteen_20

# === COLLAPSED TIME-VARYING VARIABLES (ages 14-32, prefix hown) ===

# Age 14 (W1)
hown_14 <- w1$W1hous12HH
hown_14[hown_14 %in% c(1, 2, 3)] <- 1
hown_14[hown_14 %in% c(4, 5, 6, 7, 8)] <- 2
hown_14[hown_14 %in% c(-999, -92, -91)] <- -3
hown_14[hown_14 == -1] <- -8
w1$hown14 <- hown_14

# Age 15 (W2)
hown_15 <- w2$W2Hous12HH
hown_15[hown_15 %in% c(1, 2, 3)] <- 1
hown_15[hown_15 %in% c(4, 5, 6, 7, 8)] <- 2
hown_15[hown_15 %in% c(-999, -99, -998, -997, -995)] <- -3
hown_15[hown_15 %in% c(-92, -91)] <- c(-9, -1)
hown_15[hown_15 == -1] <- -8
w2$hown15 <- hown_15

# Age 16 (W3)
hown_16 <- w3$W3hous12HH
hown_16[hown_16 %in% c(1, 2, 3)] <- 1
hown_16[hown_16 %in% c(4, 5, 6, 7, 8)] <- 2
hown_16[hown_16 == -999] <- -3
hown_16[hown_16 %in% c(-92, -91)] <- c(-9, -1)
hown_16[hown_16 == -1] <- -8
w3$hown16 <- hown_16

# Age 17 (W4)
hown_17 <- w4$W4Hous12HH
hown_17[hown_17 %in% c(1, 2, 3)] <- 1
hown_17[hown_17 %in% c(4, 5, 6, 7, 8)] <- 2
hown_17[hown_17 %in% c(-999, -997)] <- -3
hown_17[hown_17 %in% c(-92, -91)] <- c(-9, -1)
hown_17[hown_17 == -1] <- -8
w4$hown17 <- hown_17

# Age 18 (W5)
hown_18 <- w5$W5Hous12HH
hown_18[hown_18 == 1] <- 1
hown_18[hown_18 == 2] <- 2
hown_18[hown_18 == 3] <- NA_real_
hown_18[hown_18 %in% c(-999, -92, -91, -100, -6)] <- -3
hown_18[hown_18 == -1] <- -8
w5$hown18 <- hown_18

# Age 19 (W6)
hown_19 <- w6$W6Hous12YP
hown_19[hown_19 == 1] <- 1
hown_19[hown_19 == 2] <- 2
hown_19[hown_19 == 3] <- 2
hown_19[hown_19 %in% c(-92, -91)] <- -3
hown_19[hown_19 == -1] <- -8
w6$hown19 <- hown_19

# Age 20 (W7)
hown_20 <- w7$W7Hous12YP
hown_20[hown_20 == 1] <- 1
hown_20[hown_20 == 2] <- 2
hown_20[hown_20 == 3] <- 2
hown_20[hown_20 %in% c(-92, -91)] <- -3
hown_20[hown_20 == -1] <- -8
w7$hown20 <- hown_20

# Age 25 (W8)
hown_25 <- w8$W8TENURE
hown_25[hown_25 == 1] <- 1
hown_25[hown_25 == 2] <- 1
hown_25[hown_25 == 3] <- 1
hown_25[hown_25 == 4] <- 2
hown_25[hown_25 == 5] <- 2
hown_25[hown_25 == 6] <- 2
hown_25[hown_25 == 7] <- 2
hown_25[hown_25 == 8] <- 2
hown_25[hown_25 %in% c(-9, -8, -1)] <- -3
w8$hown25 <- hown_25

# Age 32 (W9)
hown_32 <- w9$W9DTENURE
hown_32[hown_32 == 1] <- 1
hown_32[hown_32 == 2] <- 1
hown_32[hown_32 == 3] <- 1
hown_32[hown_32 == 4] <- 2
hown_32[hown_32 == 5] <- -3
hown_32[hown_32 == 6] <- -3
hown_32[hown_32 == 7] <- -3
hown_32[hown_32 == -8] <- -8
w9$hown32 <- hown_32

cat('Derived variables added to wave datasets\n')

# Combine all wave datasets
combined <- full_join(w1, w2, by = 'NSID') %>%
  full_join(w3, by = 'NSID') %>%
  full_join(w4, by = 'NSID') %>%
  full_join(w5, by = 'NSID') %>%
  full_join(w6, by = 'NSID') %>%
  full_join(w7, by = 'NSID') %>%
  full_join(w8, by = 'NSID') %>%
  full_join(w9, by = 'NSID')

cat('Combined dataset rows:', nrow(combined), '\n')
cat('Variables:', ncol(combined), '\n')

# Remove raw source tenure variables
combined <- combined %>%
  select(-W1hous12HH, -W2Hous12HH, -W3hous12HH, -W4Hous12HH, -W5Hous12HH,
         -W6Hous12YP, -W6Hous12bYP, -W6Hous12cYP,
         -W7Hous12YP, -W7Hous12bYP, -W7Hous12cYP,
         -W8TENURE, -W9DTENURE)

cat('Final variables:', ncol(combined), '\n')
print(names(combined))

# Write output
write_csv(combined, 'data/output/cleaned_data.csv')
cat('Output written to data/output/cleaned_data.csv\n')

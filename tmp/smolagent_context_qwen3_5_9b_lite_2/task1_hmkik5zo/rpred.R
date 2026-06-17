library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
w2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
w3 <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
w4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
w5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
w6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
w7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
ns8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t', show_col_types = FALSE)
ns9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)

# Function to recode missing values to standard codes
rec_missing <- function(x, exclude = c(-9, -8, -3, -2, -1, 1, 2)) {
  x[!x %in% exclude] <- -3
  return(x)
}

# Apply missing value recoding
w1$W1sexYP <- rec_missing(w1$W1sexYP)
w2$W2SexYP <- rec_missing(w2$W2SexYP)
w3$W3sexYP <- rec_missing(w3$W3sexYP)
w4$W4SexYP <- rec_missing(w4$W4SexYP)
w5$W5SexYP <- rec_missing(w5$W5SexYP)
w6$W6Sex <- rec_missing(w6$W6Sex)
w7$W7Sex <- rec_missing(w7$W7Sex)
ns8$W8CMSEX <- rec_missing(ns8$W8CMSEX)
ns9$W9DSEX <- rec_missing(ns9$W9DSEX)

# Create full cohort frame with all waves
cohort <- w1
cohort <- full_join(cohort, w2, by = 'NSID')
cohort <- full_join(cohort, w3, by = 'NSID')
cohort <- full_join(cohort, w4, by = 'NSID')
cohort <- full_join(cohort, w5, by = 'NSID')
cohort <- full_join(cohort, w6, by = 'NSID')
cohort <- full_join(cohort, w7, by = 'NSID')
cohort <- full_join(cohort, ns8, by = 'NSID')
cohort <- full_join(cohort, ns9, by = 'NSID')

# Create sex variable using earliest-valid-first approach
# Valid values are 1 (Male) and 2 (Female)
cohort$sex <- NA_integer_

# Process in order from earliest wave to latest
if (!is.na(cohort$W1sexYP) && cohort$W1sexYP %in% c(1L, 2L)) {
  cohort$sex <- cohort$W1sexYP
}
if (is.na(cohort$sex) && !is.na(cohort$W2SexYP) && cohort$W2SexYP %in% c(1L, 2L)) {
  cohort$sex <- cohort$W2SexYP
}
if (is.na(cohort$sex) && !is.na(cohort$W3sexYP) && cohort$W3sexYP %in% c(1L, 2L)) {
  cohort$sex <- cohort$W3sexYP
}
if (is.na(cohort$sex) && !is.na(cohort$W4SexYP) && cohort$W4SexYP %in% c(1L, 2L)) {
  cohort$sex <- cohort$W4SexYP
}
if (is.na(cohort$sex) && !is.na(cohort$W5SexYP) && cohort$W5SexYP %in% c(1L, 2L)) {
  cohort$sex <- cohort$W5SexYP
}
if (is.na(cohort$sex) && !is.na(cohort$W6Sex) && cohort$W6Sex %in% c(1L, 2L)) {
  cohort$sex <- cohort$W6Sex
}
if (is.na(cohort$sex) && !is.na(cohort$W7Sex) && cohort$W7Sex %in% c(1L, 2L)) {
  cohort$sex <- cohort$W7Sex
}
if (is.na(cohort$sex) && !is.na(cohort$W8CMSEX) && cohort$W8CMSEX %in% c(1L, 2L)) {
  cohort$sex <- cohort$W8CMSEX
}
if (is.na(cohort$sex) && !is.na(cohort$W9DSEX) && cohort$W9DSEX %in% c(1L, 2L)) {
  cohort$sex <- cohort$W9DSEX
}

# Convert to factor with explicit labels for valid categories and missing codes
cohort$sex <- factor(cohort$sex, levels = c(1L, 2L, -9L, -8L, -3L, -2L, -1L),
  labels = c('Male', 'Female', 'Refusal', "Don't know", 'Not asked', 'Not applicable', 'Not applicable'))

# Write output - only keep NSID and sex
cohort_final <- cohort %>% select(NSID, sex)
write_csv(cohort_final, 'data/output/cleaned_data.csv')

cat('Script completed successfully.\n')
print(table(cohort$sex))
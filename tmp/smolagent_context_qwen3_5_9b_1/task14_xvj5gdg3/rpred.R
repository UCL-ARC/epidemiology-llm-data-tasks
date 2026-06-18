library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')
wave5 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Start with wave1 as base
cohort <- wave1

# Add subsequent waves
cohort <- full_join(cohort, wave2, by = 'NSID')
cohort <- full_join(cohort, wave3, by = 'NSID')
cohort <- full_join(cohort, wave4, by = 'NSID')
cohort <- full_join(cohort, wave5, by = 'NSID')
cohort <- full_join(cohort, wave6, by = 'NSID')
cohort <- full_join(cohort, wave7, by = 'NSID')
cohort <- full_join(cohort, wave8, by = 'NSID')
cohort <- full_join(cohort, wave9, by = 'NSID')

cat('Cohort rows:', nrow(cohort), '\n')

collapse_tenure <- function(x) {
  if (is.numeric(x)) {
    x[x %in% c(4L, 5L, 6L)] <- 4L
    x[x %in% c(6L, 7L)] <- 6L
  }
  return(x)
}

convert_mis <- function(x, keep_m1 = FALSE) {
  if (is.numeric(x)) {
    x[x %in% c(-999, -998, -997, -995, -99, -94)] <- -3
    if (!keep_m1) x[x %in% -1] <- -8
  }
  return(x)
}

# Sweeps 1-4
hownteen14 <- convert_mis(cohort$W1hous12HH)
hownteen15 <- convert_mis(cohort$W2Hous12HH)
hownteen16 <- convert_mis(cohort$W3hous12HH)
hownteen17 <- convert_mis(cohort$W4Hous12HH)

# Sweep 5
w5type <- cohort$W5Hous12HH
w5owned <- cohort$W5Hous12BHH
w5rented <- cohort$W5Hous12CHH
hownteen18 <- w5type
hownteen18[is.na(hownteen18) & !is.na(w5owned) & w5owned %in% c(1L,2L,3L,4L)] <- w5owned[is.na(hownteen18) & !is.na(w5owned) & w5owned %in% c(1L,2L,3L,4L)]
hownteen18[is.na(hownteen18) & !is.na(w5rented) & w5rented %in% c(1L,2L,3L,4L,5L)] <- w5rented[is.na(hownteen18) & !is.na(w5rented) & w5rented %in% c(1L,2L,3L,4L,5L)]
hownteen18 <- convert_mis(hownteen18, TRUE)

# Sweep 6
w6type <- cohort$W6Hous12YP
w6owned <- cohort$W6Hous12bYP
w6rented <- cohort$W6Hous12cYP
hownteen19 <- w6type
hownteen19[is.na(hownteen19) & !is.na(w6owned) & w6owned %in% c(1L,2L,3L,4L)] <- w6owned[is.na(hownteen19) & !is.na(w6owned) & w6owned %in% c(1L,2L,3L,4L)]
hownteen19[is.na(hownteen19) & !is.na(w6rented) & w6rented %in% c(1L,2L,3L,4L,5L)] <- w6rented[is.na(hownteen19) & !is.na(w6rented) & w6rented %in% c(1L,2L,3L,4L,5L)]
hownteen19 <- convert_mis(hownteen19, TRUE)

# Sweep 7
w7type <- cohort$W7Hous12YP
w7owned <- cohort$W7Hous12bYP
w7rented <- cohort$W7Hous12cYP
hownteen20 <- w7type
hownteen20[is.na(hownteen20) & !is.na(w7owned) & w7owned %in% c(1L,2L,3L,4L)] <- w7owned[is.na(hownteen20) & !is.na(w7owned) & w7owned %in% c(1L,2L,3L,4L)]
hownteen20[is.na(hownteen20) & !is.na(w7rented) & w7rented %in% c(1L,2L,3L,4L,5L)] <- w7rented[is.na(hownteen20) & !is.na(w7rented) & w7rented %in% c(1L,2L,3L,4L,5L)]
hownteen20 <- convert_mis(hownteen20, TRUE)

# Sweeps 8-9
hownteen25 <- cohort$W8TENURE
hownteen25[hownteen25 == -1] <- -3

hownteen32 <- cohort$W9DTENURE
hownteen32[hownteen32 == -1] <- -3

# Collapse all
how14 <- collapse_tenure(hownteen14)
how15 <- collapse_tenure(hownteen15)
how16 <- collapse_tenure(hownteen16)
how17 <- collapse_tenure(hownteen17)
how18 <- collapse_tenure(hownteen18)
how19 <- collapse_tenure(hownteen19)
how20 <- collapse_tenure(hownteen20)
how25 <- collapse_tenure(hownteen25)
how32 <- collapse_tenure(hownteen32)

cat('hownteen14 rows:', length(hownteen14), '\n')
cat('how14 rows:', length(how14), '\n')
cohort$hownteen14 <- hownteen14
cohort$hownteen15 <- hownteen15
cohort$hownteen16 <- hownteen16
cohort$hownteen17 <- hownteen17
cohort$hownteen18 <- hownteen18
cohort$hownteen19 <- hownteen19
cohort$hownteen20 <- hownteen20
cohort$hownteen25 <- hownteen25
cohort$hownteen32 <- hownteen32
cohort$how14 <- how14
cohort$how15 <- how15
cohort$how16 <- how16
cohort$how17 <- how17
cohort$how18 <- how18
cohort$how19 <- how19
cohort$how20 <- how20
cohort$how25 <- how25
cohort$how32 <- how32

cat('After assignment, cohort rows:', nrow(cohort), '\n')

cleaned <- cohort[, c('NSID', 'hownteen14', 'hownteen15', 'hownteen16', 'hownteen17', 'hownteen18', 'hownteen19', 'hownteen20', 'hownteen25', 'hownteen32', 'how14', 'how15', 'how16', 'how17', 'how18', 'how19', 'how20', 'how25', 'how32')] 

write_csv(cleaned, 'data/output/cleaned_data.csv')
print('Done!')

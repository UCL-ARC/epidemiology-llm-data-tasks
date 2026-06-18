library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

setwd('data')

# Load all wave files
wave1 <- read_delim('input/wave_one_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave2 <- read_delim('input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave3 <- read_delim('input/wave_three_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('input/wave_four_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)

convert_w13 <- function(x) {
  x <- as.numeric(x)
  x[x == -999] <- -2
  x[x == -99] <- -3
  x[x == -98] <- -3
  x[x == -94] <- -8
  x[is.na(x)] <- -3
  return(x)
}

convert_w4 <- function(x) {
  x <- as.numeric(x)
  x[x == -999] <- -2
  x[x == -996] <- -1
  x[x == -99] <- -3
  x[x == -98] <- -3
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[is.na(x)] <- -3
  return(x)
}

ecoactma14 <- convert_w13(wave1$W1empsmum)
ecoactpa14 <- convert_w13(wave1$W1empsdad)
ecoactma15 <- convert_w13(wave2$W2empsmum)
ecoactpa15 <- convert_w13(wave2$W2empsdad)
ecoactma16 <- convert_w13(wave3$W3empsmum)
ecoactpa16 <- convert_w13(wave3$W3empsdad)
ecoactma17 <- convert_w4(wave4$w4empsmum)
ecoactpa17 <- convert_w4(wave4$w4empsdad)

wave1_df <- data.frame(NSID = wave1$NSID, ecoactma14 = ecoactma14, ecoactpa14 = ecoactpa14)
wave2_df <- data.frame(NSID = wave2$NSID, ecoactma15 = ecoactma15, ecoactpa15 = ecoactpa15)
wave3_df <- data.frame(NSID = wave3$NSID, ecoactma16 = ecoactma16, ecoactpa16 = ecoactpa16)
wave4_df <- data.frame(NSID = wave4$NSID, ecoactma17 = ecoactma17, ecoactpa17 = ecoactpa17)

combined <- full_join(wave1_df, wave2_df, by = 'NSID')
combined <- full_join(combined, wave3_df, by = 'NSID')
combined <- full_join(combined, wave4_df, by = 'NSID')

# Add labels
lbl_1_9 <- c('1' = 'Doing paid work for 30 or more hours a week',
             '2' = 'Doing paid work for fewer than 30 hours a week',
             '3' = 'Unemployed/ Looking for a job',
             '4' = 'On a training course or scheme',
             '5' = 'In full-time education/ at school',
             '6' = 'Looking after the family/ household',
             '7' = 'Retired from work altogether',
             '8' = 'Sick/ disabled',
             '9' = 'Other')

lbl_miss <- c(-9 = 'Refusal', -8 = 'Don\'t know / insufficient information', -7 = 'Prefer not to say', -3 = 'Not asked at the fieldwork stage / not interviewed', -2 = 'Schedule not applicable / script error / information lost', -1 = 'Item not applicable')

all_labels <- c(lbl_1_9, lbl_miss)

vars <- c('ecoactma14', 'ecoactpa14', 'ecoactma15', 'ecoactpa15', 'ecoactma16', 'ecoactpa16', 'ecoactma17', 'ecoactpa17')
for (v in vars) {
  combined[[v]] <- as.factor(combined[[v]])
  lab <- labelled(combined[[v]])
  lab <- set_label(lab, all_labels)
  combined[[v]] <- lab
}

write_csv(combined, 'output/cleaned_data.csv')
print('Done')
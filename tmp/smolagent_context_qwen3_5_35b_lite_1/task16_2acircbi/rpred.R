library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all input files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)

# Merge all datasets by NSID
cleaned <- full_join(wave1, wave2, by = 'NSID')
cleaned <- full_join(cleaned, wave3, by = 'NSID')
cleaned <- full_join(cleaned, wave4, by = 'NSID')

# Create banded household income variables (ages 14, 15, 16, 17)

# Age 14 - W1GrsswkHH (continuous values, need to convert to bands)
cleaned$income14 <- case_when(
  cleaned$W1GrsswkHH == -999 ~ -2,
  cleaned$W1GrsswkHH == -992 ~ -2,
  cleaned$W1GrsswkHH == -99 ~ -2,
  cleaned$W1GrsswkHH == -94 ~ -2,
  cleaned$W1GrsswkHH == -92 ~ -9,
  cleaned$W1GrsswkHH == -91 ~ -1,
  cleaned$W1GrsswkHH == -3 ~ -3,
  cleaned$W1GrsswkHH == -1 ~ -8,
  cleaned$W1GrsswkHH <= 49 ~ 1,
  cleaned$W1GrsswkHH >= 50 & cleaned$W1GrsswkHH <= 99 ~ 2,
  cleaned$W1GrsswkHH >= 100 & cleaned$W1GrsswkHH <= 199 ~ 3,
  cleaned$W1GrsswkHH >= 200 & cleaned$W1GrsswkHH <= 299 ~ 4,
  cleaned$W1GrsswkHH >= 300 & cleaned$W1GrsswkHH <= 399 ~ 5,
  cleaned$W1GrsswkHH >= 400 & cleaned$W1GrsswkHH <= 499 ~ 6,
  cleaned$W1GrsswkHH >= 500 & cleaned$W1GrsswkHH <= 599 ~ 7,
  cleaned$W1GrsswkHH >= 600 & cleaned$W1GrsswkHH <= 699 ~ 8,
  cleaned$W1GrsswkHH >= 700 & cleaned$W1GrsswkHH <= 799 ~ 9,
  cleaned$W1GrsswkHH >= 800 & cleaned$W1GrsswkHH <= 899 ~ 10,
  cleaned$W1GrsswkHH >= 900 & cleaned$W1GrsswkHH <= 999 ~ 11,
  cleaned$W1GrsswkHH >= 1000 ~ 12,
  TRUE ~ NA_real_
)

# Age 15 - W2GrsswkHH (continuous values, need to convert to bands)
cleaned$income15 <- case_when(
  cleaned$W2GrsswkHH == -999 ~ -2,
  cleaned$W2GrsswkHH == -992 ~ -2,
  cleaned$W2GrsswkHH == -99 ~ -2,
  cleaned$W2GrsswkHH == -94 ~ -2,
  cleaned$W2GrsswkHH == -92 ~ -9,
  cleaned$W2GrsswkHH == -91 ~ -1,
  cleaned$W2GrsswkHH == -3 ~ -3,
  cleaned$W2GrsswkHH == -1 ~ -8,
  cleaned$W2GrsswkHH == 0 ~ -2,
  cleaned$W2GrsswkHH <= 49 ~ 1,
  cleaned$W2GrsswkHH >= 50 & cleaned$W2GrsswkHH <= 99 ~ 2,
  cleaned$W2GrsswkHH >= 100 & cleaned$W2GrsswkHH <= 199 ~ 3,
  cleaned$W2GrsswkHH >= 200 & cleaned$W2GrsswkHH <= 299 ~ 4,
  cleaned$W2GrsswkHH >= 300 & cleaned$W2GrsswkHH <= 399 ~ 5,
  cleaned$W2GrsswkHH >= 400 & cleaned$W2GrsswkHH <= 499 ~ 6,
  cleaned$W2GrsswkHH >= 500 & cleaned$W2GrsswkHH <= 599 ~ 7,
  cleaned$W2GrsswkHH >= 600 & cleaned$W2GrsswkHH <= 699 ~ 8,
  cleaned$W2GrsswkHH >= 700 & cleaned$W2GrsswkHH <= 799 ~ 9,
  cleaned$W2GrsswkHH >= 800 & cleaned$W2GrsswkHH <= 899 ~ 10,
  cleaned$W2GrsswkHH >= 900 & cleaned$W2GrsswkHH <= 999 ~ 11,
  cleaned$W2GrsswkHH >= 1000 ~ 12,
  TRUE ~ NA_real_
)

# Age 16 - W3incestw (already banded 1-12)
cleaned$income16 <- case_when(
  cleaned$W3incestw == -99 ~ -2,
  cleaned$W3incestw == -92 ~ -9,
  cleaned$W3incestw == -1 ~ -8,
  cleaned$W3incestw >= 1 & cleaned$W3incestw <= 12 ~ cleaned$W3incestw,
  TRUE ~ NA_real_
)

# Age 17 - w4IncEstW (already banded 1-12)
cleaned$income17 <- case_when(
  cleaned$w4IncEstW == -996 ~ -2,
  cleaned$w4IncEstW == -99 ~ -2,
  cleaned$w4IncEstW == -92 ~ -9,
  cleaned$w4IncEstW == -1 ~ -8,
  cleaned$w4IncEstW >= 1 & cleaned$w4IncEstW <= 12 ~ cleaned$w4IncEstW,
  TRUE ~ NA_real_
)

# Create continuous income variables (ages 14 and 15)
# Keep original continuous values, but set missing codes to NA

# Age 14 continuous
cleaned$income14_cont <- cleaned$W1GrsswkHH
cleaned$income14_cont[cleaned$income14_cont %in% c(-999, -992, -99, -94, -92, -91, -3, -1)] <- NA_real_

# Age 15 continuous
cleaned$income15_cont <- cleaned$W2GrsswkHH
cleaned$income15_cont[cleaned$income15_cont %in% c(-999, -992, -99, -94, -92, -91, -3, -1, 0)] <- NA_real_

# Keep only final derived variables (ID and income variables)
output <- cleaned %>%
  select(NSID, income14, income15, income16, income17, income14_cont, income15_cont)

# Write output
dir.create('data/output', showWarnings = FALSE, recursive = TRUE)
write_csv(output, 'data/output/cleaned_data.csv')

cat('Output saved successfully.\n')
cat('Dimensions:', dim(output), '\n')
cat('\nFirst 10 rows:\n')
print(head(output, 10))
cat('\nSummary of income14 (banded):\n')
print(table(output$income14, useNA = 'always'))
cat('\nSummary of income15 (banded):\n')
print(table(output$income15, useNA = 'always'))
cat('\nSummary of income16 (banded):\n')
print(table(output$income16, useNA = 'always'))
cat('\nSummary of income17 (banded):\n')
print(table(output$income17, useNA = 'always'))
cat('\nSummary of income14_cont (continuous):\n')
print(summary(output$income14_cont))
cat('\nSummary of income15_cont (continuous):\n')
print(summary(output$income15_cont))

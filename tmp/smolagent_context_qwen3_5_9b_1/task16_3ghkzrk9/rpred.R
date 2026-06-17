library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# ---- Wave 1 (age 14) ----
w1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')

W1GrsswkHH <- w1$W1GrsswkHH
W1GrsswkHH <- case_when(
  W1GrsswkHH == -3 ~ -1,
  W1GrsswkHH == -1 ~ -8,
  W1GrsswkHH == -992 ~ -9,
  TRUE ~ W1GrsswkHH
)
W1GrsswkHH[is.na(W1GrsswkHH)] <- -3
W1GrsswkHH_positive <- ifelse(W1GrsswkHH > 0, W1GrsswkHH, NA)
incwhh14 <- factor(cut(W1GrsswkHH_positive, breaks = c(-Inf, 49, 99, 199, 299, 399, 499, 599, 699, 799, 899, 999, Inf),
                       labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)), ordered = TRUE)
incwhhcnt14 <- W1GrsswkHH_positive
w1 <- data.frame(w1, incwhh14 = incwhh14, incwhhcnt14 = incwhhcnt14)

# ---- Wave 2 (age 15) ----
w2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')

W2GrsswkHH <- w2$W2GrsswkHH
W2GrsswkHH <- case_when(
  W2GrsswkHH == -3 ~ -1,
  W2GrsswkHH == -1 ~ -8,
  W2GrsswkHH == -992 ~ -9,
  TRUE ~ W2GrsswkHH
)
W2GrsswkHH[is.na(W2GrsswkHH)] <- -3
W2GrsswkHH_positive <- ifelse(W2GrsswkHH > 0, W2GrsswkHH, NA)
incwhh15 <- factor(cut(W2GrsswkHH_positive, breaks = c(-Inf, 49, 99, 199, 299, 399, 499, 599, 699, 799, 899, 999, Inf),
                       labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)), ordered = TRUE)
incwhhcnt15 <- W2GrsswkHH_positive
w2 <- data.frame(w2, incwhh15 = incwhh15, incwhhcnt15 = incwhhcnt15)

# ---- Wave 3 (age 16) ----
w3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')

W3incestw <- w3$W3incestw
W3incestw <- case_when(
  W3incestw == -99 ~ -8,
  W3incestw == -92 ~ -9,
  W3incestw == -1 ~ -8,
  TRUE ~ W3incestw
)
W3incestw[is.na(W3incestw)] <- -3
W3incestw_positive <- ifelse(W3incestw > 0, W3incestw, NA)
incwhh16 <- factor(cut(W3incestw_positive, breaks = c(-Inf, 49, 99, 199, 299, 399, 499, 599, 699, 799, 899, 999, Inf),
                       labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)), ordered = TRUE)
w3 <- data.frame(w3, incwhh16 = incwhh16)

# ---- Wave 4 (age 17) ----
w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

w4IncEstW <- w4$w4IncEstW
w4IncEstW <- case_when(
  w4IncEstW == -996 ~ -3,
  w4IncEstW == -99 ~ -8,
  w4IncEstW == -92 ~ -9,
  w4IncEstW == -1 ~ -8,
  TRUE ~ w4IncEstW
)
w4IncEstW[is.na(w4IncEstW)] <- -3
w4IncEstW_positive <- ifelse(w4IncEstW > 0, w4IncEstW, NA)
incwhh17 <- factor(cut(w4IncEstW_positive, breaks = c(-Inf, 49, 99, 199, 299, 399, 499, 599, 699, 799, 899, 999, Inf),
                       labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)), ordered = TRUE)
w4 <- data.frame(w4, incwhh17 = incwhh17)

# Merge all datasets by NSID
combined <- full_join(w1, w2, by = 'NSID')
combined <- full_join(combined, w3, by = 'NSID')
combined <- full_join(combined, w4, by = 'NSID')

print('Columns in combined:')
print(names(combined))

# Check if our variables exist
print('Checking for income variables:')
print(c('incwhh14' %in% names(combined), 'incwhh15' %in% names(combined), 'incwhh16' %in% names(combined), 'incwhh17' %in% names(combined)))

# Create final output
final_data <- combined %>%
  select(NSID, incwhh14, incwhhcnt14, incwhh15, incwhhcnt15, incwhh16, incwhh17)

write_csv(final_data, 'data/output/cleaned_data.csv')
print('Script completed successfully')
print(head(final_data))
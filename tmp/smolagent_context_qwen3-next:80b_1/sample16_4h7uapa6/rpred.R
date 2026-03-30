library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Read files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Recode missing values for wave1 (age 14)
wave1$W1GrsswkHH[is.na(wave1$W1GrsswkHH)] <- -3
wave1$W1GrsswkHH <- case_when(
  wave1$W1GrsswkHH == -999.0 ~ -2,
  wave1$W1GrsswkHH == -992.0 ~ -9,
  wave1$W1GrsswkHH == -99.0 ~ -3,
  wave1$W1GrsswkHH == -94.0 ~ -8,
  wave1$W1GrsswkHH == -92.0 ~ -9,
  wave1$W1GrsswkHH == -91.0 ~ -1,
  wave1$W1GrsswkHH == -3.0 ~ -1,
  wave1$W1GrsswkHH == -1.0 ~ -8,
  TRUE ~ wave1$W1GrsswkHH
)

# Create continuous variable for wave1
wave1$incwhhcnt14 <- wave1$W1GrsswkHH

# Create banded variable for wave1
wave1$incwhh14 <- case_when(
  wave1$W1GrsswkHH >= 0 & wave1$W1GrsswkHH <= 49 ~ 1,
  wave1$W1GrsswkHH >= 50 & wave1$W1GrsswkHH <= 99 ~ 2,
  wave1$W1GrsswkHH >= 100 & wave1$W1GrsswkHH <= 199 ~ 3,
  wave1$W1GrsswkHH >= 200 & wave1$W1GrsswkHH <= 299 ~ 4,
  wave1$W1GrsswkHH >= 300 & wave1$W1GrsswkHH <= 399 ~ 5,
  wave1$W1GrsswkHH >= 400 & wave1$W1GrsswkHH <= 499 ~ 6,
  wave1$W1GrsswkHH >= 500 & wave1$W1GrsswkHH <= 599 ~ 7,
  wave1$W1GrsswkHH >= 600 & wave1$W1GrsswkHH <= 699 ~ 8,
  wave1$W1GrsswkHH >= 700 & wave1$W1GrsswkHH <= 799 ~ 9,
  wave1$W1GrsswkHH >= 800 & wave1$W1GrsswkHH <= 899 ~ 10,
  wave1$W1GrsswkHH >= 900 & wave1$W1GrsswkHH <= 999 ~ 11,
  wave1$W1GrsswkHH >= 1000 ~ 12,
  TRUE ~ wave1$W1GrsswkHH
)

# Convert to factor with labels
wave1$incwhh14 <- factor(wave1$incwhh14,
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -9, -8, -1, -3, -2),
                         labels = c("Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399", "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899", "£900 up to £999", "£1,000 or more",
                                   "Refusal", "Don't know/insufficient information", "Item not applicable", "Not asked/interviewed", "Script error/lost"))

# Similarly for wave2 (age 15)
wave2$W2GrsswkHH[is.na(wave2$W2GrsswkHH)] <- -3
wave2$W2GrsswkHH <- case_when(
  wave2$W2GrsswkHH == -999.0 ~ -2,
  wave2$W2GrsswkHH == -992.0 ~ -9,
  wave2$W2GrsswkHH == -99.0 ~ -3,
  wave2$W2GrsswkHH == -94.0 ~ -8,
  wave2$W2GrsswkHH == -92.0 ~ -9,
  wave2$W2GrsswkHH == -91.0 ~ -1,
  wave2$W2GrsswkHH == -3.0 ~ -1,
  wave2$W2GrsswkHH == -1.0 ~ -8,
  TRUE ~ wave2$W2GrsswkHH
)

wave2$incwhhcnt15 <- wave2$W2GrsswkHH

wave2$incwhh15 <- case_when(
  wave2$W2GrsswkHH >= 0 & wave2$W2GrsswkHH <= 49 ~ 1,
  wave2$W2GrsswkHH >= 50 & wave2$W2GrsswkHH <= 99 ~ 2,
  wave2$W2GrsswkHH >= 100 & wave2$W2GrsswkHH <= 199 ~ 3,
  wave2$W2GrsswkHH >= 200 & wave2$W2GrsswkHH <= 299 ~ 4,
  wave2$W2GrsswkHH >= 300 & wave2$W2GrsswkHH <= 399 ~ 5,
  wave2$W2GrsswkHH >= 400 & wave2$W2GrsswkHH <= 499 ~ 6,
  wave2$W2GrsswkHH >= 500 & wave2$W2GrsswkHH <= 599 ~ 7,
  wave2$W2GrsswkHH >= 600 & wave2$W2GrsswkHH <= 699 ~ 8,
  wave2$W2GrsswkHH >= 700 & wave2$W2GrsswkHH <= 799 ~ 9,
  wave2$W2GrsswkHH >= 800 & wave2$W2GrsswkHH <= 899 ~ 10,
  wave2$W2GrsswkHH >= 900 & wave2$W2GrsswkHH <= 999 ~ 11,
  wave2$W2GrsswkHH >= 1000 ~ 12,
  TRUE ~ wave2$W2GrsswkHH
)

wave2$incwhh15 <- factor(wave2$incwhh15,
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -9, -8, -1, -3, -2),
                         labels = c("Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399", "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899", "£900 up to £999", "£1,000 or more",
                                   "Refusal", "Don't know/insufficient information", "Item not applicable", "Not asked/interviewed", "Script error/lost"))

# Wave3 (age 16)
wave3$W3incestw[is.na(wave3$W3incestw)] <- -3
wave3$W3incestw <- case_when(
  wave3$W3incestw == -99.0 ~ -3,
  wave3$W3incestw == -92.0 ~ -9,
  wave3$W3incestw == -1.0 ~ -8,
  TRUE ~ wave3$W3incestw
)

wave3$incwhh16 <- factor(wave3$W3incestw,
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -9, -8, -3, -1, -2),
                         labels = c("Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399", "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899", "£900 up to £990", "£1,000 or more",
                                   "Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Item not applicable", "Script error/lost"))

# Wave4 (age 17)
wave4$w4IncEstW[is.na(wave4$w4IncEstW)] <- -3
wave4$w4IncEstW <- case_when(
  wave4$w4IncEstW == -996.0 ~ -3,
  wave4$w4IncEstW == -99.0 ~ -3,
  wave4$w4IncEstW == -92.0 ~ -9,
  wave4$w4IncEstW == -1.0 ~ -8,
  TRUE ~ wave4$w4IncEstW
)

wave4$incwhh17 <- factor(wave4$w4IncEstW,
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -9, -8, -3, -1, -2),
                         labels = c("Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399", "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899", "£900 up to £999", "£1,000 or more",
                                   "Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Item not applicable", "Script error/lost"))

# Merge all waves by NSID
cleaned_data <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Select required variables
cleaned_data <- cleaned_data %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Write to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")
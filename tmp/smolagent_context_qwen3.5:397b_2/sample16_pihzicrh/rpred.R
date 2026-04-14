library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load the datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge by NSID
merged <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Recode missing values for wave 1 (age 14)
# -999 -> -2 (Missing in error -> Script error/lost)
# -992 -> -8 (No information - work status questions refused -> Don't know/insufficient information)
# -99 -> -3 (HH not interviewed -> Not asked/interviewed)
# -94 -> -8 (Insufficient information -> Don't know/insufficient information)
# -92 -> -9 (Refused -> Refusal)
# -91 -> -1 (Not applicable -> Item not applicable)
# -3 -> -1 (Not yet paid -> Item not applicable) - special mapping
# -1 -> -8 (Don't know -> Don't know/insufficient information)
# NA -> -3 (Not asked/interviewed)

merged <- merged %>%
  mutate(
    incwhhcnt14 = case_when(
      is.na(W1GrsswkHH) ~ -3,
      W1GrsswkHH == -999 ~ -2,
      W1GrsswkHH == -992 ~ -8,
      W1GrsswkHH == -99 ~ -3,
      W1GrsswkHH == -94 ~ -8,
      W1GrsswkHH == -92 ~ -9,
      W1GrsswkHH == -91 ~ -1,
      W1GrsswkHH == -3 ~ -1,  # Not yet paid -> Item not applicable
      W1GrsswkHH == -1 ~ -8,
      TRUE ~ W1GrsswkHH
    )
  )

# Recode missing values for wave 2 (age 15)
merged <- merged %>%
  mutate(
    incwhhcnt15 = case_when(
      is.na(W2GrsswkHH) ~ -3,
      W2GrsswkHH == -999 ~ -2,
      W2GrsswkHH == -992 ~ -8,
      W2GrsswkHH == -99 ~ -3,
      W2GrsswkHH == -94 ~ -8,
      W2GrsswkHH == -92 ~ -9,
      W2GrsswkHH == -91 ~ -1,
      W2GrsswkHH == -3 ~ -1,  # Not yet paid -> Item not applicable
      W2GrsswkHH == -1 ~ -8,
      TRUE ~ W2GrsswkHH
    )
  )

# Recode missing values for wave 3 (age 16)
# -99 -> -3 (MP not interviewed -> Not asked/interviewed)
# -92 -> -9 (Refused -> Refusal)
# -1 -> -8 (Don't know -> Don't know/insufficient information)
# NA -> -3

merged <- merged %>%
  mutate(
    incwhh16 = case_when(
      is.na(W3incestw) ~ -3,
      W3incestw == -99 ~ -3,
      W3incestw == -92 ~ -9,
      W3incestw == -1 ~ -8,
      TRUE ~ as.numeric(W3incestw)
    )
  )

# Recode missing values for wave 4 (age 17)
# -996 -> -3 (No parent in household -> Not asked/interviewed) - special mapping
# -99 -> -3 (MP not interviewed -> Not asked/interviewed)
# -92 -> -9 (Refused -> Refusal)
# -1 -> -8 (Don't know -> Don't know/insufficient information)
# NA -> -3

merged <- merged %>%
  mutate(
    incwhh17 = case_when(
      is.na(w4IncEstW) ~ -3,
      w4IncEstW == -996 ~ -3,  # No parent in household -> Not asked/interviewed
      w4IncEstW == -99 ~ -3,
      w4IncEstW == -92 ~ -9,
      w4IncEstW == -1 ~ -8,
      TRUE ~ as.numeric(w4IncEstW)
    )
  )

# Create banded income variables for ages 14-15
# Banding scheme matching ages 16-17:
# 1=Up to £49, 2=£50-99, 3=£100-199, 4=£200-299, 5=£300-399,
# 6=£400-499, 7=£500-599, 8=£600-699, 9=£700-799, 10=£800-899,
# 11=£900-999, 12=£1000+
# Missing codes preserved: -9, -8, -3, -2, -1

merged <- merged %>%
  mutate(
    incwhh14 = case_when(
      incwhhcnt14 < 0 ~ incwhhcnt14,  # Keep missing codes as is
      incwhhcnt14 <= 49 ~ 1,
      incwhhcnt14 <= 99 ~ 2,
      incwhhcnt14 <= 199 ~ 3,
      incwhhcnt14 <= 299 ~ 4,
      incwhhcnt14 <= 399 ~ 5,
      incwhhcnt14 <= 499 ~ 6,
      incwhhcnt14 <= 599 ~ 7,
      incwhhcnt14 <= 699 ~ 8,
      incwhhcnt14 <= 799 ~ 9,
      incwhhcnt14 <= 899 ~ 10,
      incwhhcnt14 <= 999 ~ 11,
      incwhhcnt14 >= 1000 ~ 12,
      TRUE ~ incwhhcnt14
    ),
    incwhh15 = case_when(
      incwhhcnt15 < 0 ~ incwhhcnt15,  # Keep missing codes as is
      incwhhcnt15 <= 49 ~ 1,
      incwhhcnt15 <= 99 ~ 2,
      incwhhcnt15 <= 199 ~ 3,
      incwhhcnt15 <= 299 ~ 4,
      incwhhcnt15 <= 399 ~ 5,
      incwhhcnt15 <= 499 ~ 6,
      incwhhcnt15 <= 599 ~ 7,
      incwhhcnt15 <= 699 ~ 8,
      incwhhcnt15 <= 799 ~ 9,
      incwhhcnt15 <= 899 ~ 10,
      incwhhcnt15 <= 999 ~ 11,
      incwhhcnt15 >= 1000 ~ 12,
      TRUE ~ incwhhcnt15
    )
  )

# Convert banded variables to factors with labels
income_levels <- c("-9", "-8", "-3", "-2", "-1", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
income_labels <- c("Refusal", "Don't know/insufficient information", "Not asked/interviewed",
                   "Script error/lost", "Item not applicable",
                   "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299",
                   "£300 up to £399", "£400 up to £499", "£500 up to £599", "£600 up to £699",
                   "£700 up to £799", "£800 up to £899", "£900 up to £999", "£1,000 or more")

merged <- merged %>%
  mutate(
    incwhh14 = factor(as.character(incwhh14), levels = income_levels, labels = income_labels),
    incwhh15 = factor(as.character(incwhh15), levels = income_levels, labels = income_labels),
    incwhh16 = factor(as.character(incwhh16), levels = income_levels, labels = income_labels),
    incwhh17 = factor(as.character(incwhh17), levels = income_levels, labels = income_labels)
  )

# Select only the required variables
output <- merged %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
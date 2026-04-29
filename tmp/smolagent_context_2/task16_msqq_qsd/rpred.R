library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Load files from data/input/
file_14 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
file_15 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
file_16 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
file_17 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))

# Convert specific columns to numeric for calculations
file_14$W1GrsswkHH <- as.numeric(file_14$W1GrsswkHH)
file_15$W2GrsswkHH <- as.numeric(file_15$W2GrsswkHH)
file_16$W3incestw <- as.numeric(file_16$W3incestw)
file_17$w4IncEstW <- as.numeric(file_17$w4IncEstW)

# Merge datasets using full_join by NSID
df <- file_14 %>%
  full_join(file_15, by = "NSID") %>%
  full_join(file_16, by = "NSID") %>%
  full_join(file_17, by = "NSID")

# Define banding function for continuous income
# Bands: 1: <50, 2: 50-99, 3: 100-199, 4: 200-299, 5: 300-399, 6: 400-499, 7: 500-599, 8: 600-699, 9: 700-799, 10: 800-899, 11: 900-999, 12: >=1000
get_band <- function(x) {
  case_when(
    x < 50 ~ 1,
    x < 100 ~ 2,
    x < 200 ~ 3,
    x < 300 ~ 4,
    x < 400 ~ 5,
    x < 500 ~ 6,
    x < 600 ~ 7,
    x < 700 ~ 8,
    x < 800 ~ 9,
    x < 900 ~ 10,
    x < 1000 ~ 11,
    x >= 1000 ~ 12,
    TRUE ~ NA_real_
  )
}

# Common labels for Banded variables
band_labels <- c(
  "1" = "Up to £49", "2" = "£50 up to £99", "3" = "£100 up to £199",
  "4" = "£200 up to £299", "5" = "£300 up to £399", "6" = "£400 up to £499",
  "7" = "£500 up to £599", "8" = "£600 up to £699", "9" = "£700 up to £799",
  "10" = "£800 up to £899", "11" = "£900 up to £999", "12" = "£1,000 or more",
  "-9" = "Refusal", "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say", "-3" = "Not asked / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

# Process Age 14
df <- df %>%
  mutate(
    income14_cont = W1GrsswkHH,
    # Missing Mapping
    income14_cont = case_when(
      W1GrsswkHH == -3 ~ -1, # Not yet paid
      W1GrsswkHH == -1 ~ -8, # Don't know
      W1GrsswkHH == -992 ~ -9, # Refused
      W1GrsswkHH == -92 ~ -9, # Refused (default)
      W1GrsswkHH == -91 ~ -1, # Not applicable
      W1GrsswkHH == -99 ~ -3, # Not interviewed
      W1GrsswkHH == -999 ~ -2, # Missing in error
      W1GrsswkHH == -94 ~ -8, # Insufficient info
      is.na(W1GrsswkHH) ~ -3,
      TRUE ~ W1GrsswkHH
    ),
    income14_band = case_when(
      W1GrsswkHH >= 0 ~ get_band(W1GrsswkHH),
      W1GrsswkHH == -3 ~ -1,
      W1GrsswkHH == -1 ~ -8,
      W1GrsswkHH == -992 ~ -9,
      W1GrsswkHH == -92 ~ -9,
      W1GrsswkHH == -91 ~ -1,
      W1GrsswkHH == -99 ~ -3,
      W1GrsswkHH == -999 ~ -2,
      W1GrsswkHH == -94 ~ -8,
      is.na(W1GrsswkHH) ~ -3,
      TRUE ~ as.numeric(NA)
    )
  )

# Process Age 15
df <- df %>%
  mutate(
    income15_cont = W2GrsswkHH,
    income15_cont = case_when(
      W2GrsswkHH == -3 ~ -1,
      W2GrsswkHH == -1 ~ -8,
      W2GrsswkHH == -992 ~ -9,
      W2GrsswkHH == -92 ~ -9,
      W2GrsswkHH == -91 ~ -1,
      W2GrsswkHH == -99 ~ -3,
      W2GrsswkHH == -999 ~ -2,
      W2GrsswkHH == -94 ~ -8,
      is.na(W2GrsswkHH) ~ -3,
      TRUE ~ W2GrsswkHH
    ),
    income15_band = case_when(
      W2GrsswkHH >= 0 ~ get_band(W2GrsswkHH),
      W2GrsswkHH == -3 ~ -1,
      W2GrsswkHH == -1 ~ -8,
      W2GrsswkHH == -992 ~ -9,
      W2GrsswkHH == -92 ~ -9,
      W2GrsswkHH == -91 ~ -1,
      W2GrsswkHH == -99 ~ -3,
      W2GrsswkHH == -999 ~ -2,
      W2GrsswkHH == -94 ~ -8,
      is.na(W2GrsswkHH) ~ -3,
      TRUE ~ as.numeric(NA)
    )
  )

# Process Age 16
df <- df %>%
  mutate(
    income16_band = case_when(
      W3incestw >= 0 ~ W3incestw,
      W3incestw == -92 ~ -9,
      W3incestw == -1 ~ -8,
      W3incestw == -99 ~ -3,
      is.na(W3incestw) ~ -3,
      TRUE ~ W3incestw
    )
  )

# Process Age 17
df <- df %>%
  mutate(
    income17_band = case_when(
      w4IncEstW >= 0 ~ w4IncEstW,
      w4IncEstW == -996 ~ -3,
      w4IncEstW == -92 ~ -9,
      w4IncEstW == -1 ~ -8,
      w4IncEstW == -99 ~ -3,
      is.na(w4IncEstW) ~ -3,
      TRUE ~ w4IncEstW
    )
  )

# Final Selection and Factor Conversion
final_df <- df %>%
  select(NSID, income14_cont, income14_band, income15_cont, income15_band, income16_band, income17_band)

# Apply factor labels to banded variables
banded_cols <- c("income14_band", "income15_band", "income16_band", "income17_band")
for(col in banded_cols) {
  final_df[[col]] <- factor(final_df[[col]], levels = names(band_labels), labels = band_labels)
}

write_csv(final_df, "data/output/cleaned_data.csv")
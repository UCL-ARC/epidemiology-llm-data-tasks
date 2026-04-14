library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
file14 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols())
file15 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols())
file16 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols())
file17 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols())

# Merge datasets
data <- file14 %>%
  full_join(file15, by = "NSID") %>%
  full_join(file16, by = "NSID") %>%
  full_join(file17, by = "NSID")

# 3 & 4. Standard Missing-Value Codes Mapping Function
# Standard: -9 Refusal, -8 Don't know/insufficient, -1 Not applicable, -3 Not asked, -2 Script error
harmonize_missing <- function(x, wave) {
  # First handle NAs as -3
  x[is.na(x)] <- -3
  
  if (wave == "14" || wave == "15") {
    # Wave 14/15 specific codes
    x <- case_when(
      x == -92.0 ~ -9,   # Refused
      x == -1.0 ~ -8,    # Don't know
      x == -94.0 ~ -8,   # Insufficient info
      x == -91.0 ~ -1,   # Not applicable
      x == -3.0 ~ -1,    # Not yet paid -> Map to -1
      x == -99.0 ~ -3,   # HH not interviewed
      x == -999.0 ~ -2,  # Missing in error
      x == -992.0 ~ -3,  # No info - refused
      TRUE ~ x
    )
  } else if (wave == "16") {
    # Wave 16 specific codes
    x <- case_when(
      x == -92.0 ~ -9,   # Refused
      x == -1.0 ~ -8,    # Don't know
      x == -99.0 ~ -3,   # MP not interviewed
      TRUE ~ x
    )
  } else if (wave == "17") {
    # Wave 17 specific codes
    x <- case_when(
      x == -92.0 ~ -9,   # Refused
      x == -1.0 ~ -8,    # Don't know
      x == -996.0 ~ -3,  # No parent in household -> Map to -3
      x == -99.0 ~ -3,   # MP not interviewed
      TRUE ~ x
    )
  }
  return(x)
}

# Apply harmonization to continuous/raw variables
data <- data %>%
  mutate(
    incwhhcnt14 = harmonize_missing(W1GrsswkHH, "14"),
    incwhhcnt15 = harmonize_missing(W2GrsswkHH, "15"),
    # For 16 and 17, they are banded, but we harmonize the codes first
    raw_inc16 = harmonize_missing(W3incestw, "16"),
    raw_inc17 = harmonize_missing(w4IncEstW, "17")
  )

# 5. Banding for ages 14 and 15
# Defining a realistic banding scheme for continuous weekly income
# 1: <50, 2: 50-99, 3: 100-199, 4: 200-299, 5: 300-399, 6: 400-499, 7: 500-599, 8: 600-699, 9: 700-799, 10: 800-899, 11: 900-999, 12: 1000+
create_bands <- function(x) {
  # Preserving missing codes (x < 0)
  bands <- rep(NA, length(x))
  valid_idx <- x >= 0
  
  val <- x[valid_idx]
  band_val <- case_when(
    val < 50 ~ 1,
    val < 100 ~ 2,
    val < 200 ~ 3,
    val < 300 ~ 4,
    val < 400 ~ 5,
    val < 500 ~ 6,
    val < 600 ~ 7,
    val < 700 ~ 8,
    val < 800 ~ 9,
    val < 900 ~ 10,
    val < 1000 ~ 11,
    val >= 1000 ~ 12,
    TRUE ~ NA_real_
  )
  bands[valid_idx] <- band_val
  # Fill missing codes back in
  bands[!valid_idx] <- x[!valid_idx]
  return(bands)
}

data <- data %>%
  mutate(
    incwhh14_num = create_bands(incwhhcnt14),
    incwhh15_num = create_bands(incwhhcnt15)
  )

# 7. Factor Labels
income_labels <- c(
  "1" = "Up to £49", "2" = "£50 up to £99", "3" = "£100 up to £199",
  "4" = "£200 up to £299", "5" = "£300 up to £399", "6" = "£400 up to £499",
  "7" = "£500 up to £599", "8" = "£600 up to £699", "9" = "£700 up to £799",
  "10" = "£800 up to £899", "11" = "£900 up to £999", "12" = "£1,000 or more",
  "-9" = "Refusal", "-8" = "Don't know/insufficient information",
  "-3" = "Not asked/interviewed", "-2" = "Script error/lost", "-1" = "Item not applicable"
)

# Convert to factors
data <- data %>%
  mutate(
    incwhh14 = factor(incwhh14_num, levels = as.numeric(names(income_labels)), labels = income_labels),
    incwhh15 = factor(incwhh15_num, levels = as.numeric(names(income_labels)), labels = income_labels),
    incwhh16 = factor(raw_inc16, levels = as.numeric(names(income_labels)), labels = income_labels),
    incwhh17 = factor(raw_inc17, levels = as.numeric(names(income_labels)), labels = income_labels)
  )

# 10. Output Requirements
final_data <- data %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

write_csv(final_data, "data/output/cleaned_data.csv")
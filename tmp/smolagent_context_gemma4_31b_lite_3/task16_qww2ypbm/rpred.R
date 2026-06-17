library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load data
wave1 <- readr::read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols())
wave2 <- readr::read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols())
wave3 <- readr::read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols())
wave4 <- readr::read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols())

# Merge datasets
data <- wave1 %>%
  full_join(wave2, by = 'NSID') %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(wave4, by = 'NSID')

# Mapping function for missing values based on labels provided in metadata
# -9 = Refusal
# -8 = Don't know / insufficient information
# -7 = Prefer not to say
# -3 = Not asked / not interviewed
# -2 = Schedule not applicable / script error / information lost
# -1 = Item not applicable

clean_missing <- function(x, wave_type) {
  # Standard mapping for the continuous variables (W1, W2)
  if (wave_type == 'continuous') {
    res <- x
    res[x == -999.0] <- -2 # Missing in error
    res[x == -992.0] <- -9 # Work status refused
    res[x == -99.0] <- -3  # HH not interviewed
    res[x == -94.0] <- -8  # Insufficient information
    res[x == -92.0] <- -9  # Refused
    res[x == -91.0] <- -1  # Not applicable
    res[x == -3.0] <- -1   # Not yet paid (treated as not applicable/missing)
    res[x == -1.0] <- -8   # Don't know
    res[is.na(res)] <- -3
    return(res)
  } else {
    # Standard mapping for the banded variables (W3, W4)
    res <- x
    res[x == -99.0] <- -3  # MP not interviewed
    res[x == -92.0] <- -9  # Refused
    res[x == -1.0] <- -8   # Don't know
    res[x == -996.0] <- -1 # No parent in household
    res[is.na(res)] <- -3
    return(res)
  }
}

# Process variables
# Age 14: Continuous (W1GrsswkHH)
# Age 15: Continuous (W2GrsswkHH)
# Age 16: Banded (W3incestw)
# Age 17: Banded (w4IncEstW)

data <- data %>%
  mutate(
    hhinc14 = clean_missing(W1GrsswkHH, 'continuous'),
    hhinc15 = clean_missing(W2GrsswkHH, 'continuous'),
    hhinc16 = clean_missing(W3incestw, 'banded'),
    hhinc17 = clean_missing(w4IncEstW, 'banded')
  )

# Since the requirement asks for "banded household weekly income variables at ages 14, 15, 16, 17", 
# and we only have continuous for 14 and 15, we must band the continuous ones.
# Looking at W3/W4 bands:
# 1: Up to 49, 2: 50-99, 3: 100-199, 4: 200-299, 5: 300-399, 6: 400-499, 7: 500-599, 8: 600-699, 9: 700-799, 10: 800-899, 11: 900-999, 12: 1000+

band_income <- function(x) {
  res <- rep(NA, length(x))
  valid <- x >= 0
  res[valid & x < 50] <- 1
  res[valid & x >= 50 & x < 100] <- 2
  res[valid & x >= 100 & x < 200] <- 3
  res[valid & x >= 200 & x < 300] <- 4
  res[valid & x >= 300 & x < 400] <- 5
  res[valid & x >= 400 & x < 500] <- 6
  res[valid & x >= 500 & x < 600] <- 7
  res[valid & x >= 600 & x < 700] <- 8
  res[valid & x >= 700 & x < 800] <- 9
  res[valid & x >= 800 & x < 900] <- 10
  res[valid & x >= 900 & x < 1000] <- 11
  res[valid & x >= 1000] <- 12
  
  # Preserve missing codes from the cleaned continuous variable
  # But the continuous clean function already ran. 
  # We need the raw source for banding or handle the codes.
  return(res)
}

# Recalculate based on raw data to ensure missing codes are preserved during banding
band_with_missing <- function(raw_x) {
  res <- rep(NA, length(raw_x))
  # Valid range
  valid <- raw_x >= 0
  res[valid & raw_x < 50] <- 1
  res[valid & raw_x >= 50 & raw_x < 100] <- 2
  res[valid & raw_x >= 100 & raw_x < 200] <- 3
  res[valid & raw_x >= 200 & raw_x < 300] <- 4
  res[valid & raw_x >= 300 & raw_x < 400] <- 5
  res[valid & raw_x >= 400 & raw_x < 500] <- 6
  res[valid & raw_x >= 500 & raw_x < 600] <- 7
  res[valid & raw_x >= 600 & raw_x < 700] <- 8
  res[valid & raw_x >= 700 & raw_x < 800] <- 9
  res[valid & raw_x >= 800 & raw_x < 900] <- 10
  res[valid & raw_x >= 900 & raw_x < 1000] <- 11
  res[valid & raw_x >= 1000] <- 12
  
  # Now map missing codes from the source
  # These are the same as clean_missing continuous logic
  missing_map <- c(
    `-999.0` = -2, `-992.0` = -9, `-99.0` = -3, `-94.0` = -8, 
    `-92.0` = -9, `-91.0` = -1, `-3.0` = -1, `-1.0` = -8
  )
  
  for (code in names(missing_map)) {
    val <- as.numeric(code)
    res[raw_x == val] <- missing_map[[code]]
  }
  res[is.na(res)] <- -3
  return(res)
}

data <- data %>%
  mutate(
    hhincband14 = band_with_missing(W1GrsswkHH),
    hhincband15 = band_with_missing(W2GrsswkHH),
    hhincband16 = hhinc16,
    hhincband17 = hhinc17
  )

# Factor labels for banded variables
band_labels <- c(
  "1" = "Up to £49", "2" = "£50 up to £99", "3" = "£100 up to £199",
  "4" = "£200 up to £299", "5" = "£300 up to £399", "6" = "£400 up to £499",
  "7" = "£500 up to £599", "8" = "£600 up to £699", "9" = "£700 up to £799",
  "10" = "£800 up to £899", "11" = "£900 up to £999", "12" = "£1,000 or more",
  "-9" = "Refusal", "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say", "-3" = "Not asked", "-2" = "Schedule not applicable", "-1" = "Item not applicable"
)

# Apply labels as factors
band_vars <- c("hhincband14", "hhincband15", "hhincband16", "hhincband17")
data <- data %>%
  mutate(across(all_of(band_vars), ~ factor(.x, levels = as.numeric(names(band_labels)), labels = band_labels)))

# Final selection
final_data <- data %>%
  select(NSID, hhinc14, hhinc15, hhincband14, hhincband15, hhincband16, hhincband17)

readr::write_csv(final_data, 'data/output/cleaned_data.csv')

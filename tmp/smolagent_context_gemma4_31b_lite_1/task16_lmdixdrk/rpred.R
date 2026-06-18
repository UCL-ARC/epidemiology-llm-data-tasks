library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# File loading
file1 <- "data/input/wave_one_lsype_family_background_2020.tab"
file2 <- "data/input/wave_two_lsype_family_background_2020.tab"
file3 <- "data/input/wave_three_lsype_family_background_2020.tab"
file4 <- "data/input/wave_four_lsype_family_background_2020.tab"

data1 <- read_delim(file1, delim = "\t", col_types = cols(.default = "c"))
data2 <- read_delim(file2, delim = "\t", col_types = cols(.default = "c"))
data3 <- read_delim(file3, delim = "\t", col_types = cols(.default = "c"))
data4 <- read_delim(file4, delim = "\t", col_types = cols(.default = "c"))

# Convert numeric variables to numeric
data1$W1GrsswkHH <- as.numeric(data1$W1GrsswkHH)
data2$W2GrsswkHH <- as.numeric(data2$W2GrsswkHH)
data3$W3incestw <- as.numeric(data3$W3incestw)
data4$w4IncEstW <- as.numeric(data4$w4IncEstW)

# Merge
merged_data <- data1 %>%
  full_join(data2, by = "NSID") %>%
  full_join(data3, by = "NSID") %>%
  full_join(data4, by = "NSID")

# Vectorized missing value mapping function
map_missing_vec <- function(val, mapping) {
  res <- rep(-3, length(val))
  # Match values based on the mapping keys (which are strings of numbers)
  # Convert numeric values to strings to match mapping keys
  val_str <- as.character(val)
  for (key in names(mapping)) {
    res[val_str == key] <- mapping[[key]]
  }
  # Keep valid substantive responses
  valid_idx <- !is.na(val) & !(val_str %in% names(mapping))
  res[valid_idx] <- val[valid_idx]
  return(res)
}

# Mapping for continuous/banded variables based on metadata
mapping_cont <- c(
  "-999" = -2, # Missing in error
  "-992" = -2, # No information
  "-99" = -3,  # HH not interviewed
  "-94" = -8,  # Insufficient information
  "-92" = -9,  # Refused
  "-91" = -1,  # Not applicable
  "-3" = -1,   # Not yet paid
  "-1" = -8    # Don't know
)

mapping_banded <- c(
  "-996" = -1, # No parent in household
  "-99" = -3,  # MP not interviewed
  "-92" = -9,  # Refused
  "-1" = -8    # Don't know
)

# Process variables
merged_data <- merged_data %>%
  mutate(
    hhinc14 = map_missing_vec(W1GrsswkHH, mapping_cont),
    hhinc15 = map_missing_vec(W2GrsswkHH, mapping_cont),
    hhincband16 = map_missing_vec(W3incestw, mapping_banded),
    hhincband17 = map_missing_vec(w4IncEstW, mapping_banded)
  )

# Banding for 14 and 15
band_income <- function(x) {
  case_when(
    x < 0 ~ x, # Keep missing codes
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
    TRUE ~ -3
  )
}

merged_data <- merged_data %>%
  mutate(
    hhincband14 = band_income(hhinc14),
    hhincband15 = band_income(hhinc15)
  )

# Final selection
final_data <- merged_data %>%
  select(NSID, hhinc14, hhincband14, hhinc15, hhincband15, hhincband16, hhincband17)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")
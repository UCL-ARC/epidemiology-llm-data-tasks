# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(haven)

# File paths
base_path <- "data/input/"
files <- list(
  wave1 = "wave_one_lsype_family_background_2020.tab",
  wave2 = "wave_two_lsype_family_background_2020.tab",
  wave3 = "wave_three_lsype_family_background_2020.tab",
  wave4 = "wave_four_lsype_family_background_2020.tab"
)

read_tab <- function(fname) {
  read_delim(paste0(base_path, fname), delim = "\t", col_types = cols(), na = c("", "NA"))
}

w1 <- read_tab(files["wave1"])
w2 <- read_tab(files["wave2"])
w3 <- read_tab(files["wave3"])
w4 <- read_tab(files["wave4"])

# Mapping functions for continuous waves (14 & 15)
map_cont <- function(x) {
  x_mapped <- case_when(
    x == -3  ~ -1,   # Not yet paid -> item not applicable
    x == -1  ~ -8,   # Don’t know -> don\'t know
    x == -992 ~ -9, # No information – refused -> refusal
    x == -996 ~ -3, # No parent in household -> not asked
    x == -999 ~ -2, # Missing in error -> schedule not applicable
    x == -99  ~ -3, # Not interviewed -> not asked
    x == -94  ~ -8, # Insufficient information -> don\'t know
    x == -92  ~ -9, # Refused -> refusal
    x == -91  ~ -1, # Not applicable -> item not applicable
    TRUE ~ x
  )
  x_mapped[is.na(x_mapped)] <- -3
  return(x_mapped)
}

# Mapping functions for banded waves (16 & 17)
map_banded <- function(x) {
  x_mapped <- case_when(
    x == -3  ~ -1,
    x == -1  ~ -8,
    x == -992 ~ -9,
    x == -996 ~ -3,
    x == -999 ~ -2,
    x == -99  ~ -3,
    x == -94  ~ -8,
    x == -92  ~ -9,
    x == -91  ~ -1,
    TRUE ~ x
  )
  x_mapped[is.na(x_mapped)] <- -3
  return(x_mapped)
}

# Apply mapping and create raw and mapped variables
w1 <- w1 %>% mutate(
  incwhhcnt14_raw = W1GrsswkHH,
  incwhhcnt14 = map_cont(W1GrsswkHH)
)

w2 <- w2 %>% mutate(
  incwhhcnt15_raw = W2GrsswkHH,
  incwhhcnt15 = map_cont(W2GrsswkHH)
)

w3 <- w3 %>% mutate(
  incwhh16 = map_banded(W3incestw)
)

w4 <- w4 %>% mutate(
  incwhh17 = map_banded(w4IncEstW)
)

# Band continuous values for ages 14 & 15
breaks <- c(-Inf, 49, 99, 199, 299, 399, 499, 599, 699, 799, 899, 990, Inf)

band_income <- function(x) {
  banded <- ifelse(x > 0, cut(x, breaks = breaks, labels = 1:12, right = TRUE), x)
  as.numeric(as.character(banded))
}

w1 <- w1 %>% mutate(incwhh14 = band_income(incwhhcnt14))
w2 <- w2 %>% mutate(incwhh15 = band_income(incwhhcnt15))

# Merge all waves by NSID
combined <- w1 %>% select(NSID, incwhh14, incwhhcnt14) %>%
  full_join(w2 %>% select(NSID, incwhh15, incwhhcnt15), by = "NSID") %>%
  full_join(w3 %>% select(NSID, incwhh16), by = "NSID") %>%
  full_join(w4 %>% select(NSID, incwhh17), by = "NSID")

# Define labelled vectors (names are labels, values are codes)
band_labels <- c(
  "Refusal" = -9,
  "Don't know / insufficient information" = -8,
  "Prefer not to say" = -7,
  "Not asked" = -3,
  "Schedule not applicable" = -2,
  "Item not applicable" = -1,
  "Up to £49" = 1,
  "£50 up to £99" = 2,
  "£100 up to £199" = 3,
  "£200 up to £299" = 4,
  "£300 up to £399" = 5,
  "£400 up to £499" = 6,
  "£500 up to £599" = 7,
  "£600 up to £699" = 8,
  "£700 up to £799" = 9,
  "£800 up to £899" = 10,
  "£900 up to £990" = 11,
  "£1,000 or more" = 12
)

missing_labels <- c(
  "Refusal" = -9,
  "Don't know / insufficient information" = -8,
  "Not asked" = -3,
  "Schedule not applicable" = -2,
  "Item not applicable" = -1
)

# Apply labelled attributes
combined <- combined %>% mutate(
  incwhh14   = labelled(incwhh14, band_labels),
  incwhh15   = labelled(incwhh15, band_labels),
  incwhh16   = labelled(incwhh16, band_labels),
  incwhh17   = labelled(incwhh17, band_labels),
  incwhhcnt14 = labelled(incwhhcnt14, missing_labels),
  incwhhcnt15 = labelled(incwhhcnt15, missing_labels)
)

# Output
output <- combined %>% select(NSID, incwhh14, incwhhcnt14, incwhh15, incwhhcnt15, incwhh16, incwhh17)
write_csv(output, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
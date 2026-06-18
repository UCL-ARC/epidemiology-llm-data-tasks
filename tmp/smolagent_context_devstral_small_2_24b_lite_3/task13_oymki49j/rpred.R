library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID")

# Define mapping for NS-SEC categories to major categories
nssec_major_mapping <- c(
  `1.0` = 1,
  `2.0` = 2,
  `3.1` = 3, `3.2` = 3, `3.3` = 3, `3.4` = 3,
  `4.1` = 4, `4.2` = 4, `4.3` = 4, `4.4` = 4,
  `5.0` = 5,
  `6.0` = 6,
  `7.1` = 7, `7.2` = 7, `7.3` = 7, `7.4` = 7,
  `8.1` = 8, `8.2` = 8,
  `9.1` = 9, `9.2` = 9,
  `10.0` = 10,
  `11.1` = 11, `11.2` = 11,
  `12.1` = 12, `12.2` = 12, `12.3` = 12, `12.4` = 12, `12.5` = 12, `12.6` = 12, `12.7` = 12,
  `13.1` = 13, `13.2` = 13, `13.3` = 13, `13.4` = 13, `13.5` = 13,
  `14.1` = 14, `14.2` = 14, `14.3` = 14,
  `15.0` = 15,
  `16.0` = 16,
  `17.0` = 17
)

# Function to collapse NS-SEC categories
collapse_nssec <- function(var) {
  var <- as.character(var)
  var[var %in% names(nssec_major_mapping)] <- nssec_major_mapping[var[var %in% names(nssec_major_mapping)]]
  var <- as.numeric(var)
  return(var)
}

# Function to apply standard missing value codes
standardize_missing <- function(var) {
  var <- ifelse(is.na(var), -3, var)
  var <- ifelse(var == -999, -2, var)
  var <- ifelse(var == -99, -3, var)
  var <- ifelse(var == -98, -1, var)
  var <- ifelse(var == -94, -8, var)
  var <- ifelse(var == -92, -9, var)
  var <- ifelse(var == -91, -1, var)
  return(var)
}

# Process each wave and parent
# Age 14
merged_data$nssecma14 <- collapse_nssec(merged_data$W1nsseccatmum)
merged_data$nssecma14 <- standardize_missing(merged_data$nssecma14)

merged_data$nssecpa14 <- collapse_nssec(merged_data$W1nsseccatdad)
merged_data$nssecpa14 <- standardize_missing(merged_data$nssecpa14)

# Age 15
merged_data$nssecma15 <- collapse_nssec(merged_data$W2nsseccatmum)
merged_data$nssecma15 <- standardize_missing(merged_data$nssecma15)

merged_data$nssecpa15 <- collapse_nssec(merged_data$W2nsseccatdad)
merged_data$nssecpa15 <- standardize_missing(merged_data$nssecpa15)

# Age 16
merged_data$nssecma16 <- collapse_nssec(merged_data$W3cnsseccatmum)
merged_data$nssecma16 <- standardize_missing(merged_data$nssecma16)

merged_data$nssecpa16 <- collapse_nssec(merged_data$W3cnsseccatdad)
merged_data$nssecpa16 <- standardize_missing(merged_data$nssecpa16)

# Age 17
merged_data$nssecma17 <- collapse_nssec(merged_data$w4cnsseccatmum)
merged_data$nssecma17 <- standardize_missing(merged_data$nssecma17)

merged_data$nssecpa17 <- collapse_nssec(merged_data$w4cnsseccatdad)
merged_data$nssecpa17 <- standardize_missing(merged_data$nssecpa17)

# Age 18
merged_data$nssecma18 <- collapse_nssec(merged_data$w5Cnsseccatmum)
merged_data$nssecma18 <- standardize_missing(merged_data$nssecma18)

merged_data$nssecpa18 <- collapse_nssec(merged_data$w5Cnsseccatdad)
merged_data$nssecpa18 <- standardize_missing(merged_data$nssecpa18)

# Select only the required variables
output_data <- merged_data %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")
library(readr)
library(dplyr)
library(haven)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID")

# Function to collapse NS-SEC categories
collapse_nssec <- function(x) {
  case_when(
    x %in% c(1.0, 2.0) ~ 1,  # Large employers and higher managerial
    x %in% c(3.1, 3.2, 3.3, 3.4, 4.1, 4.2, 4.3, 4.4) ~ 2,  # Professional occupations
    x %in% c(5.0, 6.0) ~ 3,  # Managerial and supervisory
    x %in% c(7.1, 7.2, 7.3, 7.4) ~ 4,  # Intermediate occupations
    x %in% c(8.1, 8.2, 9.1, 9.2, 10.0) ~ 5,  # Small employers and own account workers
    x %in% c(11.1, 11.2) ~ 6,  # Lower technical
    x %in% c(12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7) ~ 7,  # Semi-routine
    x %in% c(13.1, 13.2, 13.3, 13.4, 13.5) ~ 8,  # Routine
    x %in% c(14.1, 14.2, 15.0) ~ 9,  # Never worked, unemployed, students
    x %in% c(16.0, 17.0) ~ 10,  # Not classified
    TRUE ~ NA_real_
  )
}

# Apply missing value mapping and category collapsing
map_missing <- function(x) {
  case_when(
    x %in% c(-999.0, -99.0, -98.0, -94.0) ~ NA_real_,
    TRUE ~ x
  )
}

# Process each variable
merged_data <- merged_data %>%
  mutate(
    nssecma14 = collapse_nssec(map_missing(W1nsseccatmum)),
    nssecpa14 = collapse_nssec(map_missing(W1nsseccatdad)),
    nssecma15 = collapse_nssec(map_missing(W2nsseccatmum)),
    nssecpa15 = collapse_nssec(map_missing(W2nsseccatdad)),
    nssecma16 = collapse_nssec(map_missing(W3cnsseccatmum)),
    nssecpa16 = collapse_nssec(map_missing(W3cnsseccatdad)),
    nssecma17 = collapse_nssec(map_missing(w4cnsseccatmum)),
    nssecpa17 = collapse_nssec(map_missing(w4cnsseccatdad)),
    nssecma18 = collapse_nssec(map_missing(w5Cnsseccatmum)),
    nssecpa18 = collapse_nssec(map_missing(w5Cnsseccatdad))
  )

# Select only required variables
output_data <- merged_data %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")
library(readr)
library(dplyr)
library(haven)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")

# Define function to collapse NS-SEC codes
collapse_nssec <- function(x) {
  ifelse(is.na(x), -3, floor(x))
}

# Process each wave
wave1 <- wave1 %>%
  mutate(
    nssecma14 = collapse_nssec(W1nsseccatmum),
    nssecpa14 = collapse_nssec(W1nsseccatdad)
  ) %>%
  select(NSID, nssecma14, nssecpa14)

wave2 <- wave2 %>%
  mutate(
    nssecma15 = collapse_nssec(W2nsseccatmum),
    nssecpa15 = collapse_nssec(W2nsseccatdad)
  ) %>%
  select(NSID, nssecma15, nssecpa15)

wave3 <- wave3 %>%
  mutate(
    nssecma16 = collapse_nssec(W3cnsseccatmum),
    nssecpa16 = collapse_nssec(W3cnsseccatdad)
  ) %>%
  select(NSID, nssecma16, nssecpa16)

wave4 <- wave4 %>%
  mutate(
    nssecma17 = collapse_nssec(w4cnsseccatmum),
    nssecpa17 = collapse_nssec(w4cnsseccatdad)
  ) %>%
  select(NSID, nssecma17, nssecpa17)

wave5 <- wave5 %>%
  mutate(
    nssecma18 = collapse_nssec(w5Cnsseccatmum),
    nssecpa18 = collapse_nssec(w5Cnsseccatdad)
  ) %>%
  select(NSID, nssecma18, nssecpa18)

# Merge all waves
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID")

# Replace -98 with -3 for "Parent not present"
merged_data <- merged_data %>%
  mutate(across(where(is.numeric), ~ ifelse(.x == -98, -3, .x)))

# Write output
write_csv(merged_data, "data/output/cleaned_data.csv")
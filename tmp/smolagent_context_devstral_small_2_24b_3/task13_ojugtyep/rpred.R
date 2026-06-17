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

# Define function to collapse NS-SEC codes
harmonise_nssec <- function(x) {
  # Collapse fractional codes to integer part
  x_int <- floor(abs(x))
  
  # Map missing values
  x_int[x == -98] <- -3
  x_int[x == -99] <- -3
  x_int[x == -94] <- -8
  x_int[x == -999] <- -2
  x_int[is.na(x_int)] <- -3
  
  return(x_int)
}

# Apply harmonisation to each wave
wave1 <- wave1 %>%
  mutate(
    nssecma14 = harmonise_nssec(W1nsseccatmum),
    nssecpa14 = harmonise_nssec(W1nsseccatdad)
  )

wave2 <- wave2 %>%
  mutate(
    nssecma15 = harmonise_nssec(W2nsseccatmum),
    nssecpa15 = harmonise_nssec(W2nsseccatdad)
  )

wave3 <- wave3 %>%
  mutate(
    nssecma16 = harmonise_nssec(W3cnsseccatmum),
    nssecpa16 = harmonise_nssec(W3cnsseccatdad)
  )

wave4 <- wave4 %>%
  mutate(
    nssecma17 = harmonise_nssec(w4cnsseccatmum),
    nssecpa17 = harmonise_nssec(w4cnsseccatdad)
  )

wave5 <- wave5 %>%
  mutate(
    nssecma18 = harmonise_nssec(w5Cnsseccatmum),
    nssecpa18 = harmonise_nssec(w5Cnsseccatdad)
  )

# Merge datasets
merged_data <- wave1 %>%
  select(NSID, nssecma14, nssecpa14) %>%
  full_join(wave2 %>% select(NSID, nssecma15, nssecpa15), by = "NSID") %>%
  full_join(wave3 %>% select(NSID, nssecma16, nssecpa16), by = "NSID") %>%
  full_join(wave4 %>% select(NSID, nssecma17, nssecpa17), by = "NSID") %>%
  full_join(wave5 %>% select(NSID, nssecma18, nssecpa18), by = "NSID")

# Write output
write_csv(merged_data, "data/output/cleaned_data.csv")
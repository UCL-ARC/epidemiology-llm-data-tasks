library(readr)
library(dplyr)
library(haven)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")

# Define a function to collapse fractional NS-SEC codes to major categories
collapse_nssec <- function(x) {
  ifelse(is.na(x), NA, floor(x))
}

# Define a function to handle missing values
handle_missing <- function(x) {
  x <- na_if(x, -999)
  x <- na_if(x, -99)
  x <- na_if(x, -94)
  x <- ifelse(x == -98, -3, x)
  x <- ifelse(is.na(x), -3, x)
  x
}

# Process each wave for mother and father
wave1 <- wave1 %>%
  mutate(
    nssecma14 = collapse_nssec(W1nsseccatmum) %>% handle_missing(),
    nssecpa14 = collapse_nssec(W1nsseccatdad) %>% handle_missing()
  ) %>%
  select(NSID, nssecma14, nssecpa14)

wave2 <- wave2 %>%
  mutate(
    nssecma15 = collapse_nssec(W2nsseccatmum) %>% handle_missing(),
    nssecpa15 = collapse_nssec(W2nsseccatdad) %>% handle_missing()
  ) %>%
  select(NSID, nssecma15, nssecpa15)

wave3 <- wave3 %>%
  mutate(
    nssecma16 = collapse_nssec(W3cnsseccatmum) %>% handle_missing(),
    nssecpa16 = collapse_nssec(W3cnsseccatdad) %>% handle_missing()
  ) %>%
  select(NSID, nssecma16, nssecpa16)

wave4 <- wave4 %>%
  mutate(
    nssecma17 = collapse_nssec(w4cnsseccatmum) %>% handle_missing(),
    nssecpa17 = collapse_nssec(w4cnsseccatdad) %>% handle_missing()
  ) %>%
  select(NSID, nssecma17, nssecpa17)

wave5 <- wave5 %>%
  mutate(
    nssecma18 = collapse_nssec(w5Cnsseccatmum) %>% handle_missing(),
    nssecpa18 = collapse_nssec(w5Cnsseccatdad) %>% handle_missing()
  ) %>%
  select(NSID, nssecma18, nssecpa18)

# Merge all waves
cleaned_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID")

# Write output
write_csv(cleaned_data, "data/output/cleaned_data.csv")
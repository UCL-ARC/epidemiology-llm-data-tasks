library(readr)
library(dplyr)
library(purrr)

# Function to collapse detailed NS-SEC codes to major categories and map missing codes
map_nssec <- function(x) {
  dplyr::case_when(
    # schedule / error / not present
    x %in% c(-999.0, -98.0) ~ -2,
    # not asked
    x == -99.0 ~ -3,
    # insufficient information
    x == -94.0 ~ -9,
    # major categories
    x == 1.0 ~ 1,
    x == 2.0 ~ 2,
    x %in% c(3.1,3.2,3.3,3.4) ~ 3,
    x %in% c(4.1,4.2,4.3,4.4) ~ 4,
    x == 5.0 ~ 5,
    x == 6.0 ~ 6,
    x %in% c(7.1,7.2,7.3,7.4) ~ 7,
    x %in% c(8.1,8.2) ~ 8,
    x %in% c(9.1,9.2) ~ 9,
    x == 10.0 ~ 10,
    x %in% c(11.1,11.2) ~ 11,
    x %in% c(12.1,12.2,12.3,12.4,12.5,12.6,12.7) ~ 12,
    x %in% c(13.1,13.2,13.3,13.4,13.5) ~ 13,
    x %in% c(14.1,14.2,14.3) ~ 14,
    x == 15.0 ~ 15,
    x == 16.0 ~ 16,
    x == 17.0 ~ 17,
    TRUE ~ NA_real_
  )
}

# Replace NA with -3 after mapping
clean_missing <- function(x) {
  x[is.na(x)] <- -3
  x
}

# Wave 1 – age 14
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = col_double(), NSID = col_character())) %>%
  select(NSID, W1nsseccatmum, W1nsseccatdad) %>%
  mutate(
    nssecma14 = clean_missing(map_nssec(W1nsseccatmum)),
    nssecpa14 = clean_missing(map_nssec(W1nsseccatdad))
  ) %>%
  select(NSID, nssecma14, nssecpa14)

# Wave 2 – age 15
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = col_double(), NSID = col_character())) %>%
  select(NSID, W2nsseccatmum, W2nsseccatdad) %>%
  mutate(
    nssecma15 = clean_missing(map_nssec(W2nsseccatmum)),
    nssecpa15 = clean_missing(map_nssec(W2nsseccatdad))
  ) %>%
  select(NSID, nssecma15, nssecpa15)

# Wave 3 – age 16
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = col_double(), NSID = col_character())) %>%
  select(NSID, W3cnsseccatmum, W3cnsseccatdad) %>%
  mutate(
    nssecma16 = clean_missing(map_nssec(W3cnsseccatmum)),
    nssecpa16 = clean_missing(map_nssec(W3cnsseccatdad))
  ) %>%
  select(NSID, nssecma16, nssecpa16)

# Wave 4 – age 17
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = col_double(), NSID = col_character())) %>%
  select(NSID, w4cnsseccatmum, w4cnsseccatdad) %>%
  mutate(
    nssecma17 = clean_missing(map_nssec(w4cnsseccatmum)),
    nssecpa17 = clean_missing(map_nssec(w4cnsseccatdad))
  ) %>%
  select(NSID, nssecma17, nssecpa17)

# Wave 5 – age 18 – no parental data; set to missing (-3)
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = col_double(), NSID = col_character())) %>%
  select(NSID) %>%
  mutate(
    nssecma18 = -3,
    nssecpa18 = -3
  )

# Merge all waves by NSID
merged <- reduce(list(wave1, wave2, wave3, wave4, wave5), full_join, by = "NSID")

# Write output
write_csv(merged, "data/output/cleaned_data.csv")
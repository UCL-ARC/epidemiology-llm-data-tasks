library(haven)
library(dplyr)
library(purrr)
library(readr)

# Load datasets
wave1 <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- readr::read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID")

# Harmonize NS-SEC variables
merged_data <- merged_data %>%
  mutate(
    nssecma14 = ifelse(W1nsseccatmum >= 1 & W1nsseccatmum <= 17, floor(W1nsseccatmum), NA_integer_),
    nssecpa14 = ifelse(W1nsseccatdad >= 1 & W1nsseccatdad <= 17, floor(W1nsseccatdad), NA_integer_),
    nssecma15 = ifelse(W2nsseccatmum >= 1 & W2nsseccatmum <= 17, floor(W2nsseccatmum), NA_integer_),
    nssecpa15 = ifelse(W2nsseccatdad >= 1 & W2nsseccatdad <= 17, floor(W2nsseccatdad), NA_integer_),
    nssecma16 = ifelse(W3cnsseccatmum >= 1 & W3cnsseccatmum <= 17, floor(W3cnsseccatmum), NA_integer_),
    nssecpa16 = ifelse(W3cnsseccatdad >= 1 & W3cnsseccatdad <= 17, floor(W3cnsseccatdad), NA_integer_),
    nssecma17 = ifelse(w4cnsseccatmum >= 1 & w4cnsseccatmum <= 17, floor(w4cnsseccatmum), NA_integer_),
    nssecpa17 = ifelse(w4cnsseccatdad >= 1 & w4cnsseccatdad <= 17, floor(w4cnsseccatdad), NA_integer_),
    nssecma18 = ifelse(w5Cnsseccatmum >= 1 & w5Cnsseccatmum <= 17, floor(w5Cnsseccatmum), NA_integer_),
    nssecpa18 = ifelse(w5Cnsseccatdad >= 1 & w5Cnsseccatdad <= 17, floor(w5Cnsseccatdad), NA_integer_)
  )

# Harmonize missing value codes
merged_data <- merged_data %>%
  mutate(
    nssecma14 = case_when(
      nssecma14 == -999 ~ -2,
      nssecma14 == -94 ~ -8,
      nssecma14 == -99 | nssecma14 == -98 | is.na(nssecma14) ~ -3,
      TRUE ~ nssecma14
    ),
    nssecpa14 = case_when(
      nssecpa14 == -999 ~ -2,
      nssecpa14 == -94 ~ -8,
      nssecpa14 == -99 | nssecpa14 == -98 | is.na(nssecpa14) ~ -3,
      TRUE ~ nssecpa14
    ),
    nssecma15 = case_when(
      nssecma15 == -999 ~ -2,
      nssecma15 == -94 ~ -8,
      nssecma15 == -99 | nssecma15 == -98 | is.na(nssecma15) ~ -3,
      TRUE ~ nssecma15
    ),
    nssecpa15 = case_when(
      nssecpa15 == -999 ~ -2,
      nssecpa15 == -94 ~ -8,
      nssecpa15 == -99 | nssecpa15 == -98 | is.na(nssecpa15) ~ -3,
      TRUE ~ nssecpa15
    ),
    nssecma16 = case_when(
      nssecma16 == -999 ~ -2,
      nssecma16 == -94 ~ -8,
      nssecma16 == -99 | nssecma16 == -98 | is.na(nssecma16) ~ -3,
      TRUE ~ nssecma16
    ),
    nssecpa16 = case_when(
      nssecpa16 == -999 ~ -2,
      nssecpa16 == -94 ~ -8,
      nssecpa16 == -99 | nssecpa16 == -98 | is.na(nssecpa16) ~ -3,
      TRUE ~ nssecpa16
    ),
    nssecma17 = case_when(
      nssecma17 == -999 ~ -2,
      nssecma17 == -94 ~ -8,
      nssecma17 == -99 | nssecma17 == -98 | is.na(nssecma17) ~ -3,
      TRUE ~ nssecma17
    ),
    nssecpa17 = case_when(
      nssecpa17 == -999 ~ -2,
      nssecpa17 == -94 ~ -8,
      nssecpa17 == -99 | nssecpa17 == -98 | is.na(nssecpa17) ~ -3,
      TRUE ~ nssecpa17
    ),
    nssecma18 = case_when(
      nssecma18 == -999 ~ -2,
      nssecma18 == -94 ~ -8,
      nssecma18 == -99 | nssecma18 == -98 | is.na(nssecma18) ~ -3,
      TRUE ~ nssecma18
    ),
    nssecpa18 = case_when(
      nssecpa18 == -999 ~ -2,
      nssecpa18 == -94 ~ -8,
      nssecpa18 == -99 | nssecpa18 == -98 | is.na(nssecpa18) ~ -3,
      TRUE ~ nssecpa18
    )
  )

# Select and output required variables
output_data <- merged_data %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)

# Write output
write.csv(output_data, "data/output/cleaned_data.csv", row.names = FALSE)
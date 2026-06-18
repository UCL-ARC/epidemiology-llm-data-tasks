library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load the five input files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")

# Process wave 1 (age 14)
wave1_proc <- wave1 %>%
  mutate(
    nssecma14 = case_when(
      is.na(W1nsseccatmum) | W1nsseccatmum %in% c(-999, -99, -94, -98) ~ -3,
      TRUE ~ as.integer(W1nsseccatmum)
    ),
    nssecpa14 = case_when(
      is.na(W1nsseccatdad) | W1nsseccatdad %in% c(-999, -99, -94, -98) ~ -3,
      TRUE ~ as.integer(W1nsseccatdad)
    )
  )

# Process wave 2 (age 15)
wave2_proc <- wave2 %>%
  mutate(
    nssecma15 = case_when(
      is.na(W2nsseccatmum) | W2nsseccatmum %in% c(-999, -99, -94, -98) ~ -3,
      TRUE ~ as.integer(W2nsseccatmum)
    ),
    nssecpa15 = case_when(
      is.na(W2nsseccatdad) | W2nsseccatdad %in% c(-999, -99, -94, -98) ~ -3,
      TRUE ~ as.integer(W2nsseccatdad)
    )
  )

# Process wave 3 (age 16)
wave3_proc <- wave3 %>%
  mutate(
    nssecma16 = case_when(
      is.na(W3cnsseccatmum) | W3cnsseccatmum %in% c(-999, -99, -94, -98) ~ -3,
      TRUE ~ as.integer(W3cnsseccatmum)
    ),
    nssecpa16 = case_when(
      is.na(W3cnsseccatdad) | W3cnsseccatdad %in% c(-999, -99, -94, -98) ~ -3,
      TRUE ~ as.integer(W3cnsseccatdad)
    )
  )

# Process wave 4 (age 17)
wave4_proc <- wave4 %>%
  mutate(
    nssecma17 = case_when(
      is.na(w4cnsseccatmum) | w4cnsseccatmum %in% c(-999, -99, -94, -98) ~ -3,
      TRUE ~ as.integer(w4cnsseccatmum)
    ),
    nssecpa17 = case_when(
      is.na(w4cnsseccatdad) | w4cnsseccatdad %in% c(-999, -99, -94, -98) ~ -3,
      TRUE ~ as.integer(w4cnsseccatdad)
    )
  )

# Process wave 5 (age 18)
wave5_proc <- wave5 %>%
  mutate(
    nssecma18 = case_when(
      is.na(w5Cnsseccatmum) | w5Cnsseccatmum %in% c(-999, -99, -94, -98) ~ -3,
      TRUE ~ as.integer(w5Cnsseccatmum)
    ),
    nssecpa18 = case_when(
      is.na(w5Cnsseccatdad) | w5Cnsseccatdad %in% c(-999, -99, -94, -98) ~ -3,
      TRUE ~ as.integer(w5Cnsseccatdad)
    )
  )

# Combine all waves by NSID
final_data <- full_join(wave1_proc, wave2_proc, by = "NSID") %>%
  full_join(wave3_proc, by = "NSID") %>%
  full_join(wave4_proc, by = "NSID") %>%
  full_join(wave5_proc, by = "NSID")

# Write to output CSV
write_csv(final_data, "data/output/cleaned_data.csv")
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(haven)

# Load files
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = col_guess()))
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = col_guess()))
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = col_guess()))
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = col_guess()))
wave_eight <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", col_types = cols(.default = col_guess()))
wave_nine <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", col_types = cols(.default = col_guess()))

# Merge datasets by NSID
merged <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Function to recode sexual orientation variables
recode_sori <- function(x, special5 = FALSE) {
  y <- case_when(
    special5 & x == 5 ~ -7,
    x %in% c(-97, -100) ~ -9,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -99 ~ -3,
    x %in% c(-999, -998, -997, -995) ~ -2,
    TRUE ~ x
  )
  y[is.na(y)] <- -3
  as.integer(y)
}

# Derive consolidated sexual orientation variables
derived <- merged %>%
  mutate(
    sori19 = recode_sori(W6SexualityYP, special5 = FALSE),
    sori20 = recode_sori(W7SexualityYP, special5 = FALSE),
    sori25 = recode_sori(W8SEXUALITY, special5 = FALSE),
    sori32 = recode_sori(W9SORI, special5 = TRUE)
  ) %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write cleaned data to CSV
write_csv(derived, "data/output/cleaned_data.csv")
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

merged <- full_join(wave1, wave4, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

cleaned <- merged %>% select(NSID, W8DINCB, W9DINCB) %>%
  rename(inc25 = W8DINCB, inc32 = W9DINCB)

cleaned <- cleaned %>% mutate(
  inc25 = case_when(
    is.na(inc25) ~ -3,
    inc25 == -1 ~ -1,
    TRUE ~ inc25
  ),
  inc32 = case_when(
    is.na(inc32) ~ -3,
    inc32 == -1 ~ -1,
    TRUE ~ inc32
  )
)

cleaned <- cleaned %>% mutate(
  inc25 = labelled(inc25, labels = c(
    "less than 25" = 1,
    "25 to 50" = 2,
    "50 to 90" = 3,
    "90 to 140" = 4,
    "140 to 240" = 5,
    "240 to 300" = 6,
    "300 to 350" = 7,
    "350 to 400" = 8,
    "400 to 500" = 9,
    "500 to 600" = 10,
    "600 to 700" = 11,
    "700 to 800" = 12,
    "800 to 900" = 13,
    "900 to 1200" = 14,
    "1200 to 1400" = 15,
    "more than 1400" = 16,
    "Refusal" = -9,
    "Don't know/insufficient information" = -8,
    "Item not applicable" = -1,
    "Not asked/interviewed" = -3,
    "Script error/lost" = -2
  )),
  inc32 = labelled(inc32, labels = c(
    "less than 25" = 1,
    "25 to 50" = 2,
    "50 to 90" = 3,
    "90 to 140" = 4,
    "140 to 240" = 5,
    "240 to 300" = 6,
    "300 to 350" = 7,
    "350 to 400" = 8,
    "400 to 500" = 9,
    "500 to 600" = 10,
    "600 to 700" = 11,
    "700 to 800" = 12,
    "800 to 900" = 13,
    "900 to 1200" = 14,
    "1200 to 1400" = 15,
    "more than 1400" = 16,
    "Refusal" = -9,
    "Don't know/insufficient information" = -8,
    "Item not applicable" = -1,
    "Not asked/interviewed" = -3,
    "Script error/lost" = -2
  ))
)

write_csv(cleaned, "data/output/cleaned_data.csv")
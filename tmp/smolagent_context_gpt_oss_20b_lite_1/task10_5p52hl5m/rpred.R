library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# mapping function for missing values
map_missing <- function(x){
  x <- ifelse(is.na(x), -3, x)
  x <- case_when(
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -99 ~ -3,
    TRUE ~ x
  )
  return(x)
}

# read files
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = col_guess()))
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = col_guess()))
wave_five <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = col_guess()))
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = col_guess()))
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = col_guess()))
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = cols(.default = col_guess()))
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = cols(.default = col_guess()))

# merge
full <- list(wave_one, wave_four, wave_five, wave_six, wave_seven, ns8, ns9) %>%
  reduce(full_join, by = "NSID")

# map missing codes
full <- full %>%
  mutate(
    W4empsYP = map_missing(W4empsYP),
    W5mainactYP = map_missing(W5mainactYP),
    W6TCurrentAct = map_missing(W6TCurrentAct),
    W7TCurrentAct = map_missing(W7TCurrentAct),
    W8DACTIVITYC = map_missing(W8DACTIVITYC),
    W9DACTIVITYC = map_missing(W9DACTIVITYC)
  )

# create collapsed 6-category variables
full <- full %>%
  mutate(
    ecoact17 = case_when(
      W4empsYP %in% c(1,2) ~ 1,
      W4empsYP == 3 ~ 3,
      W4empsYP %in% c(4) ~ 4,
      W4empsYP %in% c(5) ~ 2,
      W4empsYP %in% c(6) ~ 5,
      W4empsYP %in% c(7,8,9) ~ 6,
      TRUE ~ W4empsYP
    ),
    ecoact18 = case_when(
      W5mainactYP %in% c(1,2,3) ~ 1,
      W5mainactYP == 4 ~ 2,
      W5mainactYP %in% c(5,6) ~ 4,
      W5mainactYP == 7 ~ 3,
      W5mainactYP == 8 ~ 5,
      W5mainactYP %in% c(9,10,11) ~ 6,
      TRUE ~ W5mainactYP
    ),
    ecoact19 = case_when(
      W6TCurrentAct %in% c(3,10) ~ 1,
      W6TCurrentAct %in% c(1,2) ~ 2,
      W6TCurrentAct %in% c(4,5) ~ 4,
      W6TCurrentAct == 8 ~ 3,
      W6TCurrentAct == 7 ~ 5,
      W6TCurrentAct %in% c(6,9,11) ~ 6,
      TRUE ~ W6TCurrentAct
    ),
    ecoact20 = case_when(
      W7TCurrentAct %in% c(3,9) ~ 1,
      W7TCurrentAct %in% c(1,2) ~ 2,
      W7TCurrentAct %in% c(4,5) ~ 4,
      W7TCurrentAct == 8 ~ 3,
      W7TCurrentAct == 7 ~ 5,
      W7TCurrentAct %in% c(6,10,11,12,13,14,15) ~ 6,
      TRUE ~ W7TCurrentAct
    ),
    ecoact25 = case_when(
      W8DACTIVITYC %in% c(1,2) ~ 1,
      W8DACTIVITYC == 5 ~ 2,
      W8DACTIVITYC %in% c(4) ~ 3,
      W8DACTIVITYC %in% c(6,7) ~ 4,
      W8DACTIVITYC == 9 ~ 5,
      W8DACTIVITYC %in% c(3,8,10) ~ 6,
      TRUE ~ W8DACTIVITYC
    ),
    ecoact32 = case_when(
      W9DACTIVITYC %in% c(1,2) ~ 1,
      W9DACTIVITYC == 5 ~ 2,
      W9DACTIVITYC %in% c(4) ~ 3,
      W9DACTIVITYC %in% c(6,7) ~ 4,
      W9DACTIVITYC == 9 ~ 5,
      W9DACTIVITYC %in% c(3,8,10) ~ 6,
      TRUE ~ W9DACTIVITYC
    ),
    ecoactadu25 = W8DACTIVITYC,
    ecoactadu32 = W9DACTIVITYC
  )

# final selection
final <- full %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# write CSV
write_csv(final, "data/output/cleaned_data.csv")

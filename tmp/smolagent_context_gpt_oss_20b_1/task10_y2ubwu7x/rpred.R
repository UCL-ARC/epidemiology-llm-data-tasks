library(readr)
library(dplyr)

# Define file paths
files <- list(
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_five = "data/input/wave_five_lsype_young_person_2020.tab",
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "data/input/wave_seven_lsype_young_person_2020.tab",
  ns8 = "data/input/ns8_2015_derived.tab",
  ns9 = "data/input/ns9_2022_derived_variables.tab"
)

# Load datasets
wave4 <- read_delim(files$wave_four, delim = "\t", show_col_types = FALSE)
wave5 <- read_delim(files$wave_five, delim = "\t", show_col_types = FALSE)
wave6 <- read_delim(files$wave_six, delim = "\t", show_col_types = FALSE)
wave7 <- read_delim(files$wave_seven, delim = "\t", show_col_types = FALSE)
ns8  <- read_delim(files$ns8, delim = "\t", show_col_types = FALSE)
ns9  <- read_delim(files$ns9, delim = "\t", show_col_types = FALSE)

# Collapsed variables
wave4 <- wave4 %>% mutate(
  ecoact17 = case_when(
    W4empsYP %in% c(1.0, 2.0) ~ 1L,
    W4empsYP == 4.0 ~ 2L,
    W4empsYP == 5.0 ~ 3L,
    W4empsYP == 3.0 ~ 4L,
    W4empsYP == 6.0 ~ 5L,
    W4empsYP %in% c(7.0, 8.0, 9.0) ~ 6L,
    W4empsYP == -999.0 ~ -2L,
    W4empsYP == -94.0 ~ -8L,
    W4empsYP == -92.0 ~ -9L,
    W4empsYP == -91.0 ~ -1L,
    TRUE ~ NA_real_
  )
)

wave5 <- wave5 %>% mutate(
  ecoact18 = case_when(
    W5mainactYP == 3.0 ~ 1L,
    W5mainactYP %in% c(1.0, 2.0, 5.0, 6.0) ~ 2L,
    W5mainactYP == 4.0 ~ 3L,
    W5mainactYP == 7.0 ~ 4L,
    W5mainactYP == 8.0 ~ 5L,
    W5mainactYP %in% c(9.0, 10.0, 11.0) ~ 6L,
    W5mainactYP == -94.0 ~ -8L,
    TRUE ~ NA_real_
  )
)

wave6 <- wave6 %>% mutate(
  ecoact19 = case_when(
    W6TCurrentAct == 3.0 ~ 1L,
    W6TCurrentAct %in% c(1.0, 2.0) ~ 3L,
    W6TCurrentAct %in% c(4.0, 5.0, 10.0) ~ 2L,
    W6TCurrentAct == 8.0 ~ 4L,
    W6TCurrentAct == 7.0 ~ 5L,
    W6TCurrentAct %in% c(6.0, 9.0, 11.0) ~ 6L,
    W6TCurrentAct == -91.0 ~ -2L,
    TRUE ~ NA_real_
  )
)

wave7 <- wave7 %>% mutate(
  ecoact20 = case_when(
    W7TCurrentAct %in% c(1.0, 2.0, 9.0) ~ 3L,
    W7TCurrentAct == 3.0 ~ 1L,
    W7TCurrentAct %in% c(4.0, 5.0) ~ 2L,
    W7TCurrentAct == 6.0 ~ 6L,
    W7TCurrentAct == 7.0 ~ 5L,
    W7TCurrentAct == 8.0 ~ 4L,
    W7TCurrentAct %in% c(10.0, 11.0, 12.0, 13.0, 14.0, 15.0) ~ 6L,
    W7TCurrentAct == -91.0 ~ -1L,
    TRUE ~ NA_real_
  )
)

ns8 <- ns8 %>% mutate(
  ecoact25 = case_when(
    W8DACTIVITYC %in% c(1.0, 2.0) ~ 1L,
    W8DACTIVITYC == 3.0 ~ 6L,
    W8DACTIVITYC == 4.0 ~ 4L,
    W8DACTIVITYC == 5.0 ~ 3L,
    W8DACTIVITYC %in% c(6.0, 7.0) ~ 2L,
    W8DACTIVITYC == 8.0 ~ 6L,
    W8DACTIVITYC == 9.0 ~ 5L,
    W8DACTIVITYC == 10.0 ~ 6L,
    W8DACTIVITYC == -9.0 ~ -9L,
    W8DACTIVITYC == -8.0 ~ -8L,
    W8DACTIVITYC == -1.0 ~ -1L,
    TRUE ~ NA_real_
  ),
  ecoactadu25 = case_when(
    W8DACTIVITYC %in% c(-9.0, -8.0, -1.0) ~ W8DACTIVITYC,
    TRUE ~ W8DACTIVITYC
  )
)

ns9 <- ns9 %>% mutate(
  ecoact32 = case_when(
    W9DACTIVITYC %in% c(1.0, 2.0) ~ 1L,
    W9DACTIVITYC == 3.0 ~ 6L,
    W9DACTIVITYC == 4.0 ~ 4L,
    W9DACTIVITYC == 5.0 ~ 3L,
    W9DACTIVITYC %in% c(6.0, 7.0) ~ 2L,
    W9DACTIVITYC == 8.0 ~ 6L,
    W9DACTIVITYC == 9.0 ~ 5L,
    W9DACTIVITYC == 10.0 ~ 6L,
    W9DACTIVITYC == -9.0 ~ -9L,
    W9DACTIVITYC == -8.0 ~ -8L,
    W9DACTIVITYC == -1.0 ~ -1L,
    TRUE ~ NA_real_
  ),
  ecoactadu32 = case_when(
    W9DACTIVITYC %in% c(-9.0, -8.0, -1.0) ~ W9DACTIVITYC,
    TRUE ~ W9DACTIVITYC
  )
)

# Merge all waves on NSID
final <- wave4 %>% 
  select(NSID, ecoact17) %>%
  full_join(select(wave5, NSID, ecoact18), by = "NSID") %>%
  full_join(select(wave6, NSID, ecoact19), by = "NSID") %>%
  full_join(select(wave7, NSID, ecoact20), by = "NSID") %>%
  full_join(select(ns8, NSID, ecoact25, ecoactadu25), by = "NSID") %>%
  full_join(select(ns9, NSID, ecoact32, ecoactadu32), by = "NSID")

# Write output
write_csv(final, "data/output/cleaned_data.csv")
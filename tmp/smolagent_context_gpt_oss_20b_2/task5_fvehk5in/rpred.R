library(readr)
library(dplyr)
library(labelled)

# Load files
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols(), trim_ws = TRUE)
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols(), trim_ws = TRUE)
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", col_types = cols(), trim_ws = TRUE)
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = cols(), trim_ws = TRUE)
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = cols(), trim_ws = TRUE)

# Merge by NSID
merged_df <- w1 %>%
  full_join(w4, by = "NSID") %>%
  full_join(w6, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Derive partnership variables
final_df <- merged_df %>%
  mutate(
    partnr19 = case_when(
      is.na(W6MarStatYP) ~ -3L,
      W6MarStatYP == -997 ~ -2L,
      W6MarStatYP == -999 ~ -2L,
      W6MarStatYP == -998 ~ -2L,
      W6MarStatYP == -995 ~ -2L,
      W6MarStatYP == -97  ~ -9L,
      W6MarStatYP == -92  ~ -9L,
      W6MarStatYP == -91  ~ -1L,
      W6MarStatYP == -1   ~ -8L,
      TRUE ~ as.integer(W6MarStatYP)
    ),
    partnradu25 = case_when(
      is.na(W8DMARSTAT) ~ -3L,
      W8DMARSTAT == -9 ~ -9L,
      W8DMARSTAT == -8 ~ -8L,
      W8DMARSTAT == -1 ~ -1L,
      TRUE ~ as.integer(W8DMARSTAT)
    ),
    partnradu32 = case_when(
      is.na(W9DMARSTAT) ~ -3L,
      W9DMARSTAT == -9 ~ -9L,
      W9DMARSTAT == -8 ~ -8L,
      W9DMARSTAT == -1 ~ -1L,
      TRUE ~ as.integer(W9DMARSTAT)
    ),
    partnr25 = case_when(
      partnradu25 %in% 1:9 ~ recode(partnradu25,
                                   `1` = 1, `2` = 2, `3` = 3, `4` = 4,
                                   `5` = 5, `6` = 6, `7` = 3, `8` = 7, `9` = 8),
      TRUE ~ partnradu25
    ),
    partnr32 = case_when(
      partnradu32 %in% 1:8 ~ recode(partnradu32,
                                   `1` = 1, `2` = 2, `3` = 4, `4` = 3,
                                   `5` = 5, `6` = 6, `7` = 7, `8` = 8),
      TRUE ~ partnradu32
    )
  ) %>%
  select(NSID, partnr19, partnradu25, partnr25, partnradu32, partnr32)

write_csv(final_df, "data/output/cleaned_data.csv")
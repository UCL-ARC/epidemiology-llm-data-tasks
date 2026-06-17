library(readr)
library(dplyr)
library(tidyr)
library(labelled)

na_to_miss3 <- function(v) ifelse(is.na(v), -3, v)

wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols(), progress = FALSE)
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols(), progress = FALSE)
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", col_types = cols(), progress = FALSE)
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", col_types = cols(), progress = FALSE)
ns8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", col_types = cols(), progress = FALSE)
ns9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", col_types = cols(), progress = FALSE)

merged <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

merged <- merged %>%
  mutate(
    sori19 = case_when(
      W6SexualityYP %in% c(1,2,3,4) ~ W6SexualityYP,
      W6SexualityYP %in% c(-97, -92) ~ -9,
      W6SexualityYP == -91 ~ -1,
      W6SexualityYP == -1 ~ -8,
      TRUE ~ NA_real_
    ),
    sori20 = case_when(
      W7SexualityYP %in% c(1,2,3,4) ~ W7SexualityYP,
      W7SexualityYP %in% c(-100, -97, -92) ~ -9,
      W7SexualityYP == -91 ~ -1,
      W7SexualityYP == -1 ~ -8,
      TRUE ~ NA_real_
    ),
    sori25 = case_when(
      W8SEXUALITY %in% c(1,2,3,4) ~ W8SEXUALITY,
      W8SEXUALITY == -9 ~ -9,
      W8SEXUALITY == -8 ~ -8,
      W8SEXUALITY == -1 ~ -1,
      TRUE ~ NA_real_
    ),
    sori32 = case_when(
      W9SORI %in% c(1,2,3,4) ~ W9SORI,
      W9SORI == 5 ~ -7,
      W9SORI %in% c(-9, -8, -3, -1) ~ W9SORI,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    sori19 = na_to_miss3(sori19),
    sori20 = na_to_miss3(sori20),
    sori25 = na_to_miss3(sori25),
    sori32 = na_to_miss3(sori32)
  )

final_df <- merged %>% select(NSID, sori19, sori20, sori25, sori32)

write_csv(final_df, "data/output/cleaned_data.csv", na = "")
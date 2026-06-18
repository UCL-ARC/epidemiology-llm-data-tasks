#!/usr/bin/env Rscript
# Clean and harmonise sexual orientation variables across waves
library(readr)
library(dplyr)

df_wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = cols())
df_wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = cols())
df_wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', col_types = cols())
df_wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', col_types = cols())
df_wave8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t', col_types = cols())
df_wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', col_types = cols())

df6_raw <- df_wave6 %>% select(NSID, sori19_raw = W6SexualityYP)
df7_raw <- df_wave7 %>% select(NSID, sori20_raw = W7SexualityYP)
df8_raw <- df_wave8 %>% select(NSID, sori25_raw = W8SEXUALITY)
df9_raw <- df_wave9 %>% select(NSID, sori32_raw = W9SORI)

df_cohort <- df_wave1 %>% select(NSID) %>%
  full_join(df_wave4 %>% select(NSID), by = 'NSID') %>%
  full_join(df_wave6 %>% select(NSID), by = 'NSID') %>%
  full_join(df_wave7 %>% select(NSID), by = 'NSID') %>%
  full_join(df_wave8 %>% select(NSID), by = 'NSID') %>%
  full_join(df_wave9 %>% select(NSID), by = 'NSID')

df_cohort <- df_cohort %>%
  left_join(df6_raw, by = 'NSID') %>%
  left_join(df7_raw, by = 'NSID') %>%
  left_join(df8_raw, by = 'NSID') %>%
  left_join(df9_raw, by = 'NSID')

lbls <- c(
  '1' = 'Heterosexual / Straight',
  '2' = 'Gay / Lesbian',
  '3' = 'Bisexual',
  '4' = 'Other',
  '-9' = 'Refused',
  '-8' = 'Don’t know',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at fieldwork stage',
  '-2' = 'Schedule not applicable',
  '-1' = 'Item not applicable'
)

recode_wave <- function(raw, valid, ref, dontknow, notapp, prefer) {
  rec <- case_when(
    is.na(raw) ~ -3,
    raw %in% valid ~ raw,
    raw %in% ref ~ -9,
    raw %in% prefer ~ -7,
    raw %in% dontknow ~ -8,
    raw %in% notapp ~ -1,
    TRUE ~ -3
  )
  factor(rec,
         levels = c(1,2,3,4,-9,-8,-7,-3,-2,-1),
         labels = c('Heterosexual / Straight',
                    'Gay / Lesbian',
                    'Bisexual',
                    'Other',
                    'Refused',
                    'Don’t know',
                    'Prefer not to say',
                    'Not asked at fieldwork stage',
                    'Schedule not applicable',
                    'Item not applicable'))
}

df_clean <- df_cohort %>%
  mutate(
    sori19 = recode_wave(
      raw = sori19_raw,
      valid = 1:4,
      ref = c(-97,-92),
      dontknow = c(-1),
      notapp = c(-91),
      prefer = integer(0)
    ),
    sori20 = recode_wave(
      raw = sori20_raw,
      valid = 1:4,
      ref = c(-100,-97,-92),
      dontknow = c(-1),
      notapp = c(-91),
      prefer = integer(0)
    ),
    sori25 = recode_wave(
      raw = sori25_raw,
      valid = 1:4,
      ref = c(-9),
      dontknow = c(-8),
      notapp = c(-1),
      prefer = integer(0)
    ),
    sori32 = recode_wave(
      raw = sori32_raw,
      valid = 1:4,
      ref = c(-9),
      dontknow = c(-8),
      notapp = c(-1,-3),
      prefer = c(5)
    )
  )

final_df <- df_clean %>% select(NSID, sori19, sori20, sori25, sori32)

write_csv(final_df, 'data/output/cleaned_data.csv')

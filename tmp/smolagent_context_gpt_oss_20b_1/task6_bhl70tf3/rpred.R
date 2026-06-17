library(readr)
library(dplyr)
library(tidyr)
library(labelled)

map_urbind_gor <- function(x) {
  case_when(
    x == -94 ~ -8,
    x == -1  ~ -1,
    x < 0    ~ -2,
    TRUE      ~ x
  )
}

map_gor_w8w9 <- function(x) {
  case_when(
    x == 13 ~ -2,
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    x < 0  ~ -2,
    TRUE      ~ x
  )
}

map_nationres <- function(x) {
  case_when(
    x %in% 1:4 ~ 1,
    x == 5   ~ 2,
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -3 ~ -3,
    x == -1 ~ -1,
    TRUE      ~ NA_real_
  )
}

file_list <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab',
  'ns9_2022_main_interview.tab'
)

input_path  <- 'data/input/'
raw_data    <- lapply(file_list, function(fname) {
  read_delim(
    file = file.path(input_path, fname),
    delim = '\t',
    col_types = cols(.default = 'c')
  )
})
names(raw_data) <- file_list

df_clean <- data.frame(
  NSID = raw_data[['wave_one_lsype_young_person_2020.tab']]$NSID,
  stringsAsFactors = FALSE
)

wave2 <- raw_data[['wave_two_lsype_family_background_2020.tab']] %>%
  mutate(
    urbind = as.numeric(urbind),
    gor    = as.numeric(gor)
  )

wave3 <- raw_data[['wave_three_lsype_family_background_2020.tab']] %>%
  mutate(
    urbind = as.numeric(urbind),
    gor    = as.numeric(gor)
  )

wave8 <- raw_data[['ns8_2015_derived.tab']] %>%
  mutate(
    W8DGOR = as.numeric(W8DGOR)
  )

wave9_derived <- raw_data[['ns9_2022_derived_variables.tab']] %>%
  mutate(
    W9DRGN = as.numeric(W9DRGN)
  )

wave9_main <- raw_data[['ns9_2022_main_interview.tab']] %>%
  mutate(
    W9NATIONRES = as.numeric(W9NATIONRES)
  )

df_clean <- df_clean %>%
  left_join(select(wave2, NSID, urbind, gor), by = 'NSID') %>%
  rename(regub15 = urbind, regov15 = gor) %>%
  mutate(
    regub15 = map_urbind_gor(regub15),
    regov15 = map_urbind_gor(regov15)
  ) %>%
  left_join(select(wave3, NSID, urbind, gor), by = 'NSID') %>%
  rename(regub16 = urbind, regov16 = gor) %>%
  mutate(
    regub16 = map_urbind_gor(regub16),
    regov16 = map_urbind_gor(regov16)
  ) %>%
  left_join(select(wave8, NSID, W8DGOR), by = 'NSID') %>%
  rename(regor25_raw = W8DGOR) %>%
  mutate(regor25 = map_gor_w8w9(regor25_raw)) %>%
  left_join(select(wave9_derived, NSID, W9DRGN), by = 'NSID') %>%
  rename(regor32_raw = W9DRGN) %>%
  mutate(regor32 = map_gor_w8w9(regor32_raw)) %>%
  left_join(select(wave9_main, NSID, W9NATIONRES), by = 'NSID') %>%
  rename(regint32_raw = W9NATIONRES) %>%
  mutate(regint32 = map_nationres(regint32_raw)) %>%
  select(NSID, regub15, regov15, regub16, regov16, regor25, regor32, regint32)

df_clean <- df_clean %>%
  mutate(across(-NSID, ~ replace_na(., -3)))

output_path <- 'data/output/'
write_csv(df_clean, file = file.path(output_path, 'cleaned_data.csv'))
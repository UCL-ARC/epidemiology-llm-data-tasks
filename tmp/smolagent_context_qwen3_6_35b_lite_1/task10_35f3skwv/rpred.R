library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(haven)
library(labelled)

# Load all files
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_five_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

data_list <- list()
for (f in files) {
  data_list[[f]] <- read_tsv(file.path('data/input', f), show_col_types = FALSE)
}

# Merge all datasets by NSID using full_join
df <- reduce(data_list, full_join, by = 'NSID')

cat('Dimensions after merge:', nrow(df), 'rows,', ncol(df), 'cols\n')
cat('NSIDs:', length(unique(df$NSID)), '\n')

# --- ecoact17 from W4empsYP (Age 17) ---
df <- df %>%
  mutate(ecoact17 = case_when(
    W4empsYP %in% c(1, 2) ~ 1L,
    W4empsYP == 3 ~ 3L,
    W4empsYP %in% c(4, 5) ~ 4L,
    W4empsYP == 6 ~ 5L,
    W4empsYP == 8 ~ 6L,
    W4empsYP == 7 ~ 6L,
    W4empsYP == 9 ~ 6L,
    W4empsYP == -92 ~ -9L,
    W4empsYP == -94 ~ -8L,
    W4empsYP == -91 ~ -1L,
    W4empsYP == -999 ~ -2L,
    is.na(W4empsYP) ~ -3L,
    TRUE ~ -3L
  ))

# --- ecoact18 from W5mainactYP (Age 18) ---
df <- df %>%
  mutate(ecoact18 = case_when(
    W5mainactYP %in% c(3) ~ 1L,
    W5mainactYP %in% c(1, 2, 4, 5, 6) ~ 4L,
    W5mainactYP == 7 ~ 3L,
    W5mainactYP == 8 ~ 5L,
    W5mainactYP %in% c(9, 10, 11) ~ 6L,
    W5mainactYP == -94 ~ -8L,
    is.na(W5mainactYP) ~ -3L,
    TRUE ~ -3L
  ))

# --- ecoact19 from W6TCurrentAct (Age 19) ---
df <- df %>%
  mutate(ecoact19 = case_when(
    W6TCurrentAct %in% c(3) ~ 1L,
    W6TCurrentAct %in% c(1, 2, 4, 5, 10) ~ 4L,
    W6TCurrentAct == 8 ~ 3L,
    W6TCurrentAct == 7 ~ 5L,
    W6TCurrentAct %in% c(6, 9, 11) ~ 6L,
    W6TCurrentAct == -91 ~ -1L,
    is.na(W6TCurrentAct) ~ -3L,
    TRUE ~ -3L
  ))

# --- ecoact20 from W7TCurrentAct (Age 20) ---
df <- df %>%
  mutate(ecoact20 = case_when(
    W7TCurrentAct %in% c(3) ~ 1L,
    W7TCurrentAct %in% c(2, 4, 5, 9) ~ 4L,
    W7TCurrentAct == 8 ~ 3L,
    W7TCurrentAct == 7 ~ 5L,
    W7TCurrentAct %in% c(1, 6, 10, 11, 12, 13, 14, 15) ~ 6L,
    W7TCurrentAct == -91 ~ -1L,
    is.na(W7TCurrentAct) ~ -3L,
    TRUE ~ -3L
  ))

# --- ecoact25 from W8DACTIVITYC (Age 25) - collapsed 6 categories ---
df <- df %>%
  mutate(ecoact25 = case_when(
    W8DACTIVITYC == 1 ~ 1L,
    W8DACTIVITYC == 2 ~ 2L,
    W8DACTIVITYC == 4 ~ 3L,
    W8DACTIVITYC %in% c(5, 6, 7) ~ 4L,
    W8DACTIVITYC == 9 ~ 5L,
    W8DACTIVITYC %in% c(3, 8, 10) ~ 6L,
    W8DACTIVITYC == -9 ~ -9L,
    W8DACTIVITYC == -8 ~ -8L,
    W8DACTIVITYC == -1 ~ -1L,
    is.na(W8DACTIVITYC) ~ -3L,
    TRUE ~ -3L
  ))

# --- ecoact32 from W9DACTIVITYC (Age 32) - collapsed 6 categories ---
df <- df %>%
  mutate(ecoact32 = case_when(
    W9DACTIVITYC == 1 ~ 1L,
    W9DACTIVITYC == 2 ~ 2L,
    W9DACTIVITYC == 4 ~ 3L,
    W9DACTIVITYC %in% c(5, 6, 7) ~ 4L,
    W9DACTIVITYC == 9 ~ 5L,
    W9DACTIVITYC %in% c(3, 8, 10) ~ 6L,
    W9DACTIVITYC == -9 ~ -9L,
    W9DACTIVITYC == -8 ~ -8L,
    W9DACTIVITYC == -1 ~ -1L,
    is.na(W9DACTIVITYC) ~ -3L,
    TRUE ~ -3L
  ))

# --- ecoactadu25 from W8DACTIVITYC (Age 25) - detailed ---
df <- df %>%
  mutate(ecoactadu25 = case_when(
    W8DACTIVITYC == 1 ~ 1L,
    W8DACTIVITYC == 2 ~ 2L,
    W8DACTIVITYC == 3 ~ 3L,
    W8DACTIVITYC == 4 ~ 4L,
    W8DACTIVITYC == 5 ~ 5L,
    W8DACTIVITYC == 6 ~ 6L,
    W8DACTIVITYC == 7 ~ 7L,
    W8DACTIVITYC == 8 ~ 8L,
    W8DACTIVITYC == 9 ~ 9L,
    W8DACTIVITYC == 10 ~ 10L,
    W8DACTIVITYC == -9 ~ -9L,
    W8DACTIVITYC == -8 ~ -8L,
    W8DACTIVITYC == -1 ~ -1L,
    is.na(W8DACTIVITYC) ~ -3L,
    TRUE ~ -3L
  ))

# --- ecoactadu32 from W9DACTIVITYC (Age 32) - detailed ---
df <- df %>%
  mutate(ecoactadu32 = case_when(
    W9DACTIVITYC == 1 ~ 1L,
    W9DACTIVITYC == 2 ~ 2L,
    W9DACTIVITYC == 3 ~ 3L,
    W9DACTIVITYC == 4 ~ 4L,
    W9DACTIVITYC == 5 ~ 5L,
    W9DACTIVITYC == 6 ~ 6L,
    W9DACTIVITYC == 7 ~ 7L,
    W9DACTIVITYC == 8 ~ 8L,
    W9DACTIVITYC == 9 ~ 9L,
    W9DACTIVITYC == 10 ~ 10L,
    W9DACTIVITYC == -9 ~ -9L,
    W9DACTIVITYC == -8 ~ -8L,
    W9DACTIVITYC == -1 ~ -1L,
    is.na(W9DACTIVITYC) ~ -3L,
    TRUE ~ -3L
  ))

# Select only NSID and derived variables
df_out <- df %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Save to CSV
write_csv(df_out, 'data/output/cleaned_data.csv')

cat('\nOutput saved to data/output/cleaned_data.csv\n')
cat('Dimensions:', nrow(df_out), 'rows,', ncol(df_out), 'cols\n')
cat('\nSummary of derived variables:\n')
print(summary(df_out))
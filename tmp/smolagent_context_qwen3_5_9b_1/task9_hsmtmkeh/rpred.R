library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Apply harmonisation to all qualification variables, keeping original
wave1 <- wave1 %>%
  mutate(
    W1hiqualmum_hm = case_when(
      W1hiqualmum == -999 ~ -2,
      W1hiqualmum == -99 ~ -8,
      W1hiqualmum == -98 ~ -3,
      W1hiqualmum == -94 ~ -8,
      W1hiqualmum == -92 ~ -9,
      W1hiqualmum == -91 ~ -1,
      W1hiqualmum == -1 ~ -8,
      TRUE ~ as.integer(W1hiqualmum)
    ),
    W1hiqualdad_hm = case_when(
      W1hiqualdad == -999 ~ -2,
      W1hiqualdad == -99 ~ -8,
      W1hiqualdad == -98 ~ -3,
      W1hiqualdad == -94 ~ -8,
      W1hiqualdad == -92 ~ -9,
      W1hiqualdad == -91 ~ -1,
      W1hiqualdad == -1 ~ -8,
      TRUE ~ as.integer(W1hiqualdad)
    )
  )

wave2 <- wave2 %>%
  mutate(
    W2hiqualmum_hm = case_when(
      W2hiqualmum == -999 ~ -2,
      W2hiqualmum == -99 ~ -8,
      W2hiqualmum == -98 ~ -3,
      W2hiqualmum == -94 ~ -8,
      W2hiqualmum == -92 ~ -9,
      W2hiqualmum == -91 ~ -1,
      W2hiqualmum == -1 ~ -8,
      TRUE ~ as.integer(W2hiqualmum)
    ),
    W2hiqualdad_hm = case_when(
      W2hiqualdad == -999 ~ -2,
      W2hiqualdad == -99 ~ -8,
      W2hiqualdad == -98 ~ -3,
      W2hiqualdad == -94 ~ -8,
      W2hiqualdad == -92 ~ -9,
      W2hiqualdad == -91 ~ -1,
      W2hiqualdad == -1 ~ -8,
      TRUE ~ as.integer(W2hiqualdad)
    )
  )

wave4 <- wave4 %>%
  mutate(
    w4hiqualmum_hm = case_when(
      w4hiqualmum == -99 ~ -8,
      w4hiqualmum == -98 ~ -3,
      w4hiqualmum == -94 ~ -8,
      TRUE ~ as.integer(w4hiqualmum)
    ),
    w4hiqualdad_hm = case_when(
      w4hiqualdad == -99 ~ -8,
      w4hiqualdad == -98 ~ -3,
      w4hiqualdad == -94 ~ -8,
      TRUE ~ as.integer(w4hiqualdad)
    )
  )

# Merge all datasets by NSID
combined <- full_join(
  wave1,
  wave2,
  by = 'NSID'
) %>%
  full_join(
    wave4,
    by = 'NSID'
  )

# Consolidate detailed education - scan waves 1, 2, 4 in order
# Take first positive value (1-20); if no positive, take first negative; if no value, -3
result <- combined %>%
  mutate(
    educdtlma = case_when(
      !is.na(W1hiqualmum_hm) & W1hiqualmum_hm > 0 & W1hiqualmum_hm <= 20 ~ W1hiqualmum_hm,
      !is.na(W1hiqualmum_hm) & W1hiqualmum_hm < 0 ~ W1hiqualmum_hm,
      is.na(W1hiqualmum_hm) & !is.na(W2hiqualmum_hm) & W2hiqualmum_hm > 0 & W2hiqualmum_hm <= 20 ~ W2hiqualmum_hm,
      is.na(W1hiqualmum_hm) & !is.na(W2hiqualmum_hm) & W2hiqualmum_hm < 0 ~ W2hiqualmum_hm,
      is.na(W1hiqualmum_hm) & is.na(W2hiqualmum_hm) & !is.na(w4hiqualmum_hm) & w4hiqualmum_hm > 0 & w4hiqualmum_hm <= 20 ~ w4hiqualmum_hm,
      is.na(W1hiqualmum_hm) & is.na(W2hiqualmum_hm) & !is.na(w4hiqualmum_hm) & w4hiqualmum_hm < 0 ~ w4hiqualmum_hm,
      TRUE ~ -3
    ),
    educdtlpa = case_when(
      !is.na(W1hiqualdad_hm) & W1hiqualdad_hm > 0 & W1hiqualdad_hm <= 20 ~ W1hiqualdad_hm,
      !is.na(W1hiqualdad_hm) & W1hiqualdad_hm < 0 ~ W1hiqualdad_hm,
      is.na(W1hiqualdad_hm) & !is.na(W2hiqualdad_hm) & W2hiqualdad_hm > 0 & W2hiqualdad_hm <= 20 ~ W2hiqualdad_hm,
      is.na(W1hiqualdad_hm) & !is.na(W2hiqualdad_hm) & W2hiqualdad_hm < 0 ~ W2hiqualdad_hm,
      is.na(W1hiqualdad_hm) & is.na(W2hiqualdad_hm) & !is.na(w4hiqualdad_hm) & w4hiqualdad_hm > 0 & w4hiqualdad_hm <= 20 ~ w4hiqualdad_hm,
      is.na(W1hiqualdad_hm) & is.na(W2hiqualdad_hm) & !is.na(w4hiqualdad_hm) & w4hiqualdad_hm < 0 ~ w4hiqualdad_hm,
      TRUE ~ -3
    )
  ) %>%
  mutate(
    educma = case_when(
      educdtlma == -3 ~ -3,
      educdtlma >= 1 & educdtlma <= 4 ~ 0,
      educdtlma >= 5 & educdtlma <= 7 ~ 1,
      educdtlma == 18 ~ 2,
      educdtlma == 19 ~ 3,
      educdtlma == 20 ~ 4,
      TRUE ~ as.numeric(educdtlma)
    ),
    educpa = case_when(
      educdtlpa == -3 ~ -3,
      educdtlpa >= 1 & educdtlpa <= 4 ~ 0,
      educdtlpa >= 5 & educdtlpa <= 7 ~ 1,
      educdtlpa == 18 ~ 2,
      educdtlpa == 19 ~ 3,
      educdtlpa == 20 ~ 4,
      TRUE ~ as.numeric(educdtlpa)
    )
  ) %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

# Write output
write_csv(result, 'data/output/cleaned_data.csv')

print('Done')
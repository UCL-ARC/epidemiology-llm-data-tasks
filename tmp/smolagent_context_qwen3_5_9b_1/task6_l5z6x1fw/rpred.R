library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')
ns9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Start with full join of all datasets by NSID
combined <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave2, by = 'NSID') %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(ns8, by = 'NSID') %>%
  full_join(ns9, by = 'NSID') %>%
  full_join(ns9_main, by = 'NSID')

# Create regub15 from urbind.x (wave2 urbind, age 15)
combined <- combined %>%
  mutate(
    regub15 = case_when(
      urbind.x %in% c(-999, -998, -997, -995, -1) ~ -1,
      urbind.x == -94 ~ -3,
      TRUE ~ as.numeric(urbind.x)
    )
  )

# Create regub16 from urbind.y (wave3 urbind, age 16)
combined <- combined %>%
  mutate(
    regub16 = case_when(
      urbind.y %in% c(-999, -998, -997, -995, -1) ~ -1,
      urbind.y == -94 ~ -3,
      TRUE ~ as.numeric(urbind.y)
    )
  )

# Create regov15 from gor.x (wave2 gor, age 15)
combined <- combined %>%
  mutate(
    regov15 = case_when(
      gor.x %in% c(-999, -998, -997, -995, -1) ~ -1,
      gor.x == -94 ~ -3,
      TRUE ~ as.numeric(gor.x)
    )
  )

# Create regov16 from gor.y (wave3 gor, age 16)
combined <- combined %>%
  mutate(
    regov16 = case_when(
      gor.y %in% c(-999, -998, -997, -995, -1) ~ -1,
      gor.y == -94 ~ -3,
      TRUE ~ as.numeric(gor.y)
    )
  )

# Create regor25 from W8DGOR (age 25)
# 13 is "Unknown due to faulty/missing postcode" -> -2
combined <- combined %>%
  mutate(
    regor25 = case_when(
      W8DGOR == 13 ~ -2,
      W8DGOR %in% c(-9, -8, -1) ~ NA_real_,
      TRUE ~ as.numeric(W8DGOR)
    )
  )

# Create regor32 from W9DRGN (age 32)
# 13 is "Unknown due to faulty/missing postcode" -> -2
combined <- combined %>%
  mutate(
    regor32 = case_when(
      W9DRGN == 13 ~ -2,
      W9DRGN %in% c(-9, -8, -1) ~ NA_real_,
      TRUE ~ as.numeric(W9DRGN)
    )
  )

# Create regint32 from W9NATIONRES (age 32)
# 1 (England), 2 (Scotland), 3 (Wales), 4 (Northern Ireland) -> 1 (In UK)
# 5 (Outside of UK or unknown) -> 2 (Abroad)
combined <- combined %>%
  mutate(
    regint32 = case_when(
      W9NATIONRES == 5 ~ 2,
      W9NATIONRES %in% c(1, 2, 3, 4) ~ 1,
      W9NATIONRES %in% c(-9, -8, -3, -1) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

# Select final variables: NSID and the 7 derived variables
final_data <- combined %>%
  select(NSID, regub15, regub16, regov15, regov16, regor25, regor32, regint32)

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')
print('Done')
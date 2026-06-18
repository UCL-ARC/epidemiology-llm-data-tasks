library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. Load datasets
file_list <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_self_completion.tab',
  'ns9_2022_main_interview.tab'
)

data_frames <- map(file_list, ~ read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = 'c')))
names(data_frames) <- file_list

full_df <- data_frames[[1]] %>% 
  mutate(NSID = as.character(NSID))

for (i in 2:length(data_frames)) {
  df_next <- data_frames[[i]] %>% mutate(NSID = as.character(NSID))
  full_df <- full_join(full_df, df_next, by = 'NSID')
}

# 2. Harmonisation Logic

# Wave 6 (Age 19): W6SexualityYP
full_df <- full_df %>% 
  mutate(
    W6SexualityYP = as.numeric(W6SexualityYP),
    sori19 = case_when(
      W6SexualityYP == 1 ~ 1,
      W6SexualityYP == 2 ~ 2,
      W6SexualityYP == 3 ~ 3,
      W6SexualityYP == 4 ~ 4,
      W6SexualityYP == -97 ~ -9,
      W6SexualityYP == -92 ~ -9,
      W6SexualityYP == -91 ~ -1,
      W6SexualityYP == -1 ~ -8,
      is.na(W6SexualityYP) ~ -3,
      TRUE ~ -3
    )
  )

# Wave 7 (Age 20): W7SexualityYP
full_df <- full_df %>% 
  mutate(
    W7SexualityYP = as.numeric(W7SexualityYP),
    sori20 = case_when(
      W7SexualityYP == 1 ~ 1,
      W7SexualityYP == 2 ~ 2,
      W7SexualityYP == 3 ~ 3,
      W7SexualityYP == 4 ~ 4,
      W7SexualityYP == -100 ~ -9,
      W7SexualityYP == -97 ~ -9,
      W7SexualityYP == -92 ~ -9,
      W7SexualityYP == -91 ~ -1,
      W7SexualityYP == -1 ~ -8,
      is.na(W7SexualityYP) ~ -3,
      TRUE ~ -3
    )
  )

# Wave 8 (Age 25): W8SEXUALITY
full_df <- full_df %>% 
  mutate(
    W8SEXUALITY = as.numeric(W8SEXUALITY),
    sori25 = case_when(
      W8SEXUALITY == 1 ~ 1,
      W8SEXUALITY == 2 ~ 2,
      W8SEXUALITY == 3 ~ 3,
      W8SEXUALITY == 4 ~ 4,
      W8SEXUALITY == -9 ~ -9,
      W8SEXUALITY == -8 ~ -8,
      W8SEXUALITY == -1 ~ -1,
      is.na(W8SEXUALITY) ~ -3,
      TRUE ~ -3
    )
  )

# Wave 9 (Age 32): W9SORI
full_df <- full_df %>% 
  mutate(
    W9SORI = as.numeric(W9SORI),
    sori32 = case_when(
      W9SORI == 1 ~ 1,
      W9SORI == 2 ~ 2,
      W9SORI == 3 ~ 3,
      W9SORI == 4 ~ 4,
      W9SORI == 5 ~ -7,
      W9SORI == -9 ~ -9,
      W9SORI == -8 ~ -8,
      W9SORI == -3 ~ -3,
      W9SORI == -1 ~ -1,
      is.na(W9SORI) ~ -3,
      TRUE ~ -3
    )
  )

# 3. Final Variables and Output
final_vars <- c('NSID', 'sori19', 'sori20', 'sori25', 'sori32')
output_df <- full_df %>% select(all_of(final_vars))

# Write output
write_csv(output_df, 'data/output/cleaned_data.csv')

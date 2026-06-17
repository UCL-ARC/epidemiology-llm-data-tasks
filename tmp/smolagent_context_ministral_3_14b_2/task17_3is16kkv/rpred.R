library(readr)
library(dplyr)

wave_two <- readr::read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave_three <- readr::read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave_nine <- readr::read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Get the actual column names
last_col_two <- tail(names(wave_two), 1)
last_col_three <- tail(names(wave_three), 1)

wave_two_imd <- wave_two %>%
  select(NSID, all_of(last_col_two)) %>%
  rename(imd15 = last_col_two)

wave_three_imd <- wave_three %>%
  select(NSID, all_of(last_col_three)) %>%
  rename(imd16 = last_col_three)

merged_data <- full_join(wave_two_imd, wave_three_imd, by = 'NSID') %>%
  full_join(wave_nine, by = 'NSID')

merged_data <- merged_data %>%
  mutate(
    imd15 = ifelse(imd15 %in% -999: -1, -3, imd15),
    imd16 = ifelse(imd16 %in% -999: -1, -3, imd16),
    imd32 = ifelse(W9DIMDD == -8, -3, W9DIMDD)
  )

final_data <- merged_data %>%
  select(NSID, imd15, imd16, imd32)

readr::write_csv(final_data, 'data/output/cleaned_data.csv')
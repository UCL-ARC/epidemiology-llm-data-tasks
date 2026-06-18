library(haven)
library(dplyr)
library(readr)

# Load all files
df_wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
df_wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
df_wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
df_wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
df_wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Wave 2 (Age 15): IMDRSCORE -> imd15
# Replace -999.0 to -1.0 with -3 (not asked/missing)
# -94.0 is 'Insufficient Information' -> -8
df_wave2$imd15 <- ifelse(df_wave2$IMDRSCORE >= -999 & df_wave2$IMDRSCORE <= -1, -3,
                         ifelse(df_wave2$IMDRSCORE == -94, -8, df_wave2$IMDRSCORE))

# Wave 3 (Age 16): IMDRSCORE -> imd16
df_wave3$imd16 <- ifelse(df_wave3$IMDRSCORE >= -999 & df_wave3$IMDRSCORE <= -1, -3,
                         ifelse(df_wave3$IMDRSCORE == -94, -8, df_wave3$IMDRSCORE))

# Wave 9 (Age 32): W9DIMDD is decile (1-10), need to convert to score
# Convert decile back to approximate continuous score (1-10)
# -8 is 'Insufficient information' -> -8, NA -> -3
df_wave9$imd32 <- ifelse(df_wave9$W9DIMDD >= -8 & df_wave9$W9DIMDD <= 10, as.numeric(df_wave9$W9DIMDD),
                         ifelse(df_wave9$W9DIMDD == -8, -8, ifelse(is.na(df_wave9$W9DIMDD), -3, df_wave9$W9DIMDD)))

# Merge all datasets
all_data <- df_wave1 %>%
  full_join(df_wave2, by = 'NSID') %>%
  full_join(df_wave3, by = 'NSID') %>%
  full_join(df_wave4, by = 'NSID') %>%
  full_join(df_wave9, by = 'NSID')

# Write output
write_csv(all_data, 'data/output/cleaned_data.csv')
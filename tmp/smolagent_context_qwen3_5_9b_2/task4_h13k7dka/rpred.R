library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Recode missing values according to requirements
# sori19 from W6SexualityYP: map -97, -100 -> -9; -92 -> -9; -91 -> -1; -1 -> -8
w6 <- wave6$W6SexualityYP
w6[w6 == -97] <- -9
w6[w6 == -100] <- -9
w6[w6 == -92] <- -9
w6[w6 == -91] <- -1
w6[w6 == -1] <- -8

# sori20 from W7SexualityYP: map -100 -> -9; -97 -> -9; -92 -> -9; -91 -> -1; -1 -> -8
w7 <- wave7$W7SexualityYP
w7[w7 == -100] <- -9
w7[w7 == -97] <- -9
w7[w7 == -92] <- -9
w7[w7 == -91] <- -1
w7[w7 == -1] <- -8

# sori25 from W8SEXUALITY: -9, -8, -1 already correct (use as-is)
w8 <- ns8$W8SEXUALITY

# sori32 from W9SORI: map -97, -100 -> -9; 5 -> -7; -9 -> -9; -8 -> -8; -3 -> -3; -1 -> -1
w9 <- ns9$W9SORI
w9[w9 == -97] <- -9
w9[w9 == -100] <- -9
w9[w9 == 5] <- -7

# Merge all datasets
combined <- full_join(wave1, wave4, by = 'NSID') %>%
  full_join(wave6, by = 'NSID') %>%
  full_join(wave7, by = 'NSID') %>%
  full_join(ns8, by = 'NSID') %>%
  full_join(ns9, by = 'NSID')

# Add sori variables to combined dataset
combined$sori19 <- w6[match(combined$NSID, wave6$NSID)]
combined$sori20 <- w7[match(combined$NSID, wave7$NSID)]
combined$sori25 <- w8[match(combined$NSID, ns8$NSID)]
combined$sori32 <- w9[match(combined$NSID, ns9$NSID)]

# Convert to factors with harmonised labels
# Common labels for sori19, sori20, sori25
labels_common <- c(
  Refused = -9,
  `Don't know` = -8,
  `Not applicable` = -1,
  `Heterosexual / Straight` = 1,
  `Gay / Lesbian` = 2,
  `Bisexual` = 3,
  `Other` = 4
)

combined$sori19 <- factor(combined$sori19,
  levels = c(-9, -8, -1, 1, 2, 3, 4),
  labels = c('Refused', "Don't know", 'Not applicable', 
             'Heterosexual / Straight', 'Gay / Lesbian', 'Bisexual', 'Other'))

combined$sori20 <- factor(combined$sori20,
  levels = c(-9, -8, -1, 1, 2, 3, 4),
  labels = c('Refused', "Don't know", 'Not applicable', 
             'Heterosexual / Straight', 'Gay / Lesbian', 'Bisexual', 'Other'))

combined$sori25 <- factor(combined$sori25,
  levels = c(-9, -8, -1, 1, 2, 3, 4),
  labels = c('Refused', "Don't know", 'Not applicable', 
             'Heterosexual / Straight', 'Gay / Lesbian', 'Bisexual', 'Other'))

# sori32 has additional -7 level
combined$sori32 <- factor(combined$sori32,
  levels = c(-9, -8, -7, -1, 1, 2, 3, 4),
  labels = c('Refused', "Don't know", 'Prefer not to say', 
             'Not applicable', 'Heterosexual / Straight', 'Gay / Lesbian', 'Bisexual', 'Other'))

# Write output
write_csv(combined, 'data/output/cleaned_data.csv')

print('Script completed successfully')
print(paste('Total observations:', nrow(combined)))
print(paste('Variables:', ncol(combined)))
print('sori19 levels:', paste(levels(combined$sori19), collapse=', '))
print('sori32 levels:', paste(levels(combined$sori32), collapse=', '))
}
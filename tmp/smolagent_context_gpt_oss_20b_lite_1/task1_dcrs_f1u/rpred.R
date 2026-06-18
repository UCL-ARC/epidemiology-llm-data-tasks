library(readr)
library(dplyr)

# Helper to read tab
read_tab <- function(path){
  read_delim(path, delim = '\t', col_types = cols(), na = c('','NA'))
}

# File paths
files <- list(
  wave_one = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_two = 'data/input/wave_two_lsype_young_person_2020.tab',
  wave_three = 'data/input/wave_three_lsype_young_person_2020.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  wave_five = 'data/input/wave_five_lsype_young_person_2020.tab',
  wave_six = 'data/input/wave_six_lsype_young_person_2020.tab',
  wave_seven = 'data/input/wave_seven_lsype_young_person_2020.tab',
  wave_eight = 'data/input/ns8_2015_main_interview.tab',
  wave_nine = 'data/input/ns9_2022_main_interview.tab'
)

# Read all waves
wave1 <- read_tab(files$wave_one)
wave2 <- read_tab(files$wave_two)
wave3 <- read_tab(files$wave_three)
wave4 <- read_tab(files$wave_four)
wave5 <- read_tab(files$wave_five)
wave6 <- read_tab(files$wave_six)
wave7 <- read_tab(files$wave_seven)
wave8 <- read_tab(files$wave_eight)
wave9 <- read_tab(files$wave_nine)

# Recode sex
wave1 <- wave1 %>% mutate(sex_wave1 = ifelse(W1sexYP %in% c(1,2), W1sexYP, NA_real_)) %>% select(NSID, sex_wave1)
wave2 <- wave2 %>% mutate(sex_wave2 = ifelse(W2SexYP %in% c(1,2), W2SexYP, NA_real_)) %>% select(NSID, sex_wave2)
wave3 <- wave3 %>% mutate(sex_wave3 = ifelse(W3sexYP %in% c(1,2), W3sexYP, NA_real_)) %>% select(NSID, sex_wave3)
wave4 <- wave4 %>% mutate(sex_wave4 = ifelse(W4SexYP %in% c(1,2), W4SexYP, NA_real_)) %>% select(NSID, sex_wave4)
wave5 <- wave5 %>% mutate(sex_wave5 = ifelse(W5SexYP %in% c(1,2), W5SexYP, NA_real_)) %>% select(NSID, sex_wave5)
wave6 <- wave6 %>% mutate(sex_wave6 = ifelse(W6Sex %in% c(1,2), W6Sex, NA_real_)) %>% select(NSID, sex_wave6)
wave7 <- wave7 %>% mutate(sex_wave7 = ifelse(W7Sex %in% c(1,2), W7Sex, NA_real_)) %>% select(NSID, sex_wave7)
wave8 <- wave8 %>% mutate(sex_wave8 = ifelse(W8CMSEX %in% c(1,2), W8CMSEX, NA_real_)) %>% select(NSID, sex_wave8)
wave9 <- wave9 %>% mutate(sex_wave9 = ifelse(W9DSEX %in% c(1,2), W9DSEX, NA_real_)) %>% select(NSID, sex_wave9)

# Merge all waves
merged <- full_join(wave1, wave2, by = 'NSID') %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave5, by = 'NSID') %>%
  full_join(wave6, by = 'NSID') %>%
  full_join(wave7, by = 'NSID') %>%
  full_join(wave8, by = 'NSID') %>%
  full_join(wave9, by = 'NSID')

# Consolidated sex: earliest valid
merged <- merged %>% mutate(sex = coalesce(sex_wave1, sex_wave2, sex_wave3, sex_wave4, sex_wave5, sex_wave6, sex_wave7, sex_wave8, sex_wave9))

# Map missing to -3 (not asked)
merged <- merged %>% mutate(sex = ifelse(is.na(sex), -3, sex))

# Select final columns
final_df <- merged %>% select(NSID, sex)

# Write CSV
write_csv(final_df, 'data/output/cleaned_data.csv')

cat('Cleaning complete. Output written to data/output/cleaned_data.csv\n')

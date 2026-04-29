library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

dir.create("data/output", recursive = TRUE, showWarnings = FALSE)

df1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
df2 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
df8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
df9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

merged <- df1 %>% full_join(df2, by = 'NSID') %>% full_join(df8, by = 'NSID') %>% full_join(df9, by = 'NSID')

cleaned <- merged %>% select(NSID, W8DINCB, W9DINCB) %>% rename(inc25 = W8DINCB, inc32 = W9DINCB)

cleaned$inc25[is.na(cleaned$inc25)] <- -3
cleaned$inc32[is.na(cleaned$inc32)] <- -3

labels_inc25 <- c(
  'Not asked' = -3,
  'Script error/lost' = -2,
  'Not applicable' = -1,
  'Refusal' = -9,
  'Don\'t know' = -8,
  'less than 25' = 1,
  '25 to 50' = 2,
  '50 to 90' = 3,
  '90 to 140' = 4,
  '140 to 240' = 5,
  '240 to 300' = 6,
  '300 to 350' = 7,
  '350 to 400' = 8,
  '400 to 500' = 9,
  '500 to 600' = 10,
  '600 to 700' = 11,
  '700 to 800' = 12,
  '800 to 900' = 13,
  '900 to 1200' = 14,
  '1200 to 1400' = 15,
  'more than 1400' = 16
)

cleaned$inc25 <- labelled(cleaned$inc25, labels = labels_inc25)
cleaned$inc32 <- labelled(cleaned$inc32, labels = labels_inc25)

write_csv(cleaned, 'data/output/cleaned_data.csv')
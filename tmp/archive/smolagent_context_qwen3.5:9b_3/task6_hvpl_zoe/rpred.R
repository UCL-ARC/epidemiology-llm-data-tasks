library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

if(!dir.exists('data/output')) dir.create('data/output', recursive=TRUE)

wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim='\t', col_types=cols(.default='c'))
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim='\t', col_types=cols(.default='c'))
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim='\t', col_types=cols(.default='c'))
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim='\t', col_types=cols(.default='c'))
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim='\t', col_types=cols(.default='c'))
ns9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim='\t', col_types=cols(.default='c'))
ns9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim='\t', col_types=cols(.default='c'))

final_data <- wave1
final_data <- full_join(final_data, wave2, by='NSID')
final_data <- full_join(final_data, wave3, by='NSID')
final_data <- full_join(final_data, wave4, by='NSID')
final_data <- full_join(final_data, ns8, by='NSID')
final_data <- full_join(final_data, ns9_derived, by='NSID')
final_data <- full_join(final_data, ns9_main, by='NSID')

final_data <- final_data %>%
  mutate(
    urbind_std = as.numeric(ifelse(is.na(urbind.x) | urbind.x %in% c(-94, -999, -1), -3, urbind.x)),
    urbind_harmonized = factor(ifelse(urbind_std %in% c(1:8), urbind_std, -3),
      levels = c(1:8, -3),
      labels = c('Urban >= 10k - sparse', 'Town & Fringe - sparse', 'Village - sparse',
                 'Hamlet and Isolated Dwelling - sparse', 'Urban >= 10k - less sparse',
                 'Town & Fringe - less sparse', 'Village - less sparse',
                 'Hamlet & Isolated Dwelling', 'Not asked/missing'))
  )

final_data <- final_data %>%
  mutate(
    gor_std = as.numeric(ifelse(is.na(gor.x) | gor.x %in% c(-94, -999, -1), -3, gor.x)),
    gor_harmonized = factor(ifelse(gor_std %in% c(1:9), gor_std, -3),
      levels = c(1:9, -3),
      labels = c('North East', 'North West', 'Yorkshire and The Humber',
                 'East Midlands', 'West Midlands', 'East of England',
                 'London', 'South East', 'South West', 'Not asked/missing'))
  )

final_data <- final_data %>%
  mutate(
    W8DGOR_std = ifelse(W8DGOR %in% c(1:13), W8DGOR, ifelse(W8DGOR %in% c(-9, -8, -1), -3, -3)),
    W8DGOR_harmonized = factor(ifelse(W8DGOR_std %in% c(1:13), W8DGOR_std, -3),
      levels = c(1:13, -3),
      labels = c('North East', 'North West', 'Yorkshire and The Humber',
                 'East Midlands', 'West Midlands', 'East of England',
                 'London', 'South East', 'South West', 'Wales', 'Scotland',
                 'Northern Ireland', 'Unknown due to faulty/missing postcode', 'Not asked/missing'))
  )

final_data <- final_data %>%
  mutate(
    W9DRGN_std = ifelse(W9DRGN %in% c(1:13), W9DRGN, ifelse(W9DRGN %in% c(-9, -8, -1), -3, -3)),
    W9DRGN_harmonized = factor(ifelse(W9DRGN_std %in% c(1:13), W9DRGN_std, -3),
      levels = c(1:13, -3),
      labels = c('North East', 'North West', 'Yorkshire and The Humber',
                 'East Midlands', 'West Midlands', 'East of England',
                 'London', 'South East', 'South West', 'Wales', 'Scotland',
                 'Northern Ireland', 'Unknown due to faulty/missing postcode', 'Not asked/missing'))
  )

final_data <- final_data %>%
  mutate(
    W9NATIONRES_std = ifelse(W9NATIONRES %in% c(1:5), W9NATIONRES, ifelse(W9NATIONRES %in% c(-9, -8, -3, -1), -3, -3)),
    W9NATIONRES_harmonized = factor(ifelse(W9NATIONRES_std %in% c(1:5), W9NATIONRES_std, -3),
      levels = c(1:5, -3),
      labels = c('England', 'Scotland', 'Wales', 'Northern Ireland',
                 'Outside of UK or unknown', 'Not asked/missing'))
  )

final_data <- final_data %>%
  mutate(urban_binary = case_when(
    urbind_harmonized == 1 ~ 'Urban',
    urbind_harmonized == 2 ~ 'Town',
    urbind_harmonized == 3 ~ 'Village',
    urbind_harmonized == 4 ~ 'Hamlet',
    urbind_harmonized == 5 ~ 'Urban',
    urbind_harmonized == 6 ~ 'Town',
    urbind_harmonized == 7 ~ 'Village',
    urbind_harmonized == 8 ~ 'Hamlet',
    urbind_harmonized == -3 ~ 'Missing'
  ))

cleaned_data <- final_data
write_csv(cleaned_data, 'data/output/cleaned_data.csv')
print('Data cleaning complete!')
print(paste('Rows:', nrow(cleaned_data)))
print(paste('Columns:', ncol(cleaned_data)))
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9a <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')
ns9b <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

print('wave2 urbind values:')
print(table(wave2$urbind))

print('wave3 urbind values:')
print(table(wave3$urbind))

print('wave2 gor values:')
print(table(wave2$gor))

print('wave3 gor values:')
print(table(wave3$gor))

# Merge all datasets by NSID, handling duplicate column names
merged <- wave1 %>%
  full_join(wave2, by = 'NSID') %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(ns8, by = 'NSID') %>%
  full_join(ns9a, by = 'NSID') %>%
  full_join(ns9b, by = 'NSID')

print('Columns after merge:')
print(head(names(merged), 1100))

# Check if the columns are available
print('urbind.x exists:', 'urbind.x' %in% names(merged))
print('urbind.y exists:', 'urbind.y' %in% names(merged))
print('gor.x exists:', 'gor.x' %in% names(merged))
print('gor.y exists:', 'gor.y' %in% names(merged))

# Create the required variables
# regub15 and regub16 from urbind (age 15 and 16 respectively)
# regov15 and regov16 from gor (age 15 and 16 respectively)

# urbind: 1=Urban >= 10k - sparse, 2=Town & Fringe - sparse, 3=Village - sparse,
#         4=Hamlet and Isolated Dwelling - sparse, 5=Urban >= 10k - less sparse,
#         6=Town & Fringe - less sparse, 7=Village - less sparse, 8=Hamlet & Isolated Dwelling
# Missing: -94=Insufficient information (-3), user_missing: -999 thru -1

# gor: 1=North East, 2=North West, 3=Yorkshire and The Humber, 4=East Midlands,
#      5=West Midlands, 6=East of England, 7=London, 8=South East, 9=South West
#      Missing: -94=Insufficient information (-3), user_missing: -999 thru -1

# Create regub15 (from wave2 urbind.x)
merged <- merged %>% mutate(
  regub15 = case_when(
    is.na(urbind.x) | urbind.x <= -1 | urbind.x == -94 | urbind.x == -999 ~ -3,
    TRUE ~ as.numeric(urbind.x)
  )
)

# Create regub16 (from wave3 urbind.y)
merged <- merged %>% mutate(
  regub16 = case_when(
    is.na(urbind.y) | urbind.y <= -1 | urbind.y == -94 | urbind.y == -999 ~ -3,
    TRUE ~ as.numeric(urbind.y)
  )
)

# Create regov15 (from wave2 gor.x)
merged <- merged %>% mutate(
  regov15 = case_when(
    is.na(gor.x) | gor.x <= -1 | gor.x == -94 | gor.x == -999 ~ -3,
    TRUE ~ as.numeric(gor.x)
  )
)

# Create regov16 (from wave3 gor.y)
merged <- merged %>% mutate(
  regov16 = case_when(
    is.na(gor.y) | gor.y <= -1 | gor.y == -94 | gor.y == -999 ~ -3,
    TRUE ~ as.numeric(gor.y)
  )
)

print('regub15 summary:')
print(summary(merged$regub15))

print('regub16 summary:')
print(summary(merged$regub16))

print('regov15 summary:')
print(summary(merged$regov15))

print('regov16 summary:')
print(summary(merged$regov16))

# Write output
write_csv(merged, 'data/output/cleaned_data.csv')
print('CSV written successfully')
}
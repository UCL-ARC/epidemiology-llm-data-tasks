library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(haven)
library(labelled)

# Load each file explicitly by name
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', show_col_types = FALSE)

# Rename IMDRSCORE in wave2 and wave3 to avoid conflicts during merge
wave2 <- wave2 %>% rename(IMDRSCORE_15 = IMDRSCORE)
wave3 <- wave3 %>% rename(IMDRSCORE_16 = IMDRSCORE)

# Merge all datasets
merged <- wave1 %>%
  full_join(wave2, by = 'NSID') %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave9, by = 'NSID')

print(paste('Merged dimensions:', nrow(merged), 'rows,', ncol(merged), 'cols'))
print('Column names containing IMD:')
print(grep('IMD', names(merged), value = TRUE))

# Create imd15 from wave2 IMDRSCORE_15
# -94 means 'Insufficient Information' -> NA for continuous
imd15 <- merged$IMDRSCORE_15
imd15[imd15 == -94] <- NA
imd15[imd15 < 0] <- NA

# Create imd16 from wave3 IMDRSCORE_16
imd16 <- merged$IMDRSCORE_16
imd16[imd16 == -94] <- NA
imd16[imd16 < 0] <- NA

# Create imd32 from wave9 W9DIMDD
# -8 means 'Insufficient information' -> NA for continuous
imd32 <- merged$W9DIMDD
imd32[imd32 < 0] <- NA

# Create final output dataframe with only NSID and the three IMD variables
output <- merged %>%
  select(NSID) %>%
  mutate(
    imd15 = imd15,
    imd16 = imd16,
    imd32 = imd32
  )

print('Output dimensions:')
print(dim(output))
print('Output columns:')
print(names(output))
print('Sample of output:')
print(head(output, 10))

# Summary of imd variables
print('Summary of imd15:')
print(summary(output$imd15))
print('Summary of imd16:')
print(summary(output$imd16))
print('Summary of imd32:')
print(summary(output$imd32))

# Write output
dir.create('data/output', showWarnings = FALSE, recursive = TRUE)
write_csv(output, 'data/output/cleaned_data.csv')
print('Done: cleaned_data.csv written successfully')

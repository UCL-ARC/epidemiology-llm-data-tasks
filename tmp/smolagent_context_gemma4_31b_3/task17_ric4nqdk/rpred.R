library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
w2_fam <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
w3_fam <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
w4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
w9_der <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = cols(.default = 'c'))

# Merge datasets carefully
# We merge w1 as the base, then join others
cohort <- w1 %>%
  full_join(w2_fam, by = 'NSID') %>%
  full_join(w3_fam, by = 'NSID') %>%
  full_join(w4, by = 'NSID') %>%
  full_join(w9_der, by = 'NSID')

# Note: When joining w2_fam and w3_fam, both have IMDRSCORE. 
# dplyr's full_join will create IMDRSCORE.x and IMDRSCORE.y if not specified.
# To avoid confusion and handle them correctly, let's join and rename specifically.

# Reset and join again with specific suffixes
cohort <- w1 %>%
  full_join(w2_fam, by = 'NSID') %>%
  full_join(w3_fam, by = 'NSID')

# Let's check the names if we just did full_join
# If w2_fam and w3_fam both have IMDRSCORE, they become IMDRSCORE.x and IMDRSCORE.y
# Let's just load the specific columns we need to be safe.

# Redoing the merge process with explicit column selection to avoid name collisions
cohort <- w1 %>%
  select(NSID) %>%
  full_join(w2_fam %>% select(NSID, IMDRSCORE), by = 'NSID') %>%
  full_join(w3_fam %>% select(NSID, IMDRSCORE), by = 'NSID', suffix = c('_15', '_16')) %>%
  full_join(w4 %>% select(NSID), by = 'NSID') %>%
  full_join(w9_der %>% select(NSID, W9DIMDD), by = 'NSID')

# Now the columns are IMDRSCORE_15 and IMDRSCORE_16

clean_imd <- function(x) {
  x <- as.numeric(x)
  res <- ifelse(is.na(x), -3, x)
  res <- ifelse(x == -94, -8, res)
  res <- ifelse(x < -1 & x != -94, -2, res)
  return(res)
}

clean_imd_w9 <- function(x) {
  x <- as.numeric(x)
  res <- ifelse(is.na(x), -3, x)
  res <- ifelse(x == -8, -8, res)
  return(res)
}

final_data <- cohort %>%
  mutate(
    imd15 = clean_imd(IMDRSCORE_15),
    imd16 = clean_imd(IMDRSCORE_16),
    imd32 = clean_imd_w9(W9DIMDD)
  ) %>%
  select(NSID, imd15, imd16, imd32)

write_csv(final_data, 'data/output/cleaned_data.csv')

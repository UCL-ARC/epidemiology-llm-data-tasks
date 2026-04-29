library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Load all datasets
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave17 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave19 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave20 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave25 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t', show_col_types = FALSE)
wave32 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)

# Convert NSID to character
wave1$NSID <- as.character(wave1$NSID)
wave17$NSID <- as.character(wave17$NSID)
wave19$NSID <- as.character(wave19$NSID)
wave20$NSID <- as.character(wave20$NSID)
wave25$NSID <- as.character(wave25$NSID)
wave32$NSID <- as.character(wave32$NSID)

# Merge all datasets
cohort <- wave1 %>%
  full_join(wave17, by = 'NSID') %>%
  full_join(wave19, by = 'NSID') %>%
  full_join(wave20, by = 'NSID') %>%
  full_join(wave25, by = 'NSID') %>%
  full_join(wave32, by = 'NSID')

# AGE 17: w4saim
wave17 <- wave17 %>%
  mutate(
    educaim17 = case_when(
      w4saim == 14 | is.na(w4saim) | w4saim < 0 ~ 5,
      w4saim %in% c(1:13) ~ 1
    )
  ) %>% select(NSID, educaim17)

# AGE 19: W6Saim
wave19 <- wave19 %>%
  mutate(
    educaim19 = case_when(
      W6Saim == 16 | is.na(W6Saim) | W6Saim < 0 ~ 5,
      W6Saim %in% c(1:15) ~ 1
    )
  ) %>% select(NSID, educaim19)

# AGE 20: W7SAim
wave20 <- wave20 %>%
  mutate(
    educaim20 = case_when(
      W7SAim %in% c(-94, -91, -999, -998, -997, -996, -995) ~ -8,
      W7SAim == -91 ~ -1,
      W7SAim %in% c(10, 11, 12, 13, 14) ~ 0,
      W7SAim %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ 1,
      TRUE ~ NA_real_
    )
  ) %>% select(NSID, educaim20)

# AGE 25: W8ACTIVITY05 + qualification indicators
wave25_clean <- wave25 %>%
  select(NSID, W8ACTIVITY05, W8ACQUC0A, W8ACQUC0B, W8ACQUC0C, W8ACQUC0D, W8ACQUC0E, W8ACQUC0F, W8ACQUC0G, W8ACQUC0I, W8ACQUC0J, W8ACQUC0K, W8ACQUC0L, W8ACQUC0M, W8ACQUC0N, W8ACQUC0O, W8ACQUC0P, W8ACQUC0Q, W8VCQUC0J, W8VCQUC0C, W8VCQUC0D, W8VCQUC0E)

wave25_clean <- wave25_clean %>%
  mutate(
    educaim25 = case_when(
      W8ACTIVITY05 != 1 & !is.na(W8ACTIVITY05) ~ 5,
      W8ACQUC0A == 1 ~ 0, W8ACQUC0B == 1 ~ 0, W8ACQUC0C == 1 ~ 0, W8ACQUC0E == 1 ~ 0, W8ACQUC0D == 1 ~ 0, W8ACQUC0K == 1 ~ 0,
      W8ACQUC0F == 1 ~ 1, W8ACQUC0G == 1 ~ 1, W8ACQUC0I == 1 ~ 1, W8ACQUC0J == 1 ~ 1, W8ACQUC0L == 1 ~ 1, W8ACQUC0M == 1 ~ 1, W8VCQUC0J == 1 ~ 1,
      W8VCQUC0C == 1 ~ 2, W8VCQUC0D == 1 ~ 2, W8VCQUC0E == 1 ~ 2,
      W8ACQUC0N == 1 ~ 3, W8ACQUC0O == 1 ~ 3, W8ACQUC0P == 1 ~ 3,
      W8ACQUC0Q == 1 ~ -8,
      TRUE ~ 5
    )
  ) %>% select(NSID, educaim25)

# AGE 32: Complex two-stage logic
# Get actual variable names from the data
cat('Variables in wave32 starting with W9ACQU or W9ACQUC or W9VCQU or W9VCQUC:\n')
relevant_vars <- c(grep('^W9ACQU', names(wave32), value=TRUE), grep('^W9ACQUC', names(wave32), value=TRUE))
cat('W9ACQU/ACQUC vars:', paste(relevant_vars, collapse=', '), '\n')

cat('Variables in wave32 starting with W9VCQU or W9VCQUC:\n')
relevant_vars2 <- c(grep('^W9VCQU', names(wave32), value=TRUE), grep('^W9VCQUC', names(wave32), value=TRUE))
cat('W9VCQU/VCQUC vars:', paste(relevant_vars2, collapse=', '), '\n')

# Select only NSID, W9ECONACT2, and the qualification variables
wave32_clean <- wave32 %>%
  select(NSID, W9ECONACT2, all_of(relevant_vars), all_of(relevant_vars2))

cat('wave32_clean columns:', paste(names(wave32_clean), collapse=', '), '\n')

in_edu <- wave32_clean %>%
  mutate(
    educaim32 = case_when(
      W9ECONACT2 == 6 & !is.na(W9ECONACT2) ~ NA_real_,
      W9ECONACT2 == 7 & !is.na(W9ECONACT2) ~ NA_real_,
      W9ECONACT2 == 12 & !is.na(W9ECONACT2) ~ NA_real_,
      W9ECONACT2 %in% c(1:5, 8:14) & !is.na(W9ECONACT2) ~ 5,
      TRUE ~ 5
    )
  )

studying <- in_edu %>% filter(is.na(educaim32))
cat('Studying:', nrow(studying), '\n')

if (nrow(studying) > 0) {
  studying <- studying %>% mutate(
    educaim32 = case_when(
      # Higher level (0): Doctorate, Masters, Undergrad, Diplomas in HE, Teaching, Nursing, HNC/HND, BTEC National
      W9ACQUC0A == 1 ~ 0,  # Doctorate
      W9ACQUC0B == 1 ~ 0,  # Masters
      W9ACQUC0C == 1 ~ 0,  # Undergraduate
      W9ACQUC0E == 1 ~ 0,  # Diplomas in HE
      W9ACQUC0D == 1 ~ 0,  # Post-grad diplomas
      W9ACQUC0F == 1 ~ 0,  # Teaching quals
      W9VCQUC0V == 1 ~ 0,  # Professional quals at degree level
      W9VCQUC0W == 1 ~ 0,  # BTEC National
      W9VCQUCAC == 1 ~ 0,  # HNC/HND
      # Lower level (1): A/AS, GCSEs, NVQ 1-3, etc.
      W9ACQUC0G == 1 ~ 1,  # A/AS Levels
      W9ACQUC0H == 1 ~ 1,  # Grade A-C Level 4-9
      W9ACQUC0I == 1 ~ 1,  # Grade D-G Level 1-3
      W9ACQUC0J == 1 ~ 1,  # SCE Higher
      W9ACQUC0K == 1 ~ 1,  # Scottish Certificate Sixth Year
      W9ACQUC0L == 1 ~ 1,  # SCE Standard
      W9ACQUC0M == 1 ~ 1,  # National 4/5
      W9ACQUC0N == 1 ~ 1,  # National 2/3
      W9ACQUC0O == 1 ~ 1,  # Leaving Certificate
      W9ACQUC0P == 1 ~ 1,  # Junior Cert A-C
      W9ACQUC0Q == 1 ~ 1,  # Junior Cert D and below
      W9VCQUC0D == 1 ~ 1,  # NVQ 3
      W9VCQUC0E == 1 ~ 1,  # NVQ 2
      W9VCQUC0F == 1 ~ 1,  # NVQ 1
      W9VCQUC0G == 1 ~ 1,  # GNVQ Advanced
      W9VCQUC0H == 1 ~ 1,  # GNVQ Intermediate
      W9VCQUC0I == 1 ~ 1,  # Level 3
      W9VCQUC0J == 1 ~ 1,  # Level 2
      W9VCQUC0K == 1 ~ 1,  # Level 1
      W9VCQUC0R == 1 ~ 1,  # Advanced Diploma
      W9VCQUC0S == 1 ~ 1,  # Higher Diploma
      W9VCQUC0T == 1 ~ 1,  # RSA Diploma
      W9VCQUC0U == 1 ~ 1,  # RSA Stage I-III
      W9VCQUC0X == 1 ~ 1,  # BTEC First
      W9VCQUC0Y == 1 ~ 1,  # SCOTVEC National
      W9VCQUC0Z == 1 ~ 1,  # SCOTVEC first/general
      W9VCQUCAC == 1 ~ 1,  # HNC/HND (already handled above)
      W9VCQUCAB == 1 ~ 1,  # SCOTVEC modules
      # Entry level (2)
      W9VCQUC0C == 1 ~ 2,  # Level 3 (entry)
      W9VCQUC0D == 1 ~ 2,  # Level 2 (entry)
      # Other (3)
      W9ACQUC0R == 1 ~ 3,  # Other academic
      W9ACQUC0S == 1 ~ 3,  # None
      W9ACQUC0T == 1 ~ 3,  # Don't know
      W9ACQUC0U == 1 ~ 3,  # Refused
      W9ACQUC0V == 1 ~ 3,  # No answer
      W9VCQUCAG == 1 ~ 3,  # None of these (vocational)
      W9VCQUCAH == 1 ~ 3,  # Don't know (vocational)
      W9VCQUCAI == 1 ~ 3,  # Refused (vocational)
      TRUE ~ 5
    )
  )
}

cat('Wave 32 processed.\n')

cleaned <- cohort %>%
  left_join(wave17, by = 'NSID') %>%
  left_join(wave19, by = 'NSID') %>%
  left_join(wave20, by = 'NSID') %>%
  left_join(wave25_clean, by = 'NSID') %>%
  left_join(studying, by = 'NSID') %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

write_csv(cleaned, 'data/output/cleaned_data.csv')

cat('Output written to data/output/cleaned_data.csv\n')
cat('Rows:', nrow(cleaned), '\n')
cat('Variables:', paste(names(cleaned), collapse=', '), '\n')

# Check for NA values
for (v in c('educaim17', 'educaim19', 'educaim20', 'educaim25', 'educaim32')) {
  cat(v, ':', sum(is.na(cleaned[[v]])), 'NA values\n')
}

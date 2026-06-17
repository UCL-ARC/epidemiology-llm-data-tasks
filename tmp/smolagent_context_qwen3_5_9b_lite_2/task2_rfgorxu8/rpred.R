library(haven)
library(dplyr)
library(readr)
library(purrr)
library(labelled)

if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

MISS_REFUSE <- -9
MISS_DK <- -8
MISS_NA <- -1
MISS_NT <- -3
MISS_SA <- -2

wave1_recode <- function(x) {
  x <- as.integer(x)
  x[x == -92] <- MISS_REFUSE
  x[x == -91] <- MISS_NA
  x[x == -1] <- MISS_DK
  x[x == -94] <- MISS_DK
  x[x == -999] <- MISS_SA
  x
}

wave2_recode <- function(x) {
  x <- as.integer(x)
  x[x == -92] <- MISS_REFUSE
  x[x == -91] <- MISS_NA
  x[x == -1] <- MISS_DK
  x[x == -99] <- MISS_NT
  x[x == -998] <- MISS_SA
  x[x == -997] <- MISS_SA
  x[x == -995] <- MISS_SA
  x
}

wave4_recode <- function(x) {
  x <- as.integer(x)
  x[x == -94] <- MISS_DK
  x[x == -1] <- MISS_DK
  x
}

wave8_recode <- function(x) {
  x <- as.integer(x)
  x[x == -9] <- MISS_REFUSE
  x[x == -8] <- MISS_DK
  x[x == -1] <- MISS_NA
  x
}

wave9_recode <- function(x) {
  x <- as.integer(x)
  x[x == -8] <- MISS_DK
  x
}

wave1 <- wave1 %>% mutate(W1ethnic2YP_std = wave1_recode(W1ethnic2YP))
wave2 <- wave2 %>% mutate(W2ethnicYP_std = wave2_recode(W2ethnicYP))
wave4 <- wave4 %>% mutate(w4ethnic2YP_std = wave4_recode(w4ethnic2YP))
ns8 <- ns8 %>% mutate(W8DETHN15_std = wave8_recode(W8DETHN15))
ns9 <- ns9 %>% mutate(W9DETHN15_std = wave9_recode(W9DETHN15))

cleaned <- wave1 %>%
  full_join(wave2, by = 'NSID') %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(ns8, by = 'NSID') %>%
  full_join(ns9, by = 'NSID')

cleaned <- cleaned %>%
  mutate(
    W1_num = as.integer(W1ethnic2YP_std),
    W2_num = as.integer(W2ethnicYP_std),
    W4_num = as.integer(w4ethnic2YP_std),
    W8_num = as.integer(W8DETHN15_std),
    W9_num = as.integer(W9DETHN15_std)
  ) %>%
  mutate(
    eth = case_when(
      W1_num >= 1 & W1_num <= 16 ~ W1_num,
      W2_num >= 1 & W2_num <= 16 ~ W2_num,
      W4_num >= 1 & W4_num <= 16 ~ W4_num,
      W8_num >= 1 & W8_num <= 16 ~ W8_num,
      W9_num >= 1 & W9_num <= 16 ~ W9_num,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-ends_with('_num'))

# Write CSV directly
output <- cleaned %>% select(NSID, eth)
write_csv(output, 'data/output/cleaned_data.csv')

print('Script completed successfully')
print(head(output))
print(table(as.integer(output$eth), useNA = 'always'))
}]
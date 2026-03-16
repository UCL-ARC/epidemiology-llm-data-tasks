library(haven)
library(dplyr)
library(readr)

input_dir <- 'data/input'
output_file <- 'data/output/cleaned_data.csv'

if (!dir.exists('data')) dir.create('data', showWarnings = FALSE)
if (!dir.exists('data/input')) dir.create('data/input', showWarnings = FALSE)
if (!dir.exists('data/output')) dir.create('data/output', showWarnings = FALSE)

files <- list(
  wave1 = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave2 = 'data/input/wave_two_lsype_young_person_2020.tab',
  wave3 = 'data/input/wave_three_lsype_young_person_2020.tab',
  wave4 = 'data/input/wave_four_lsype_young_person_2020.tab',
  wave6 = 'data/input/wave_six_lsype_young_person_2020.tab',
  wave7 = 'data/input/wave_seven_lsype_young_person_2020.tab',
  wave8 = 'data/input/ns8_2015_self_completion.tab',
  wave9 = 'data/input/ns9_2022_main_interview.tab'
)

wave1 <- read_delim(files$wave1, delim = '\t', show_col_types = FALSE)
wave2 <- read_delim(files$wave2, delim = '\t', show_col_types = FALSE)
wave3 <- read_delim(files$wave3, delim = '\t', show_col_types = FALSE)
wave4 <- read_delim(files$wave4, delim = '\t', show_col_types = FALSE)
wave6 <- read_delim(files$wave6, delim = '\t', show_col_types = FALSE)
wave7 <- read_delim(files$wave7, delim = '\t', show_col_types = FALSE)
wave8 <- read_delim(files$wave8, delim = '\t', show_col_types = FALSE)
wave9 <- read_delim(files$wave9, delim = '\t', show_col_types = FALSE)

# Extract alcohol variables as per metadata
# Age 14: NSID at 1, W1alceverYP at 273, W1alcmonYP at 274
wave1_alc <- wave1 %>% select(1, 273, 274) %>%
  mutate(age = 14)

# Age 15: NSID at 1, W2alceverYP at 453
wave2_alc <- wave2 %>% select(1, 453) %>% mutate(age = 15)

# Age 16: NSID at 1, W3alceverYP at 577
wave3_alc <- wave3 %>% select(1, 577) %>% mutate(age = 16)

# Age 17: NSID at 1, W4AlcEverYP at 885
wave4_alc <- wave4 %>% select(1, 885) %>% mutate(age = 17)

# Age 19: NSID at 1, W6AlcEverYP at 547
wave6_alc <- wave6 %>% select(1, 547) %>% mutate(age = 19)

# Age 20: NSID at 1, W7AlcEverYP at 617
wave7_alc <- wave7 %>% select(1, 617) %>% mutate(age = 20)

# Age 25: NSID at 1, W8AUDIT6 at 44
wave8_alc <- wave8 %>% select(1, 44) %>% mutate(age = 25)

# Age 32: NSID at 1, W9AUDIT1 at 1583
wave9_alc <- wave9 %>% select(1, 1583) %>% mutate(age = 32)

# Add variable names
names(wave1_alc) <- c('NSID', 'W1alceverYP', 'W1alcmonYP', 'age')
names(wave2_alc) <- c('NSID', 'W2alceverYP', 'age')
names(wave3_alc) <- c('NSID', 'W3alceverYP', 'age')
names(wave4_alc) <- c('NSID', 'W4AlcEverYP', 'age')
names(wave6_alc) <- c('NSID', 'W6AlcEverYP', 'age')
names(wave7_alc) <- c('NSID', 'W7AlcEverYP', 'age')
names(wave8_alc) <- c('NSID', 'W8AUDIT6', 'age')
names(wave9_alc) <- c('NSID', 'W9AUDIT1', 'age')

# Convert to numeric
wave1_alc <- wave1_alc %>% mutate(W1alceverYP = as.numeric(W1alceverYP), W1alcmonYP = as.numeric(W1alcmonYP))
wave2_alc <- wave2_alc %>% mutate(W2alceverYP = as.numeric(W2alceverYP))
wave3_alc <- wave3_alc %>% mutate(W3alceverYP = as.numeric(W3alceverYP))
wave4_alc <- wave4_alc %>% mutate(W4AlcEverYP = as.numeric(W4AlcEverYP))
wave6_alc <- wave6_alc %>% mutate(W6AlcEverYP = as.numeric(W6AlcEverYP))
wave7_alc <- wave7_alc %>% mutate(W7AlcEverYP = as.numeric(W7AlcEverYP))
wave8_alc <- wave8_alc %>% mutate(W8AUDIT6 = as.numeric(W8AUDIT6))
wave9_alc <- wave9_alc %>% mutate(W9AUDIT1 = as.numeric(W9AUDIT1))

# Create drinker indicators
wave1_ind <- wave1_alc %>% mutate(drinker = ifelse(W1alceverYP == 1 & W1alcmonYP == 1, 1, NA))
wave2_ind <- wave2_alc %>% mutate(drinker = ifelse(W2alceverYP == 1, 1, NA))
wave3_ind <- wave3_alc %>% mutate(drinker = ifelse(W3alceverYP == 1, 1, NA))
wave4_ind <- wave4_alc %>% mutate(drinker = ifelse(W4AlcEverYP == 1, 1, NA))
wave6_ind <- wave6_alc %>% mutate(drinker = ifelse(W6AlcEverYP == 1, 1, NA))
wave7_ind <- wave7_alc %>% mutate(drinker = ifelse(W7AlcEverYP == 1, 1, NA))
wave8_ind <- wave8_alc %>% mutate(drinker = ifelse(W8AUDIT6 > 1, 1, NA))
wave9_ind <- wave9_alc %>% mutate(drinker = ifelse(W9AUDIT1 > 1, 1, NA))

all_indicators <- bind_rows(
  wave1_ind %>% select(NSID, age, drinker),
  wave2_ind %>% select(NSID, age, drinker),
  wave3_ind %>% select(NSID, age, drinker),
  wave4_ind %>% select(NSID, age, drinker),
  wave6_ind %>% select(NSID, age, drinker),
  wave7_ind %>% select(NSID, age, drinker),
  wave8_ind %>% select(NSID, age, drinker),
  wave9_ind %>% select(NSID, age, drinker)
)

all_nsids <- unique(c(wave1_ind$NSID, wave2_ind$NSID, wave3_ind$NSID, wave4_ind$NSID, wave6_ind$NSID, wave7_ind$NSID, wave8_ind$NSID, wave9_ind$NSID))
all_possible <- expand.grid(NSID = all_nsids, age = c(14, 15, 16, 17, 19, 20, 25, 32))
merged <- all_possible %>% left_join(all_indicators, by = c('NSID', 'age'))

# Calculate alcfst
result <- merged %>%
  group_by(NSID) %>%
  summarise(
    has_drinker = any(drinker == 1, na.rm = TRUE),
    any_missing = any(is.na(drinker)),
    all_non_drinker = all(drinker == 0 | is.na(drinker)),
    .groups = 'drop'
  )

# Find earliest drinking age
earliest <- all_indicators %>%
  filter(drinker == 1) %>%
  group_by(NSID) %>%
  summarise(earliest_age = min(age), .groups = 'drop')

result <- result %>%
  left_join(earliest, by = 'NSID') %>%
  mutate(
    alcfst = case_when(
      has_drinker & !is.na(earliest_age) ~ earliest_age,
      !any_missing & all_non_drinker ~ 99,
      TRUE ~ -8
    )
  ) %>%
  select(NSID, alcfst) %>%
  arrange(NSID)

write_csv(result, output_file)
ncat('Output written to', output_file, '\n')
ncat('Total records:', nrow(result), '\n')
ncat('Unique alcfst:', paste(sort(unique(result$alcfst)), collapse=', '), '\n')
print(table(result$alcfst))

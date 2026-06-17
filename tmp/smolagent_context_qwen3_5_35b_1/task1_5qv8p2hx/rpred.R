library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
cat('Loading wave 1...\n')
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")

cat('Loading wave 2...\n')
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")

cat('Loading wave 3...\n')
wave3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")

cat('Loading wave 4...\n')
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")

cat('Loading wave 5...\n')
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")

cat('Loading wave 6...\n')
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")

cat('Loading wave 7...\n')
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")

cat('Loading wave 8...\n')
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")

cat('Loading wave 9...\n')
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

cat('All files loaded successfully.\n')

# Create a helper function to recode sex variables based on metadata labels
recode_sex <- function(x) {
  case_when(
    # Codes indicating not interviewed or missing data
    x %in% c(-99, -998, -997, -995) ~ -3,
    # Codes indicating refusal
    x %in% c(-92, -9) ~ -9,
    # Codes indicating not applicable
    x %in% c(-91) ~ -1,
    # Codes indicating don't know
    x %in% c(-1, -8) ~ -8,
    # Valid values (Male = 1, Female = 2)
    x %in% c(1, 2) ~ x,
    # Any other value defaults to not asked
    TRUE ~ -3
  )
}

cat('Recoding sex variables...\n')
# Apply recoding to all sex variables
merged <- wave1 %>%
  mutate(W1sexYP = recode_sex(W1sexYP)) %>%
  full_join(wave2 %>% mutate(W2SexYP = recode_sex(W2SexYP)), by = "NSID") %>%
  full_join(wave3 %>% mutate(W3sexYP = recode_sex(W3sexYP)), by = "NSID") %>%
  full_join(wave4 %>% mutate(W4SexYP = recode_sex(W4SexYP)), by = "NSID") %>%
  full_join(wave5 %>% mutate(W5SexYP = recode_sex(W5SexYP)), by = "NSID") %>%
  full_join(wave6 %>% mutate(W6Sex = recode_sex(W6Sex)), by = "NSID") %>%
  full_join(wave7 %>% mutate(W7Sex = recode_sex(W7Sex)), by = "NSID") %>%
  full_join(wave8 %>% mutate(W8CMSEX = recode_sex(W8CMSEX)), by = "NSID") %>%
  full_join(wave9 %>% mutate(W9DSEX = recode_sex(W9DSEX)), by = "NSID")

cat('Merged data structure:\n')
str(merged)

cat('\nCreating consolidated sex variable (most recent valid first)...\n')
# Create consolidated sex variable using most recent valid response first
# Order: W9DSEX → W8CMSEX → W7Sex → W6Sex → W5SexYP → W4SexYP → W3sexYP → W2SexYP → W1sexYP
merged$sex <- case_when(
  merged$W9DSEX %in% c(1, 2) ~ merged$W9DSEX,
  merged$W8CMSEX %in% c(1, 2) ~ merged$W8CMSEX,
  merged$W7Sex %in% c(1, 2) ~ merged$W7Sex,
  merged$W6Sex %in% c(1, 2) ~ merged$W6Sex,
  merged$W5SexYP %in% c(1, 2) ~ merged$W5SexYP,
  merged$W4SexYP %in% c(1, 2) ~ merged$W4SexYP,
  merged$W3sexYP %in% c(1, 2) ~ merged$W3sexYP,
  merged$W2SexYP %in% c(1, 2) ~ merged$W2SexYP,
  merged$W1sexYP %in% c(1, 2) ~ merged$W1sexYP,
  TRUE ~ NA_real_
)

cat('\nFinal output structure:\n')
# Select only NSID and sex for output
output <- merged %>% select(NSID, sex)
str(output)

cat('\nSummary statistics:\n')
print(table(output$sex, useNA = "ifany"))

# Write to CSV
cat('\nWriting output to data/output/cleaned_data.csv...\n')
write_csv(output, "data/output/cleaned_data.csv")

cat('\nDone!\n')

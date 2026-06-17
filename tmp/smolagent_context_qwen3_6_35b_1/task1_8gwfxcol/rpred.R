library(dplyr)
library(tidyr)
library(haven)
library(readr)
library(labelled)

# Ensure output directory exists
dir.create('data/output', showWarnings = FALSE)

# Load all files
s1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
s2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
s3 <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
s4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
s5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
s6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
s7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
s8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t', show_col_types = FALSE)
s9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)

cat('Loaded files\n')
cat('s1:', nrow(s1), 'rows\n')
cat('s2:', nrow(s2), 'rows\n')
cat('s3:', nrow(s3), 'rows\n')
cat('s4:', nrow(s4), 'rows\n')
cat('s5:', nrow(s5), 'rows\n')
cat('s6:', nrow(s6), 'rows\n')
cat('s7:', nrow(s7), 'rows\n')
cat('s8:', nrow(s8), 'rows\n')
cat('s9:', nrow(s9), 'rows\n')

# Standardise sex variables with proper missing value mapping

# W1sexYP: -99=YP not interviewed -> -3, -92=Refused -> -9, -91=Not applicable -> -1
s1 <- s1 %>%
  mutate(W1sexYP_clean = case_when(
    W1sexYP == -99 ~ -3,
    W1sexYP == -92 ~ -9,
    W1sexYP == -91 ~ -1,
    W1sexYP == 1 ~ 1,
    W1sexYP == 2 ~ 2,
    is.na(W1sexYP) ~ -3,
    TRUE ~ -3
  ))

# W2SexYP: -998=Interviewer missed question -> -2, -997=Script error -> -2,
#          -995=Missing history section data -> -2, -99=YP not interviewed -> -3,
#          -92=Refused -> -9, -91=Not applicable -> -1, -1=Don\'t know -> -8
s2 <- s2 %>%
  mutate(W2SexYP_clean = case_when(
    W2SexYP == -998 ~ -2,
    W2SexYP == -997 ~ -2,
    W2SexYP == -995 ~ -2,
    W2SexYP == -99 ~ -3,
    W2SexYP == -92 ~ -9,
    W2SexYP == -91 ~ -1,
    W2SexYP == -1 ~ -8,
    W2SexYP == 1 ~ 1,
    W2SexYP == 2 ~ 2,
    is.na(W2SexYP) ~ -3,
    TRUE ~ -3
  ))

# W3sexYP: -99=YP not interviewed -> -3, -92=Refused -> -9, -91=Not applicable -> -1
s3 <- s3 %>%
  mutate(W3sexYP_clean = case_when(
    W3sexYP == -99 ~ -3,
    W3sexYP == -92 ~ -9,
    W3sexYP == -91 ~ -1,
    W3sexYP == 1 ~ 1,
    W3sexYP == 2 ~ 2,
    is.na(W3sexYP) ~ -3,
    TRUE ~ -3
  ))

# W4SexYP: -99=YP not interviewed -> -3, -92=Refused -> -9,
#          -91=Not applicable -> -1, -1=Don\'t know -> -8
s4 <- s4 %>%
  mutate(W4SexYP_clean = case_when(
    W4SexYP == -99 ~ -3,
    W4SexYP == -92 ~ -9,
    W4SexYP == -91 ~ -1,
    W4SexYP == -1 ~ -8,
    W4SexYP == 1 ~ 1,
    W4SexYP == 2 ~ 2,
    is.na(W4SexYP) ~ -3,
    TRUE ~ -3
  ))

# W5SexYP: -1=Don\'t know -> -8
s5 <- s5 %>%
  mutate(W5SexYP_clean = case_when(
    W5SexYP == -1 ~ -8,
    W5SexYP == 1 ~ 1,
    W5SexYP == 2 ~ 2,
    is.na(W5SexYP) ~ -3,
    TRUE ~ -3
  ))

# W6Sex: -92=Refused -> -9, -91=Not applicable -> -1
s6 <- s6 %>%
  mutate(W6Sex_clean = case_when(
    W6Sex == -92 ~ -9,
    W6Sex == -91 ~ -1,
    W6Sex == 1 ~ 1,
    W6Sex == 2 ~ 2,
    is.na(W6Sex) ~ -3,
    TRUE ~ -3
  ))

# W7Sex: -91=Not applicable -> -1
s7 <- s7 %>%
  mutate(W7Sex_clean = case_when(
    W7Sex == -91 ~ -1,
    W7Sex == 1 ~ 1,
    W7Sex == 2 ~ 2,
    is.na(W7Sex) ~ -3,
    TRUE ~ -3
  ))

# W8CMSEX: -9=Refused -> -9, -8=Don\'t know -> -8, -1=Not applicable -> -1
s8 <- s8 %>%
  mutate(W8CMSEX_clean = case_when(
    W8CMSEX == -9 ~ -9,
    W8CMSEX == -8 ~ -8,
    W8CMSEX == -1 ~ -1,
    W8CMSEX == 1 ~ 1,
    W8CMSEX == 2 ~ 2,
    is.na(W8CMSEX) ~ -3,
    TRUE ~ -3
  ))

# W9DSEX: 1=Male, 2=Female (no user missing values in metadata)
s9 <- s9 %>%
  mutate(W9DSEX_clean = case_when(
    W9DSEX == 1 ~ 1,
    W9DSEX == 2 ~ 2,
    is.na(W9DSEX) ~ -3,
    TRUE ~ -3
  ))

# Merge all files by NSID
merged <- full_join(s1, s2, by = 'NSID')
merged <- full_join(merged, s3, by = 'NSID')
merged <- full_join(merged, s4, by = 'NSID')
merged <- full_join(merged, s5, by = 'NSID')
merged <- full_join(merged, s6, by = 'NSID')
merged <- full_join(merged, s7, by = 'NSID')
merged <- full_join(merged, s8, by = 'NSID')
merged <- full_join(merged, s9, by = 'NSID')

cat('Merged data:', nrow(merged), 'rows\n')

# Create consolidated sex variable
# Logic: most recent valid first (W9DSEX), then fall back from earliest to most recent
# Order: W9DSEX -> W1sexYP -> W2SexYP -> W3sexYP -> W4SexYP -> W5SexYP -> W6Sex -> W7Sex -> W8CMSEX
merged <- merged %>%
  mutate(
    sex = case_when(
      # Most recent first
      !is.na(W9DSEX_clean) & W9DSEX_clean %in% c(1, 2) ~ W9DSEX_clean,
      # Then earliest to most recent
      !is.na(W1sexYP_clean) & W1sexYP_clean %in% c(1, 2) ~ W1sexYP_clean,
      !is.na(W2SexYP_clean) & W2SexYP_clean %in% c(1, 2) ~ W2SexYP_clean,
      !is.na(W3sexYP_clean) & W3sexYP_clean %in% c(1, 2) ~ W3sexYP_clean,
      !is.na(W4SexYP_clean) & W4SexYP_clean %in% c(1, 2) ~ W4SexYP_clean,
      !is.na(W5SexYP_clean) & W5SexYP_clean %in% c(1, 2) ~ W5SexYP_clean,
      !is.na(W6Sex_clean) & W6Sex_clean %in% c(1, 2) ~ W6Sex_clean,
      !is.na(W7Sex_clean) & W7Sex_clean %in% c(1, 2) ~ W7Sex_clean,
      !is.na(W8CMSEX_clean) & W8CMSEX_clean %in% c(1, 2) ~ W8CMSEX_clean,
      TRUE ~ -3
    )
  )

# Select only NSID and sex for output
output <- merged %>%
  select(NSID, sex)

cat('Output data:', nrow(output), 'rows\n')

# Write output
write_csv(output, 'data/output/cleaned_data.csv')

cat('Done. Output written to data/output/cleaned_data.csv\n')

# Show some summary statistics
cat('\nSex distribution:\n')
print(table(output$sex, useNA = 'ifany'))

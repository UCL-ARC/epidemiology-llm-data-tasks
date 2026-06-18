library(haven)
library(dplyr)
library(readr)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Start with the smallest file to ensure we have the full cohort
master <- wave6

# Perform full joins with each dataset
master <- full_join(master, wave7, by = 'NSID')
master <- full_join(master, ns8, by = 'NSID')
master <- full_join(master, ns9, by = 'NSID')
master <- full_join(master, wave1, by = 'NSID')
master <- full_join(master, wave4, by = 'NSID')

# Map missing values to standard codes
# Standard codes: -9=Refusal, -8=Don't know, -7=Prefer not to say, -3=Not asked, -2=Not applicable (script), -1=Item not applicable

# For age 19 (wave6): W6SexualityYP
master <- master %>%
  mutate(
    W6SexualityYP = case_when(
      W6SexualityYP == -97 ~ -9,
      W6SexualityYP == -92 ~ -9,
      W6SexualityYP == -91 ~ -1,
      W6SexualityYP == -1 ~ -8,
      NA == W6SexualityYP ~ -3,
      TRUE ~ W6SexualityYP
    )
  )

# For age 20 (wave7): W7SexualityYP
master <- master %>%
  mutate(
    W7SexualityYP = case_when(
      W7SexualityYP == -100 ~ -2,
      W7SexualityYP == -97 ~ -9,
      W7SexualityYP == -92 ~ -9,
      W7SexualityYP == -91 ~ -1,
      W7SexualityYP == -1 ~ -8,
      NA == W7SexualityYP ~ -3,
      TRUE ~ W7SexualityYP
    )
  )

# For age 25 (ns8): W8SEXUALITY
master <- master %>%
  mutate(
    W8SEXUALITY = case_when(
      W8SEXUALITY == -9 ~ -9,
      W8SEXUALITY == -8 ~ -8,
      W8SEXUALITY == -1 ~ -1,
      NA == W8SEXUALITY ~ -3,
      TRUE ~ W8SEXUALITY
    )
  )

# For age 32 (ns9): W9SORI
master <- master %>%
  mutate(
    W9SORI = case_when(
      W9SORI == -9 ~ -9,
      W9SORI == -8 ~ -8,
      W9SORI == -3 ~ -3,
      W9SORI == -1 ~ -1,
      NA == W9SORI ~ -3,
      TRUE ~ W9SORI
    )
  )

# Create final output variables based on age mapping
master <- master %>%
  mutate(
    sori19 = W6SexualityYP,
    sori20 = W7SexualityYP,
    sori25 = W8SEXUALITY,
    sori32 = W9SORI
  ) %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write output
write_csv(master, 'data/output/cleaned_data.csv')

cat('Script completed successfully. Output written to data/output/cleaned_data.csv\n')

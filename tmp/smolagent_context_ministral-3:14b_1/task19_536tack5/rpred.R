library(haven)
library(dplyr)
library(purrr)
library(readr)

# Step 1: Load files and merge
wave_one_young_person <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four_young_person <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_derived <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_derived <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge wave 1 and wave 4 to establish full cohort
full_cohort <- full_join(wave_one_young_person, wave_four_young_person, by = "NSID")

# Merge with derived variables
full_cohort <- full_join(full_cohort, ns8_derived, by = "NSID")
full_cohort <- full_join(full_cohort, ns9_derived, by = "NSID")

# Step 2: Rename variables
full_cohort <- full_cohort %>%
  rename(bmi25 = W8DBMI, bmi32 = W9DBMI)

# Step 3: Harmonize missing values
full_cohort <- full_cohort %>%
  mutate(
    bmi25 = case_when(
      bmi25 == -9 ~ -9,
      bmi25 == -8 ~ -8,
      bmi25 == -1 ~ -1,
      is.na(bmi25) ~ -3,
      TRUE ~ bmi25
    ),
    bmi32 = case_when(
      bmi32 == -9 ~ -9,
      bmi32 == -8 ~ -8,
      bmi32 == -1 ~ -1,
      is.na(bmi32) ~ -3,
      TRUE ~ bmi32
    )
  )

# Step 4: Ensure variables are numeric
full_cohort <- full_cohort %>%
  mutate(
    bmi25 = as.numeric(bmi25),
    bmi32 = as.numeric(bmi32)
  )

# Step 5: Select only required variables
cleaned_data <- full_cohort %>%
  select(NSID, bmi25, bmi32)

# Step 6: Write output
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)
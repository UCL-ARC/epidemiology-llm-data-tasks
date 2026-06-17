library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Set global options for labelled data
options(stringsAsFactors = FALSE)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = FALSE)

# Load all data files
data_wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
data_wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
data_wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
data_wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
data_wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
data_wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
data_wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
data_wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
data_wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Create a full cohort frame by full_joining all datasets
cohort <- data_wave1 %>%
  full_join(data_wave2, by = "NSID") %>%
  full_join(data_wave3, by = "NSID") %>%
  full_join(data_wave4, by = "NSID") %>%
  full_join(data_wave5, by = "NSID") %>%
  full_join(data_wave6, by = "NSID") %>%
  full_join(data_wave7, by = "NSID") %>%
  full_join(data_wave8, by = "NSID") %>%
  full_join(data_wave9, by = "NSID")

# Process sweeps 1-7 for missing value harmonization
# In sweeps 1-7: source code -1 labelled "Don't know" maps to -8
cohort <- cohort %>%
  mutate(
    # Sweep 1
    W1hous12HH_clean = case_when(
      is.na(W1hous12HH) ~ -3,
      W1hous12HH == -1 ~ -8,
      TRUE ~ as.integer(W1hous12HH)
    ),
    # Sweep 2
    W2Hous12HH_clean = case_when(
      is.na(W2Hous12HH) ~ -3,
      W2Hous12HH == -1 ~ -8,
      TRUE ~ as.integer(W2Hous12HH)
    ),
    # Sweep 3
    W3hous12HH_clean = case_when(
      is.na(W3hous12HH) ~ -3,
      W3hous12HH == -1 ~ -8,
      TRUE ~ as.integer(W3hous12HH)
    ),
    # Sweep 4
    W4Hous12HH_clean = case_when(
      is.na(W4Hous12HH) ~ -3,
      W4Hous12HH == -1 ~ -8,
      TRUE ~ as.integer(W4Hous12HH)
    ),
    # Sweep 5
    W5Hous12HH_clean = case_when(
      is.na(W5Hous12HH) ~ -3,
      W5Hous12HH == -1 ~ -8,
      TRUE ~ as.integer(W5Hous12HH)
    ),
    W5Hous12BHH_clean = case_when(
      is.na(W5Hous12BHH) ~ -3,
      W5Hous12BHH == -1 ~ -8,
      TRUE ~ as.integer(W5Hous12BHH)
    ),
    W5Hous12CHH_clean = case_when(
      is.na(W5Hous12CHH) ~ -3,
      W5Hous12CHH == -1 ~ -8,
      TRUE ~ as.integer(W5Hous12CHH)
    ),
    # Sweep 6
    W6Hous12YP_clean = case_when(
      is.na(W6Hous12YP) ~ -3,
      W6Hous12YP == -1 ~ -8,
      TRUE ~ as.integer(W6Hous12YP)
    ),
    W6Hous12bYP_clean = case_when(
      is.na(W6Hous12bYP) ~ -3,
      W6Hous12bYP == -1 ~ -8,
      TRUE ~ as.integer(W6Hous12bYP)
    ),
    W6Hous12cYP_clean = case_when(
      is.na(W6Hous12cYP) ~ -3,
      W6Hous12cYP == -1 ~ -8,
      TRUE ~ as.integer(W6Hous12cYP)
    ),
    # Sweep 7
    W7Hous12YP_clean = case_when(
      is.na(W7Hous12YP) ~ -3,
      W7Hous12YP == -1 ~ -8,
      TRUE ~ as.integer(W7Hous12YP)
    ),
    W7Hous12bYP_clean = case_when(
      is.na(W7Hous12bYP) ~ -3,
      W7Hous12bYP == -1 ~ -8,
      TRUE ~ as.integer(W7Hous12bYP)
    ),
    W7Hous12cYP_clean = case_when(
      is.na(W7Hous12cYP) ~ -3,
      W7Hous12cYP == -1 ~ -8,
      TRUE ~ as.integer(W7Hous12cYP)
    )
  )

# Process sweeps 8-9: -1 retains its standard meaning

# Now create the detailed and collapsed variables

# Sweeps 1-4: Detailed (copy directly from cleaned variables)
cohort <- cohort %>%
  mutate(
    hownteen14 = W1hous12HH_clean,
    hown14 = case_when(
      W1hous12HH_clean >= 4 & W1hous12HH_clean <= 6 ~ 4,
      W1hous12HH_clean == 8 ~ 8,
      TRUE ~ W1hous12HH_clean
    ),
    hownteen15 = W2Hous12HH_clean,
    hown15 = case_when(
      W2Hous12HH_clean >= 4 & W2Hous12HH_clean <= 6 ~ 4,
      W2Hous12HH_clean == 8 ~ 8,
      TRUE ~ W2Hous12HH_clean
    ),
    hownteen16 = W3hous12HH_clean,
    hown16 = case_when(
      W3hous12HH_clean >= 4 & W3hous12HH_clean <= 6 ~ 4,
      W3hous12HH_clean == 8 ~ 8,
      TRUE ~ W3hous12HH_clean
    ),
    hownteen17 = W4Hous12HH_clean,
    hown17 = case_when(
      W4Hous12HH_clean >= 4 & W4Hous12HH_clean <= 6 ~ 4,
      W4Hous12HH_clean == 8 ~ 8,
      TRUE ~ W4Hous12HH_clean
    )
  )

# Sweeps 5-7: Three source variables per sweep
# Priority: owned-subtype before rented-subtype
# "Some other arrangement" from any source → 8

# Sweep 5 (age 18)
cohort <- cohort %>%
  mutate(
    hownteen18 = case_when(
      W5Hous12BHH_clean >= 1 & W5Hous12BHH_clean <= 4 ~ W5Hous12BHH_clean,
      W5Hous12CHH_clean >= 1 & W5Hous12CHH_clean <= 5 ~ W5Hous12CHH_clean,
      TRUE ~ as.numeric(NA)
    ),
    hown18 = case_when(
      W5Hous12BHH_clean == 1 ~ 1,
      W5Hous12BHH_clean == 2 ~ 2,
      W5Hous12BHH_clean == 3 ~ 3,
      W5Hous12BHH_clean == 4 ~ 8,
      W5Hous12CHH_clean %in% c(1, 2, 3) ~ 4,
      W5Hous12CHH_clean == 4 ~ 4,
      W5Hous12CHH_clean == 5 ~ 8,
      TRUE ~ as.numeric(NA)
    )
  )

# Sweep 6 (age 19)
cohort <- cohort %>%
  mutate(
    hownteen19 = case_when(
      W6Hous12bYP_clean >= 1 & W6Hous12bYP_clean <= 4 ~ W6Hous12bYP_clean,
      W6Hous12cYP_clean >= 1 & W6Hous12cYP_clean <= 5 ~ W6Hous12cYP_clean,
      TRUE ~ as.numeric(NA)
    ),
    hown19 = case_when(
      W6Hous12bYP_clean == 1 ~ 1,
      W6Hous12bYP_clean == 2 ~ 2,
      W6Hous12bYP_clean == 3 ~ 3,
      W6Hous12bYP_clean == 4 ~ 8,
      W6Hous12cYP_clean %in% c(1, 2, 3) ~ 4,
      W6Hous12cYP_clean == 4 ~ 4,
      W6Hous12cYP_clean == 5 ~ 8,
      TRUE ~ as.numeric(NA)
    )
  )

# Sweep 7 (age 20)
cohort <- cohort %>%
  mutate(
    hownteen20 = case_when(
      W7Hous12bYP_clean >= 1 & W7Hous12bYP_clean <= 4 ~ W7Hous12bYP_clean,
      W7Hous12cYP_clean >= 1 & W7Hous12cYP_clean <= 5 ~ W7Hous12cYP_clean,
      TRUE ~ as.numeric(NA)
    ),
    hown20 = case_when(
      W7Hous12bYP_clean == 1 ~ 1,
      W7Hous12bYP_clean == 2 ~ 2,
      W7Hous12bYP_clean == 3 ~ 3,
      W7Hous12bYP_clean == 4 ~ 8,
      W7Hous12cYP_clean %in% c(1, 2, 3) ~ 4,
      W7Hous12cYP_clean == 4 ~ 4,
      W7Hous12cYP_clean == 5 ~ 8,
      TRUE ~ as.numeric(NA)
    )
  )

# Sweeps 8-9: Single source variable per sweep
# For sweeps 8-9: Squatting and Other collapse to 6

# Sweep 8 (age 25)
cohort <- cohort %>%
  mutate(
    hown25 = case_when(
      W8TENURE %in% c(4, 5) ~ 4,
      W8TENURE %in% c(6, 7) ~ 6,
      TRUE ~ W8TENURE
    )
  )

# Sweep 9 (age 32)
cohort <- cohort %>%
  mutate(
    hown32 = case_when(
      W9DTENURE %in% c(4, 5) ~ 4,
      W9DTENURE %in% c(6, 7) ~ 6,
      TRUE ~ W9DTENURE
    )
  )

# Remove raw source variables, keep only NSID and final derived variables
cohort <- cohort %>%
  select(NSID, starts_with("hownteen"), starts_with("hown"))

# Write to CSV
write_csv(cohort, "data/output/cleaned_data.csv")

cat("Script completed successfully\n")

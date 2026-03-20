library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory
dir.create("data/output", recursive = TRUE, showWarnings = FALSE)

# Load all input files
wave1 <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- readr::read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
wave6 <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- readr::read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID using full_join
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to recode missing values to standard codes
recode_missing <- function(x) {
  case_when(
    x == -999 ~ -3,  # Missing household grid/info -> Not asked
    x == -998 ~ -2,  # Interviewer missed question -> Script error
    x == -997 ~ -2,  # Script error -> Script error
    x == -995 ~ -2,  # Missing history section -> Script error/info lost
    x == -99 ~ -3,   # Missing -> Not asked
    x == -92 ~ -9,   # Refused -> Refusal
    x == -91 ~ -1,   # Not applicable -> Not applicable
    x == -9 ~ -9,    # Refused -> Refusal
    x == -8 ~ -8,    # Don't know -> Don't know
    x == -1 ~ -8,    # Don't know -> Don't know
    x < 0 ~ -3,      # Other negative -> Not asked
    TRUE ~ as.numeric(x)
  )
}

# Create detailed adolescent variables (hownteen14-20)
# Waves 1-4: Direct mapping from single tenure variable
merged_data <- merged_data %>%
  mutate(
    hownteen14 = recode_missing(W1hous12HH),
    hownteen15 = recode_missing(W2Hous12HH),
    hownteen16 = recode_missing(W3hous12HH),
    hownteen17 = recode_missing(W4Hous12HH)
  )

# Waves 5-7: Need to combine type and sub-type variables
# Wave 5 (Age 18)
merged_data <- merged_data %>%
  mutate(
    W5Hous12HH_rec = recode_missing(W5Hous12HH),
    W5Hous12BHH_rec = recode_missing(W5Hous12BHH),
    W5Hous12CHH_rec = recode_missing(W5Hous12CHH),
    hownteen18 = case_when(
      W5Hous12HH_rec == 1 ~ case_when(
        W5Hous12BHH_rec == 1 ~ 1,  # Owned outright
        W5Hous12BHH_rec == 2 ~ 2,  # Being bought on mortgage
        W5Hous12BHH_rec == 3 ~ 3,  # Shared ownership
        W5Hous12BHH_rec == 4 ~ 8,  # Some other arrangement
        W5Hous12BHH_rec < 0 ~ W5Hous12BHH_rec,
        TRUE ~ 8
      ),
      W5Hous12HH_rec == 2 ~ case_when(
        W5Hous12CHH_rec == 1 ~ 4,  # Rented from Council
        W5Hous12CHH_rec == 2 ~ 5,  # Rented from Housing Association
        W5Hous12CHH_rec == 3 ~ 6,  # Rented privately
        W5Hous12CHH_rec == 4 ~ 7,  # Rent free
        W5Hous12CHH_rec == 5 ~ 8,  # Some other arrangement
        W5Hous12CHH_rec < 0 ~ W5Hous12CHH_rec,
        TRUE ~ 8
      ),
      W5Hous12HH_rec == 3 ~ 8,  # Something else
      W5Hous12HH_rec < 0 ~ W5Hous12HH_rec,
      TRUE ~ 8
    )
  )

# Wave 6 (Age 19)
merged_data <- merged_data %>%
  mutate(
    W6Hous12YP_rec = recode_missing(W6Hous12YP),
    W6Hous12bYP_rec = recode_missing(W6Hous12bYP),
    W6Hous12cYP_rec = recode_missing(W6Hous12cYP),
    hownteen19 = case_when(
      W6Hous12YP_rec == 1 ~ case_when(
        W6Hous12bYP_rec == 1 ~ 1,  # Owned outright
        W6Hous12bYP_rec == 2 ~ 2,  # Being bought on mortgage
        W6Hous12bYP_rec == 3 ~ 3,  # Shared ownership
        W6Hous12bYP_rec == 4 ~ 8,  # Some other arrangement
        W6Hous12bYP_rec < 0 ~ W6Hous12bYP_rec,
        TRUE ~ 8
      ),
      W6Hous12YP_rec == 2 ~ case_when(
        W6Hous12cYP_rec == 1 ~ 4,  # Rented from Council
        W6Hous12cYP_rec == 2 ~ 5,  # Rented from Housing Association
        W6Hous12cYP_rec == 3 ~ 6,  # Rented privately
        W6Hous12cYP_rec == 4 ~ 7,  # Rent free
        W6Hous12cYP_rec == 5 ~ 8,  # Some other arrangement
        W6Hous12cYP_rec < 0 ~ W6Hous12cYP_rec,
        TRUE ~ 8
      ),
      W6Hous12YP_rec == 3 ~ 8,  # Something else
      W6Hous12YP_rec < 0 ~ W6Hous12YP_rec,
      TRUE ~ 8
    )
  )

# Wave 7 (Age 20)
merged_data <- merged_data %>%
  mutate(
    W7Hous12YP_rec = recode_missing(W7Hous12YP),
    W7Hous12bYP_rec = recode_missing(W7Hous12bYP),
    W7Hous12cYP_rec = recode_missing(W7Hous12cYP),
    hownteen20 = case_when(
      W7Hous12YP_rec == 1 ~ case_when(
        W7Hous12bYP_rec == 1 ~ 1,  # Owned outright
        W7Hous12bYP_rec == 2 ~ 2,  # Being bought on mortgage
        W7Hous12bYP_rec == 3 ~ 3,  # Shared ownership
        W7Hous12bYP_rec == 4 ~ 8,  # Some other arrangement
        W7Hous12bYP_rec < 0 ~ W7Hous12bYP_rec,
        TRUE ~ 8
      ),
      W7Hous12YP_rec == 2 ~ case_when(
        W7Hous12cYP_rec == 1 ~ 4,  # Rented from Council
        W7Hous12cYP_rec == 2 ~ 5,  # Rented from Housing Association
        W7Hous12cYP_rec == 3 ~ 6,  # Rented privately
        W7Hous12cYP_rec == 4 ~ 7,  # Rent free
        W7Hous12cYP_rec == 5 ~ 8,  # Some other arrangement
        W7Hous12cYP_rec < 0 ~ W7Hous12cYP_rec,
        TRUE ~ 8
      ),
      W7Hous12YP_rec == 3 ~ 8,  # Something else
      W7Hous12YP_rec < 0 ~ W7Hous12YP_rec,
      TRUE ~ 8
    )
  )

# Create collapsed adolescent variables (hown14-20)
# Mapping: 1,2 -> 1 (Owned); 3 -> 2 (Part rent/mortgage); 4,5,6 -> 3 (Rent); 7 -> 4 (Rent free); 8 -> 5 (Other)
recode_collapsed <- function(x) {
  case_when(
    x %in% c(1, 2) ~ 1,   # Owned outright or mortgage -> Owned
    x == 3 ~ 2,            # Shared ownership -> Part rent/mortgage
    x %in% c(4, 5, 6) ~ 3, # Rented -> Rent it
    x == 7 ~ 4,            # Rent free -> Rent free
    x == 8 ~ 5,            # Other -> Other
    x == -9 ~ -9,          # Refusal
    x == -8 ~ -8,          # Don't know
    x == -1 ~ -1,          # Not applicable
    x == -3 ~ -3,          # Not asked
    x == -2 ~ -2,          # Script error
    TRUE ~ as.numeric(x)
  )
}

merged_data <- merged_data %>%
  mutate(
    hown14 = recode_collapsed(hownteen14),
    hown15 = recode_collapsed(hownteen15),
    hown16 = recode_collapsed(hownteen16),
    hown17 = recode_collapsed(hownteen17),
    hown18 = recode_collapsed(hownteen18),
    hown19 = recode_collapsed(hownteen19),
    hown20 = recode_collapsed(hownteen20)
  )

# Create adult collapsed variables (hown25, hown32)
# Wave 8 (Age 25)
merged_data <- merged_data %>%
  mutate(
    hown25 = case_when(
      W8TENURE == 1 ~ 1,   # Own outright -> Owned outright
      W8TENURE == 2 ~ 1,   # Own buying with mortgage -> Owned with mortgage
      W8TENURE == 3 ~ 2,   # Part rent/mortgage -> Part rent, part mortgage
      W8TENURE == 4 ~ 3,   # Rent inc Housing Ben -> Rent it
      W8TENURE == 5 ~ 4,   # Rent-free -> live rent-free
      W8TENURE == 6 ~ 5,   # Squatting -> Other
      W8TENURE == 7 ~ 5,   # Other arrangement -> Other
      W8TENURE == -9 ~ -9, # Refused
      W8TENURE == -8 ~ -8, # Don't know
      W8TENURE == -1 ~ -1, # Not applicable
      W8TENURE < 0 ~ -3,
      TRUE ~ as.numeric(W8TENURE)
    )
  )

# Wave 9 (Age 32)
merged_data <- merged_data %>%
  mutate(
    hown32 = case_when(
      W9DTENURE == 1 ~ 1,   # Own outright -> Owned outright
      W9DTENURE == 2 ~ 1,   # Own buying with mortgage -> Owned with mortgage
      W9DTENURE == 3 ~ 2,   # Part rent, part mortgage -> Part rent, part mortgage
      W9DTENURE == 4 ~ 3,   # Rent it -> Rent it
      W9DTENURE == 5 ~ 4,   # Live rent-free -> live rent-free
      W9DTENURE == 6 ~ 5,   # Squatting -> Other
      W9DTENURE == 7 ~ 5,   # Other -> Other
      W9DTENURE == -8 ~ -8, # Insufficient information
      W9DTENURE < 0 ~ -3,
      TRUE ~ as.numeric(W9DTENURE)
    )
  )

# Create final cleaned dataset with required variables only
cleaned_data <- merged_data %>%
  select(NSID, hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32,
         hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20)

# Write to CSV
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
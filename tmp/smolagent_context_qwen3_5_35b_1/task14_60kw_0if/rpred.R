library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all files
merged <- full_join(w1, w2, by = "NSID")
merged <- full_join(merged, w3, by = "NSID")
merged <- full_join(merged, w4, by = "NSID")
merged <- full_join(merged, w5, by = "NSID")
merged <- full_join(merged, w6, by = "NSID")
merged <- full_join(merged, w7, by = "NSID")
merged <- full_join(merged, w8, by = "NSID")
merged <- full_join(merged, w9, by = "NSID")

# Function to collapse 8 categories to 6
collapse_to_6 <- function(x) {
  case_when(
    x %in% c(1, 2, 3) ~ 1,  # All ownership categories -> 1
    x %in% c(4, 5, 6) ~ 2,  # All rental categories -> 2 (Rent it)
    x == 7 ~ 3,  # Rent free -> 3
    x == 8 ~ 4,  # Some other arrangement -> 4
    TRUE ~ x
  )
}

# Process Sweeps 1-4 (single source variable)
# Map -1 (Don't know) to -8 for sweeps 1-7
merged <- merged %>%
  mutate(
    hownteen14 = case_when(
      W1hous12HH == -1 ~ -8,
      TRUE ~ W1hous12HH
    ),
    hown14 = collapse_to_6(hownteen14),
    
    hownteen15 = case_when(
      W2Hous12HH == -1 ~ -8,
      TRUE ~ W2Hous12HH
    ),
    hown15 = collapse_to_6(hownteen15),
    
    hownteen16 = case_when(
      W3hous12HH == -1 ~ -8,
      TRUE ~ W3hous12HH
    ),
    hown16 = collapse_to_6(hownteen16),
    
    hownteen17 = case_when(
      W4Hous12HH == -1 ~ -8,
      TRUE ~ W4Hous12HH
    ),
    hown17 = collapse_to_6(hownteen17)
  )

# Process Sweep 5 (three source variables)
merged <- merged %>%
  mutate(
    hownteen18 = case_when(
      W5Hous12HH == 1 ~ case_when(
        W5Hous12BHH %in% c(1, 2, 3, 4) ~ W5Hous12BHH,
        TRUE ~ W5Hous12BHH
      ),
      W5Hous12HH == 2 ~ case_when(
        W5Hous12CHH %in% c(1, 2, 3, 4, 5) ~ W5Hous12CHH,
        TRUE ~ W5Hous12CHH
      ),
      W5Hous12HH == 3 ~ 8,
      W5Hous12HH %in% c(-1, -91, -92, -999) ~ W5Hous12HH,
      TRUE ~ NA_real_
    ),
    hown18 = collapse_to_6(hownteen18)
  )

# Process Sweep 6 (three source variables)
merged <- merged %>%
  mutate(
    hownteen19 = case_when(
      W6Hous12YP == 1 ~ case_when(
        W6Hous12bYP %in% c(1, 2, 3, 4) ~ W6Hous12bYP,
        TRUE ~ W6Hous12bYP
      ),
      W6Hous12YP == 2 ~ case_when(
        W6Hous12cYP %in% c(1, 2, 3, 4, 5) ~ W6Hous12cYP,
        TRUE ~ W6Hous12cYP
      ),
      W6Hous12YP == 3 ~ 8,
      W6Hous12YP %in% c(-1, -91, -92, -999) ~ W6Hous12YP,
      TRUE ~ NA_real_
    ),
    hown19 = collapse_to_6(hownteen19)
  )

# Process Sweep 7 (three source variables)
merged <- merged %>%
  mutate(
    hownteen20 = case_when(
      W7Hous12YP == 1 ~ case_when(
        W7Hous12bYP %in% c(1, 2, 3, 4) ~ W7Hous12bYP,
        TRUE ~ W7Hous12bYP
      ),
      W7Hous12YP == 2 ~ case_when(
        W7Hous12cYP %in% c(1, 2, 3, 4, 5) ~ W7Hous12cYP,
        TRUE ~ W7Hous12cYP
      ),
      W7Hous12YP == 3 ~ 8,
      W7Hous12YP %in% c(-1, -91, -92, -999) ~ W7Hous12YP,
      TRUE ~ NA_real_
    ),
    hown20 = collapse_to_6(hownteen20)
  )

# Process Sweep 8 (single source variable, -1 is Not applicable)
merged <- merged %>%
  mutate(
    hownteen25 = case_when(
      W8TENURE %in% c(-9, -8, -1) ~ W8TENURE,
      W8TENURE %in% c(1, 2, 3) ~ 1,
      W8TENURE %in% c(4) ~ 2,
      W8TENURE %in% c(5) ~ 3,
      W8TENURE %in% c(6, 7) ~ 6,
      TRUE ~ W8TENURE
    ),
    hown25 = case_when(
      hownteen25 %in% c(1, 2, 3) ~ 1,
      hownteen25 == 2 ~ 2,
      hownteen25 == 3 ~ 3,
      hownteen25 == 6 ~ 6,
      TRUE ~ hownteen25
    )
  )

# Process Sweep 9 (single source variable)
merged <- merged %>%
  mutate(
    hownteen32 = case_when(
      W9DTENURE %in% c(-8) ~ -8,
      W9DTENURE %in% c(1, 2, 3) ~ 1,
      W9DTENURE %in% c(4) ~ 2,
      W9DTENURE %in% c(5) ~ 3,
      W9DTENURE %in% c(6) ~ 6,
      W9DTENURE %in% c(7) ~ 7,
      TRUE ~ W9DTENURE
    ),
    hown32 = case_when(
      hownteen32 %in% c(1, 2, 3) ~ 1,
      hownteen32 == 2 ~ 2,
      hownteen32 == 3 ~ 3,
      hownteen32 == 6 ~ 6,
      hownteen32 == 7 ~ 7,
      TRUE ~ hownteen32
    )
  )

# Keep only NSID and the derived variables
final <- merged %>%
  select(NSID, hownteen14, hown14, hownteen15, hown15, hownteen16, hown16, hownteen17, hown17,
         hownteen18, hown18, hownteen19, hown19, hownteen20, hown20,
         hown25, hown32)

# Write output
write_csv(final, "data/output/cleaned_data.csv")

# Show summary
cat("Output dimensions:", dim(final), "\n")
cat("Variables:", names(final), "\n")
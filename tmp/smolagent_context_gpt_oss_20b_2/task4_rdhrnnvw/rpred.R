library(readr)
library(dplyr)

# Define file paths
files <- list(
  w1 = "data/input/wave_one_lsype_young_person_2020.tab",
  w4 = "data/input/wave_four_lsype_young_person_2020.tab",
  w6 = "data/input/wave_six_lsype_young_person_2020.tab",
  w7 = "data/input/wave_seven_lsype_young_person_2020.tab",
  w8 = "data/input/ns8_2015_self_completion.tab",
  w9 = "data/input/ns9_2022_main_interview.tab"
)

# Load datasets
w1 <- read_delim(files$w1, delim = "\t", col_types = cols(.default = col_guess()))
w4 <- read_delim(files$w4, delim = "\t", col_types = cols(.default = col_guess()))
w6 <- read_delim(files$w6, delim = "\t", col_types = cols(.default = col_guess()))
w7 <- read_delim(files$w7, delim = "\t", col_types = cols(.default = col_guess()))
w8 <- read_delim(files$w8, delim = "\t", col_types = cols(.default = col_guess()))
w9 <- read_delim(files$w9, delim = "\t", col_types = cols(.default = col_guess()))

# Merge all datasets by NSID using full_join
merged <- w1 %>%
  full_join(w4, by = "NSID") %>%
  full_join(w6, by = "NSID") %>%
  full_join(w7, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9, by = "NSID")

# Helper function to map sexuality values to standard codes
map_sex <- function(x, wave) {
  y <- as.numeric(x)
  # Keep valid responses 1-4 as is
  y <- case_when(
    y == 1 ~ 1,
    y == 2 ~ 2,
    y == 3 ~ 3,
    y == 4 ~ 4,
    TRUE ~ y
  )
  if (wave == "w6") {
    y <- case_when(
      y == -97 ~ -9,   # Respondent declined self completion
      y == -92 ~ -9,   # Refused
      y == -91 ~ -1,   # Not applicable
      y == -1 ~ -8,    # Don’t know
      TRUE ~ y
    )
  }
  if (wave == "w7") {
    y <- case_when(
      y == -100 ~ -9,  # Respondent declined sexual experience questions
      y == -97 ~ -9,   # Refused self completion
      y == -92 ~ -9,   # Refused
      y == -91 ~ -1,   # Not applicable
      y == -1 ~ -8,    # Don’t know
      TRUE ~ y
    )
  }
  if (wave == "w8") {
    y <- case_when(
      y == -9 ~ -9,
      y == -8 ~ -8,
      y == -1 ~ -1,
      TRUE ~ y
    )
  }
  if (wave == "w9") {
    y <- case_when(
      y == 5 ~ -7,    # Prefer not to say
      y == -9 ~ -9,
      y == -8 ~ -8,
      y == -3 ~ -3,   # Not asked at fieldwork stage
      y == -1 ~ -1,
      TRUE ~ y
    )
  }
  # Replace NA with -3 (not asked)
  y[is.na(y)] <- -3
  return(y)
}

# Derive the required sexual orientation variables
merged <- merged %>%
  mutate(
    sori19 = map_sex(W6SexualityYP, "w6"),
    sori20 = map_sex(W7SexualityYP, "w7"),
    sori25 = map_sex(W8SEXUALITY, "w8"),
    sori32 = map_sex(W9SORI, "w9")
  )

# Keep only final derived variables and ID
final_df <- merged %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write to CSV
write_csv(final_df, "data/output/cleaned_data.csv")

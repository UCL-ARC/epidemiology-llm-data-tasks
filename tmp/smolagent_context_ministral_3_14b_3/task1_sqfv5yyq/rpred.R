# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define the file paths
file_paths <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_two = "data/input/wave_two_lsype_young_person_2020.tab",
  wave_three = "data/input/wave_three_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_five = "data/input/wave_five_lsype_young_person_2020.tab",
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave_eight = "data/input/ns8_2015_main_interview.tab",
  wave_nine = "data/input/ns9_2022_main_interview.tab"
)

# Load each file into a separate object with explicit column types
wave_one <- read_delim(file_paths$wave_one, delim = "\t", col_types = cols(NSID = col_character(), W1sexYP = col_double()))
wave_two <- read_delim(file_paths$wave_two, delim = "\t", col_types = cols(NSID = col_character(), W2SexYP = col_double()))
wave_three <- read_delim(file_paths$wave_three, delim = "\t", col_types = cols(NSID = col_character(), W3sexYP = col_double()))
wave_four <- read_delim(file_paths$wave_four, delim = "\t", col_types = cols(NSID = col_character(), W4SexYP = col_double()))
wave_five <- read_delim(file_paths$wave_five, delim = "\t", col_types = cols(NSID = col_character(), W5SexYP = col_double()))
wave_six <- read_delim(file_paths$wave_six, delim = "\t", col_types = cols(NSID = col_character(), W6Sex = col_double()))
wave_seven <- read_delim(file_paths$wave_seven, delim = "\t", col_types = cols(NSID = col_character(), W7Sex = col_double()))
wave_eight <- read_delim(file_paths$wave_eight, delim = "\t", col_types = cols(NSID = col_character(), W8CMSEX = col_double()))
wave_nine <- read_delim(file_paths$wave_nine, delim = "\t", col_types = cols(NSID = col_character(), W9DSEX = col_double()))

# Function to harmonize missing values
harmonize_missing <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- -3
    x[x == -999] <- -2
    x[x == -998] <- -2
    x[x == -997] <- -2
    x[x == -995] <- -2
    x[x == -99] <- -3
    x[x == -92] <- -9
    x[x == -91] <- -1
    x[x == -9] <- -9
    x[x == -8] <- -8
    x[x == -1] <- -1
  }
  return(x)
}

# Harmonize missing values for each sex variable
wave_one$W1sexYP <- harmonize_missing(wave_one$W1sexYP)
wave_two$W2SexYP <- harmonize_missing(wave_two$W2SexYP)
wave_three$W3sexYP <- harmonize_missing(wave_three$W3sexYP)
wave_four$W4SexYP <- harmonize_missing(wave_four$W4SexYP)
wave_five$W5SexYP <- harmonize_missing(wave_five$W5SexYP)
wave_six$W6Sex <- harmonize_missing(wave_six$W6Sex)
wave_seven$W7Sex <- harmonize_missing(wave_seven$W7Sex)
wave_eight$W8CMSEX <- harmonize_missing(wave_eight$W8CMSEX)
wave_nine$W9DSEX <- harmonize_missing(wave_nine$W9DSEX)

# Merge all datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_five, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Derive the consolidated sex variable using most-recent-valid-first logic
merged_data <- merged_data %>%
  mutate(
    sex = coalesce(
      ifelse(W9DSEX %in% c(1, 2), W9DSEX, NA_real_),  # Use NA_real_ to ensure numeric NA
      ifelse(W8CMSEX %in% c(1, 2), W8CMSEX, NA_real_),
      ifelse(W7Sex %in% c(1, 2), W7Sex, NA_real_),
      ifelse(W6Sex %in% c(1, 2), W6Sex, NA_real_),
      ifelse(W5SexYP %in% c(1, 2), W5SexYP, NA_real_),
      ifelse(W4SexYP %in% c(1, 2), W4SexYP, NA_real_),
      ifelse(W3sexYP %in% c(1, 2), W3sexYP, NA_real_),
      ifelse(W2SexYP %in% c(1, 2), W2SexYP, NA_real_),
      ifelse(W1sexYP %in% c(1, 2), W1sexYP, NA_real_),
      NA_real_
    )
  )

# Convert sex to a labelled factor with explicit levels and labels
merged_data$sex <- factor(
  merged_data$sex,
  levels = c(1, 2, -9, -8, -7, -3, -2, -1),
  labels = c("Male", "Female", "Refusal", "Don't know", "Prefer not to say", "Not interviewed", "Not applicable", "Item not applicable")
)

# Select only NSID and the derived sex variable for the final output
cleaned_data <- merged_data %>%
  select(NSID, sex)

# Write the cleaned data to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv", na = "NA")

# Confirm the file was created
file.exists("data/output/cleaned_data.csv")

library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load data files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge all waves by NSID
df <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Standardize missing values for each wave's sex variable

# Wave 1 (Age 14)
df$W1sex <- df$W1sexYP
df$W1sex[df$W1sex == -99.0] <- -3  # YP not interviewed
df$W1sex[df$W1sex == -92.0] <- -9  # Refused
df$W1sex[df$W1sex == -91.0] <- -1  # Not applicable
df$W1sex[df$W1sex == -1.0] <- -8   # Don't Know

# Wave 2 (Age 15)
df$W2sex <- df$W2SexYP
df$W2sex[df$W2sex == -998.0] <- -2  # Interviewer missed question
df$W2sex[df$W2sex == -997.0] <- -2  # Script error
df$W2sex[df$W2sex == -995.0] <- -2  # Missing history section data
df$W2sex[df$W2sex == -99.0] <- -3   # YP not interviewed
df$W2sex[df$W2sex == -92.0] <- -9   # Refused
df$W2sex[df$W2sex == -91.0] <- -1   # Not applicable
df$W2sex[df$W2sex == -1.0] <- -8    # Don't Know

# Wave 3 (Age 16)
df$W3sex <- df$W3sexYP
df$W3sex[df$W3sex == -99.0] <- -3   # YP not interviewed
df$W3sex[df$W3sex == -92.0] <- -9   # Refused
df$W3sex[df$W3sex == -91.0] <- -1   # Not applicable

# Wave 4 (Age 17)
df$W4sex <- df$W4SexYP
df$W4sex[df$W4sex == -99.0] <- -3   # YP not interviewed
df$W4sex[df$W4sex == -92.0] <- -9   # Refused
df$W4sex[df$W4sex == -91.0] <- -1   # Not applicable
df$W4sex[df$W4sex == -1.0] <- -8    # Don't know

# Wave 5 (Age 18)
df$W5sex <- df$W5SexYP
df$W5sex[df$W5sex == -1.0] <- -8    # Don't know

# Wave 6 (Age 19)
df$W6sex <- df$W6Sex
df$W6sex[df$W6sex == -92.0] <- -9   # Refused
df$W6sex[df$W6sex == -91.0] <- -1   # Not applicable

# Wave 7 (Age 20)
df$W7sex <- df$W7Sex
df$W7sex[df$W7sex == -91.0] <- -1   # Not applicable

# Wave 8 (Age 25)
df$W8sex <- df$W8CMSEX
df$W8sex[df$W8sex == -9.0] <- -9    # Refused
df$W8sex[df$W8sex == -8.0] <- -8    # Don't know
df$W8sex[df$W8sex == -1.0] <- -1    # Not applicable

# Wave 9 (Age 32)
df$W9sex <- df$W9DSEX
# No missing values in this wave

# Create consolidated sex variable using earliest-valid-first rule (Wave 1 is earliest)
df$sex <- case_when(
  !is.na(df$W1sex) & df$W1sex %in% c(1, 2) ~ df$W1sex,
  !is.na(df$W2sex) & df$W2sex %in% c(1, 2) ~ df$W2sex,
  !is.na(df$W3sex) & df$W3sex %in% c(1, 2) ~ df$W3sex,
  !is.na(df$W4sex) & df$W4sex %in% c(1, 2) ~ df$W4sex,
  !is.na(df$W5sex) & df$W5sex %in% c(1, 2) ~ df$W5sex,
  !is.na(df$W6sex) & df$W6sex %in% c(1, 2) ~ df$W6sex,
  !is.na(df$W7sex) & df$W7sex %in% c(1, 2) ~ df$W7sex,
  !is.na(df$W8sex) & df$W8sex %in% c(1, 2) ~ df$W8sex,
  !is.na(df$W9sex) & df$W9sex %in% c(1, 2) ~ df$W9sex,
  TRUE ~ NA_real_
)

# Convert sex to labelled factor
df$sex <- as_factor(df$sex)
attributes(df$sex)$labels <- c(`1` = "Male", `2` = "Female", `-3` = "Not asked at the fieldwork stage / not interviewed",
                     `-9` = "Refused", 
                     `-8` = "Don't know / insufficient information", 
                     `-1` = "Item not applicable")

# Create final output with only ID and sex variable
output <- select(df, NSID, sex)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Script completed successfully. Output written to data/output/cleaned_data.csv\n")

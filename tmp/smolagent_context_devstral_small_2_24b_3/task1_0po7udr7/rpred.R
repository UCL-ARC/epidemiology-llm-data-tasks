library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(x, wave) {
  if (wave == "wave1") {
    x <- na_if(x, -99)
    x <- na_if(x, -92)
    x <- na_if(x, -91)
  } else if (wave == "wave2") {
    x <- na_if(x, -998)
    x <- na_if(x, -997)
    x <- na_if(x, -995)
    x <- na_if(x, -99)
    x <- na_if(x, -92)
    x <- na_if(x, -91)
    x <- na_if(x, -1)
  } else if (wave == "wave3") {
    x <- na_if(x, -99)
    x <- na_if(x, -92)
    x <- na_if(x, -91)
  } else if (wave == "wave4") {
    x <- na_if(x, -99)
    x <- na_if(x, -92)
    x <- na_if(x, -91)
    x <- na_if(x, -1)
  } else if (wave == "wave5") {
    x <- na_if(x, -1)
  } else if (wave == "wave6") {
    x <- na_if(x, -92)
    x <- na_if(x, -91)
  } else if (wave == "wave7") {
    x <- na_if(x, -91)
  } else if (wave == "wave8") {
    x <- na_if(x, -9)
    x <- na_if(x, -8)
    x <- na_if(x, -1)
  }
  return(x)
}

# Apply missing value mapping to each sex variable
wave1$W1sexYP <- map_missing(wave1$W1sexYP, "wave1")
wave2$W2SexYP <- map_missing(wave2$W2SexYP, "wave2")
wave3$W3sexYP <- map_missing(wave3$W3sexYP, "wave3")
wave4$W4SexYP <- map_missing(wave4$W4SexYP, "wave4")
wave5$W5SexYP <- map_missing(wave5$W5SexYP, "wave5")
wave6$W6Sex <- map_missing(wave6$W6Sex, "wave6")
wave7$W7Sex <- map_missing(wave7$W7Sex, "wave7")
wave8$W8CMSEX <- map_missing(wave8$W8CMSEX, "wave8")

# Derive the consolidated sex variable using most recent valid response first
merged_data <- merged_data %>%
  mutate(
    sex = case_when(
      !is.na(W9DSEX) ~ W9DSEX,
      !is.na(W8CMSEX) ~ W8CMSEX,
      !is.na(W7Sex) ~ W7Sex,
      !is.na(W6Sex) ~ W6Sex,
      !is.na(W5SexYP) ~ W5SexYP,
      !is.na(W4SexYP) ~ W4SexYP,
      !is.na(W3sexYP) ~ W3sexYP,
      !is.na(W2SexYP) ~ W2SexYP,
      !is.na(W1sexYP) ~ W1sexYP,
      TRUE ~ NA_integer_
    )
  )

# Convert NA to -3 for missing values
merged_data$sex <- ifelse(is.na(merged_data$sex), -3, merged_data$sex)

# Select only NSID and the derived sex variable
output_data <- merged_data %>%
  select(NSID, sex)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"

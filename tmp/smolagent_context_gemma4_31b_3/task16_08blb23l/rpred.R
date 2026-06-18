library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "numeric", NSID = "character"))
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "numeric", NSID = "character"))
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "numeric", NSID = "character"))
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "numeric", NSID = "character"))

# Merge datasets
full_frame <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Helper function for missing value mapping based on metadata and prompt
map_missing <- function(x, wave_age) {
  # 1. Specific mappings from prompt
  x <- case_when(
    x == -3 ~ -1, # "Not yet paid" (14-15) -> -1
    x == -1 ~ -8, # "Don't know" (14-15) -> -8
    x == -992 ~ -9, # "No information - refused" -> -9
    x == -996 & wave_age == 17 ~ -3, # "No parent in household" -> -3
    TRUE ~ x
  )
  
  # 2. Master Prompt Defaults (Label based mapping)
  x <- case_when(
    x %in% c(-999, -998, -997, -995) ~ -2,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -99 ~ -3,
    TRUE ~ x
  )
  
  # 3. Final NA to -3
  x[is.na(x)] <- -3
  return(x)
}

# Banding function for continuous income (14, 15) to match later bands
band_income <- function(x) {
  case_when(
    x < 50 ~ 1,
    x < 100 ~ 2,
    x < 200 ~ 3,
    x < 300 ~ 4,
    x < 400 ~ 5,
    x < 500 ~ 6,
    x < 600 ~ 7,
    x < 700 ~ 8,
    x < 800 ~ 9,
    x < 900 ~ 10,
    x < 1000 ~ 11,
    x >= 1000 ~ 12,
    TRUE ~ x
  )
}

# Process Waves
incwhhcnt14 <- map_missing(full_frame$W1GrsswkHH, 14)
incwhh14 <- case_when(
  incwhhcnt14 > 0 ~ band_income(incwhhcnt14),
  TRUE ~ incwhhcnt14
)

incwhhcnt15 <- map_missing(full_frame$W2GrsswkHH, 15)
incwhh15 <- case_when(
  incwhhcnt15 > 0 ~ band_income(incwhhcnt15),
  TRUE ~ incwhhcnt15
)

incwhh16 <- map_missing(full_frame$W3incestw, 16)
incwhh17 <- map_missing(full_frame$w4IncEstW, 17)

# Create factor labels
income_labels <- c(
  "1" = "Up to £49", "2" = "£50 up to £99", "3" = "£100 up to £199",
  "4" = "£200 up to £299", "5" = "£300 up to £399", "6" = "£400 up to £499",
  "7" = "£500 up to £599", "8" = "£600 up to £699", "9" = "£700 up to £799",
  "10" = "£800 up to £899", "11" = "£900 up to £999", "12" = "£1,000 or more",
  "-9" = "Refusal", "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say", "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost", "-1" = "Item not applicable"
)

# Final data assembly
final_df <- data.frame(NSID = full_frame$NSID)

final_df$incwhh14 <- factor(incwhh14, levels = as.numeric(names(income_labels)), labels = income_labels)
final_df$incwhh15 <- factor(incwhh15, levels = as.numeric(names(income_labels)), labels = income_labels)
final_df$incwhh16 <- factor(incwhh16, levels = as.numeric(names(income_labels)), labels = income_labels)
final_df$incwhh17 <- factor(incwhh17, levels = as.numeric(names(income_labels)), labels = income_labels)
final_df$incwhhcnt14 <- incwhhcnt14
final_df$incwhhcnt15 <- incwhhcnt15

# Use labelled::val_labels for continuous variables instead of set_labelled
missing_labels <- income_labels[names(income_labels) %in% c("-9", "-8", "-7", "-3", "-2", "-1")]
val_labels_list <- list(
  "-9" = missing_labels["-9"],
  "-8" = missing_labels["-8"],
  "-7" = missing_labels["-7"],
  "-3" = missing_labels["-3"],
  "-2" = missing_labels["-2"],
  "-1" = missing_labels["-1"]
)

final_df$incwhhcnt14 <- labelled::set_variable_labels(final_df$incwhhcnt14, label = "Gross weekly salary HH age 14")
# Note: Standard numeric columns in CSV won't preserve labelled factors, 
# but we ensure the values are correct as per requirements.

final_output <- final_df %>%
  select(NSID, incwhh14, incwhh15, incwhh16, incwhh17, incwhhcnt14, incwhhcnt15)

write_csv(final_output, "data/output/cleaned_data.csv")
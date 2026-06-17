library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define banding function for continuous income values
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
    TRUE ~ NA_real_
  )
}

# Load files - Correcting the na = NA issue by removing it
file_14 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", col_types = cols(NSID = col_character(), .default = "numeric"))
file_15 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = cols(NSID = col_character(), .default = "numeric"))
file_16 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = cols(NSID = col_character(), .default = "numeric"))
file_17 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = cols(NSID = col_character(), .default = "numeric"))

# Merge datasets
df <- file_14 %>%
  full_join(file_15, by = "NSID") %>%
  full_join(file_16, by = "NSID") %>%
  full_join(file_17, by = "NSID")

# Processing Function for Missing Values
process_missing <- function(val, wave) {
  # 1. Specific mappings from additional requirements
  if (wave %in% c(14, 15)) {
    val <- ifelse(val == -3, -1, val)
    val <- ifelse(val == -1, -8, val)
  }
  val <- ifelse(val == -992, -9, val)
  if (wave == 17) {
    val <- ifelse(val == -996, -3, val)
  }
  
  # Default Master Prompt mappings based on labels
  val <- ifelse(val == -99, -3, val)
  val <- ifelse(val == -92, -9, val)
  val <- ifelse(val == -91, -1, val)
  val <- ifelse(val == -94, -8, val)
  val <- ifelse(val == -999, -2, val)
  
  # 2. Remap remaining NAs to -3
  val[is.na(val)] <- -3
  
  return(val)
}

# Apply processing and derive variables
# Age 14
df <- df %>%
  mutate(
    incwhhcnt14 = process_missing(W1GrsswkHH, 14),
    incwhh14_temp = incwhhcnt14
  )
df$incwhh14_temp <- ifelse(df$incwhh14_temp > 0, band_income(df$incwhh14_temp), df$incwhh14_temp)

# Age 15
df <- df %>%
  mutate(
    incwhhcnt15 = process_missing(W2GrsswkHH, 15),
    incwhh15_temp = incwhhcnt15
  )
df$incwhh15_temp <- ifelse(df$incwhh15_temp > 0, band_income(df$incwhh15_temp), df$incwhh15_temp)

# Age 16
df <- df %>%
  mutate(
    incwhh16_temp = process_missing(W3incestw, 16)
  )

# Age 17
df <- df %>%
  mutate(
    incwhh17_temp = process_missing(w4IncEstW, 17)
  )

# Create final factor variables with labels
income_labels <- c(
  "1" = "Up to £49", "2" = "£50 up to £99", "3" = "£100 up to £199", 
  "4" = "£200 up to £299", "5" = "£300 up to £399", "6" = "£400 up to £499", 
  "7" = "£500 up to £599", "8" = "£600 up to £699", "9" = "£700 up to £799", 
  "10" = "£800 up to £899", "11" = "£900 up to £999", "12" = "£1,000 or more",
  "-9" = "Refusal", "-8" = "Don't know / insufficient information", "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed", 
  "-2" = "Schedule not applicable / script error / information lost", "-1" = "Item not applicable"
)

prepare_factor <- function(x) {
  x_char <- as.character(x)
  factor(x_char, levels = names(income_labels), labels = income_labels)
}

final_df <- df %>%
  transmute(
    NSID = NSID,
    incwhh14 = prepare_factor(incwhh14_temp),
    incwhh15 = prepare_factor(incwhh15_temp),
    incwhh16 = prepare_factor(incwhh16_temp),
    incwhh17 = prepare_factor(incwhh17_temp),
    incwhhcnt14 = incwhhcnt14,
    incwhhcnt15 = incwhhcnt15
  )

write_csv(final_df, "data/output/cleaned_data.csv")

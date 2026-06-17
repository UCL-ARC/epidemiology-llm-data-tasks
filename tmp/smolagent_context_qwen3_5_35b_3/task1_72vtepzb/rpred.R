library(readr)
library(dplyr)
library(tidyr)
library(haven)
library(labelled)

# Load all files
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", "\t")
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", "\t")
w3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", "\t")
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", "\t")
w5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", "\t")
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", "\t")
w7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", "\t")
w8 <- read_delim("data/input/ns8_2015_main_interview.tab", "\t")
w9 <- read_delim("data/input/ns9_2022_main_interview.tab", "\t")

# Merge all files
data <- full_join(w1, w2, by = "NSID")
data <- full_join(data, w3, by = "NSID")
data <- full_join(data, w4, by = "NSID")
data <- full_join(data, w5, by = "NSID")
data <- full_join(data, w6, by = "NSID")
data <- full_join(data, w7, by = "NSID")
data <- full_join(data, w8, by = "NSID")
data <- full_join(data, w9, by = "NSID")

# Create helper function to map missing values to standard codes
map_missing <- function(x) {
  case_when(
    x %in% c(-999, -998, -997, -995, -99) ~ -3L,
    x %in% c(-92) ~ -9L,
    x %in% c(-91) ~ -1L,
    x %in% c(-9) ~ -9L,
    x %in% c(-8) ~ -8L,
    x == -1 ~ -8L,
    TRUE ~ as.integer(x)
  )
}

# Standardize each wave's sex variable from the merged data
data$w9_dsex_std <- map_missing(data$W9DSEX)
data$w8_cmsx_std <- map_missing(data$W8CMSEX)
data$w7_sex_std <- map_missing(data$W7Sex)
data$w6_sex_std <- map_missing(data$W6Sex)
data$w5_sexyp_std <- map_missing(data$W5SexYP)
data$w4_sexyp_std <- map_missing(data$W4SexYP)
data$w3_sexyp_std <- map_missing(data$W3sexYP)
data$w2_sexyp_std <- map_missing(data$W2SexYP)
data$w1_sexyp_std <- map_missing(data$W1sexYP)

# Create consolidated sex variable
# Most recent valid first, then fall back through earlier waves
data$sex <- case_when(
  !is.na(data$w9_dsex_std) & data$w9_dsex_std %in% c(1L, 2L) ~ data$w9_dsex_std,
  !is.na(data$w8_cmsx_std) & data$w8_cmsx_std %in% c(1L, 2L) ~ data$w8_cmsx_std,
  !is.na(data$w7_sex_std) & data$w7_sex_std %in% c(1L, 2L) ~ data$w7_sex_std,
  !is.na(data$w6_sex_std) & data$w6_sex_std %in% c(1L, 2L) ~ data$w6_sex_std,
  !is.na(data$w5_sexyp_std) & data$w5_sexyp_std %in% c(1L, 2L) ~ data$w5_sexyp_std,
  !is.na(data$w4_sexyp_std) & data$w4_sexyp_std %in% c(1L, 2L) ~ data$w4_sexyp_std,
  !is.na(data$w3_sexyp_std) & data$w3_sexyp_std %in% c(1L, 2L) ~ data$w3_sexyp_std,
  !is.na(data$w2_sexyp_std) & data$w2_sexyp_std %in% c(1L, 2L) ~ data$w2_sexyp_std,
  !is.na(data$w1_sexyp_std) & data$w1_sexyp_std %in% c(1L, 2L) ~ data$w1_sexyp_std,
  TRUE ~ NA_integer_
)

# Keep only NSID and sex
output <- data %>% select(NSID, sex)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Script completed successfully.\n")
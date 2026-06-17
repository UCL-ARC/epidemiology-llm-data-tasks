library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets using NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Define the banding function for continuous income values
band_income <- function(inc) {
  ifelse(inc < 0, NA,
         ifelse(inc < 50, 1,
                ifelse(inc < 100, 2,
                       ifelse(inc < 200, 3,
                              ifelse(inc < 300, 4,
                                     ifelse(inc < 400, 5,
                                            ifelse(inc < 500, 6,
                                                   ifelse(inc < 600, 7,
                                                          ifelse(inc < 700, 8,
                                                                 ifelse(inc < 800, 9,
                                                                        ifelse(inc < 900, 10,
                                                                               ifelse(inc < 1000, 11, 12))))))))))))
}

# Process wave 1 (age 14)
incw1 <- merged_data$W1GrsswkHH
incw1 <- ifelse(incw1 == -3, -1, incw1)  # Not yet paid → -1
incw1 <- ifelse(incw1 == -1, -8, incw1)  # Don't know → -8
incw1 <- ifelse(incw1 == -992, -9, incw1)  # No information - refused → -9
incw1 <- ifelse(incw1 == -99, -3, incw1)  # HH not interviewed → -3
incw1 <- ifelse(incw1 == -94, -3, incw1)  # Insufficient information → -3
incw1 <- ifelse(incw1 == -92, -9, incw1)  # Refused → -9
incw1 <- ifelse(incw1 == -91, -1, incw1)  # Not applicable → -1
incw1 <- ifelse(is.na(incw1), -3, incw1)  # Remaining NAs → -3

# Continuous income for wave 1
incwhhcnt14 <- incw1
incwhhcnt14[incwhhcnt14 < 0] <- NA

# Banded income for wave 1
incwhh14 <- band_income(incw1)
incwhh14[incw1 < 0] <- incw1[incw1 < 0]

# Process wave 2 (age 15)
incw2 <- merged_data$W2GrsswkHH
incw2 <- ifelse(incw2 == -3, -1, incw2)  # Not yet paid → -1
incw2 <- ifelse(incw2 == -1, -8, incw2)  # Don't know → -8
incw2 <- ifelse(incw2 == -992, -9, incw2)  # No information - refused → -9
incw2 <- ifelse(incw2 == -99, -3, incw2)  # HH not interviewed → -3
incw2 <- ifelse(incw2 == -94, -3, incw2)  # Insufficient information → -3
incw2 <- ifelse(incw2 == -92, -9, incw2)  # Refused → -9
incw2 <- ifelse(incw2 == -91, -1, incw2)  # Not applicable → -1
incw2 <- ifelse(is.na(incw2), -3, incw2)  # Remaining NAs → -3

# Continuous income for wave 2
incwhhcnt15 <- incw2
incwhhcnt15[incwhhcnt15 < 0] <- NA

# Banded income for wave 2
incwhh15 <- band_income(incw2)
incwhh15[incw2 < 0] <- incw2[incw2 < 0]

# Process wave 3 (age 16)
incw3 <- merged_data$W3incestw
incw3 <- ifelse(incw3 == -99, -3, incw3)  # MP not interviewed → -3
incw3 <- ifelse(incw3 == -92, -9, incw3)  # Refused → -9
incw3 <- ifelse(incw3 == -1, -8, incw3)  # Don't know → -8
incw3 <- ifelse(is.na(incw3), -3, incw3)  # Remaining NAs → -3

# Banded income for wave 3
incwhh16 <- incw3
incwhh16[incw3 < 0] <- incw3[incw3 < 0]

# Process wave 4 (age 17)
incw4 <- merged_data$w4IncEstW
incw4 <- ifelse(incw4 == -996, -3, incw4)  # No parent in household → -3
incw4 <- ifelse(incw4 == -99, -3, incw4)  # MP not interviewed → -3
incw4 <- ifelse(incw4 == -92, -9, incw4)  # Refused → -9
incw4 <- ifelse(incw4 == -1, -8, incw4)  # Don't know → -8
incw4 <- ifelse(is.na(incw4), -3, incw4)  # Remaining NAs → -3

# Banded income for wave 4
incwhh17 <- incw4
incwhh17[incw4 < 0] <- incw4[incw4 < 0]

# Create the final dataset
final_data <- data.frame(
  NSID = merged_data$NSID,
  incwhh14 = incwhh14,
  incwhh15 = incwhh15,
  incwhh16 = incwhh16,
  incwhh17 = incwhh17,
  incwhhcnt14 = incwhhcnt14,
  incwhhcnt15 = incwhhcnt15
)

# Write the output
write_csv(final_data, "data/output/cleaned_data.csv")
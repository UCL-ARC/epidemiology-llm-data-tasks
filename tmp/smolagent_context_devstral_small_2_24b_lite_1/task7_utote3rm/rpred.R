library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define the mapping from source variables to NVQ levels
# Wave 4 (Age 17)
w4saim_to_nvq <- c(
  "1.0" = 3, "2.0" = 3, "3.0" = 3, "4.0" = 3,
  "5.0" = 2, "6.0" = 2, "7.0" = 2, "8.0" = 2,
  "9.0" = 1, "10.0" = 1, "11.0" = 1,
  "12.0" = NA, "13.0" = NA, "14.0" = NA
)

# Wave 6 (Age 19)
w6saim_to_nvq <- c(
  "1.0" = 5, "2.0" = 4, "3.0" = 4, "4.0" = 4,
  "5.0" = 3, "6.0" = 3, "7.0" = 3, "8.0" = 3,
  "9.0" = 2, "10.0" = 2, "11.0" = 2,
  "12.0" = 1, "13.0" = 1,
  "14.0" = NA, "15.0" = NA, "16.0" = NA
)

# Wave 7 (Age 20)
w7saim_to_nvq <- c(
  "1.0" = 1, "2.0" = 1,
  "3.0" = 2, "4.0" = 2, "5.0" = 2,
  "6.0" = 3, "7.0" = 3, "8.0" = 3, "9.0" = 3,
  "10.0" = 4, "11.0" = 4, "12.0" = 4,
  "13.0" = 5,
  "14.0" = NA
)

# Function to map source variables to NVQ levels
map_to_nvq <- function(var, mapping) {
  var_char <- as.character(var)
  mapped_values <- mapping[var_char]
  as.numeric(mapped_values)
}

# Derive NVQ levels for each wave
merged_data <- merged_data %>%
  mutate(
    educaim17 = map_to_nvq(w4saim, w4saim_to_nvq),
    educaim19 = map_to_nvq(W6Saim, w6saim_to_nvq),
    educaim20 = map_to_nvq(W7SAim, w7saim_to_nvq)
  )

# For Wave 8 (Age 25) and Wave 9 (Age 32), derive NVQ levels based on academic and vocational qualifications
merged_data <- merged_data %>%
  mutate(
    educaim25 = case_when(
      W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 | W8ACQUC0D == 1 | W8ACQUC0E == 1 ~ 5,
      W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1 ~ 4,
      W8ACQUC0L == 1 | W8ACQUC0M == 1 | W8ACQUC0N == 1 | W8ACQUC0O == 1 | W8ACQUC0P == 1 | W8ACQUC0Q == 1 ~ 3,
      W8VCQUC0A == 1 | W8VCQUC0B == 1 | W8VCQUC0C == 1 | W8VCQUC0D == 1 | W8VCQUC0E == 1 ~ 2,
      W8VCQUC0J == 1 | W8VCQUC0K == 1 ~ 1,
      TRUE ~ NA_real_
    ),
    educaim32 = case_when(
      W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 | W9ACQUC0D == 1 | W9ACQUC0E == 1 ~ 5,
      W9ACQUC0F == 1 | W9ACQUC0G == 1 | W9ACQUC0H == 1 | W9ACQUC0I == 1 | W9ACQUC0J == 1 | W9ACQUC0K == 1 ~ 4,
      W9ACQUC0L == 1 | W9ACQUC0M == 1 | W9ACQUC0N == 1 | W9ACQUC0O == 1 | W9ACQUC0P == 1 | W9ACQUC0Q == 1 ~ 3,
      W9VCQUC0A == 1 | W9VCQUC0B == 1 | W9VCQUC0C == 1 | W9VCQUC0D == 1 | W9VCQUC0E == 1 ~ 2,
      W9VCQUC0F == 1 | W9VCQUC0G == 1 | W9VCQUC0H == 1 | W9VCQUC0I == 1 | W9VCQUC0J == 1 | W9VCQUC0K == 1 ~ 1,
      TRUE ~ NA_real_
    )
  )

# Band NVQ levels to a common 6-category scheme
band_nvq <- function(nvq_level) {
  case_when(
    is.na(nvq_level) ~ NA_real_,
    nvq_level == 5 ~ 1,
    nvq_level == 4 ~ 2,
    nvq_level == 3 ~ 3,
    nvq_level == 2 ~ 4,
    nvq_level == 1 ~ 5,
    nvq_level == 0 ~ 6,
    TRUE ~ NA_real_
  )
}

# Apply banding to each wave
merged_data <- merged_data %>%
  mutate(
    educaim17 = band_nvq(educaim17),
    educaim19 = band_nvq(educaim19),
    educaim20 = band_nvq(educaim20),
    educaim25 = band_nvq(educaim25),
    educaim32 = band_nvq(educaim32)
  )

# Select only the ID variable and the derived variables
output_data <- merged_data %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write the output file
write_csv(output_data, "data/output/cleaned_data.csv")
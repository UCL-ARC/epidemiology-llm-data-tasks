library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

setwd('data')

# Load files
w1 <- read_delim('input/wave_one_lsype_young_person_2020.tab', '\t', show_col_types = FALSE)
w4 <- read_delim('input/wave_four_lsype_young_person_2020.tab', '\t', show_col_types = FALSE)
w5 <- read_delim('input/wave_five_lsype_young_person_2020.tab', '\t', show_col_types = FALSE)
w6 <- read_delim('input/wave_six_lsype_young_person_2020.tab', '\t', show_col_types = FALSE)
w7 <- read_delim('input/wave_seven_lsype_young_person_2020.tab', '\t', show_col_types = FALSE)
w8 <- read_delim('input/ns8_2015_derived.tab', '\t', show_col_types = FALSE)
w9 <- read_delim('input/ns9_2022_derived_variables.tab', '\t', show_col_types = FALSE)

# Full join
full <- w1
full <- full_join(full, w4, by = 'NSID')
full <- full_join(full, w5, by = 'NSID')
full <- full_join(full, w6, by = 'NSID')
full <- full_join(full, w7, by = 'NSID')
full <- full_join(full, w8, by = 'NSID')
full <- full_join(full, w9, by = 'NSID')

# Function to set missing codes
set_miss <- function(x, code = -3) {
  x[is.na(x)] <- code
  x
}

# Wave 4 (age 17) - W4empsYP to 6 categories
ecoact17 <- as.numeric(full$W4empsYP)
ecoact17[ecoact17 %in% c(1,2,3)] <- 1  # work/unemployed
ecoact17[ecoact17 %in% c(4,5)] <- 4    # training/education
ecoact17[ecoact17 %in% c(6,7,8,9)] <- 5  # other
ecoact17 <- set_miss(ecoact17)

# Wave 5 (age 18) - W5mainactYP to 6 categories
ecoact18 <- as.numeric(full$W5mainactYP)
ecoact18[ecoact18 %in% c(3)] <- 1
ecoact18[ecoact18 %in% c(1,5,6,2,4)] <- 4
ecoact18[ecoact18 %in% c(7)] <- 1
ecoact18[ecoact18 %in% c(8,9,10,11)] <- 5
ecoact18 <- set_miss(ecoact18)

# Wave 6 (age 19) - W6TCurrentAct to 6 categories
ecoact19 <- as.numeric(full$W6TCurrentAct)
ecoact19[ecoact19 %in% c(3)] <- 1
ecoact19[ecoact19 %in% c(1,2,4,5)] <- 4
ecoact19[ecoact19 %in% c(8)] <- 1
ecoact19[ecoact19 %in% c(6,7,9,10,11)] <- 5
ecoact19 <- set_miss(ecoact19)

# Wave 7 (age 20) - W7TCurrentAct to 6 categories
ecoact20 <- as.numeric(full$W7TCurrentAct)
ecoact20[ecoact20 %in% c(3)] <- 1
ecoact20[ecoact20 %in% c(1,2,4,5)] <- 4
ecoact20[ecoact20 %in% c(8)] <- 1
ecoact20[ecoact20 %in% c(6,7,9,10,11,12,13,14,15)] <- 5
ecoact20 <- set_miss(ecoact20)

# Wave 8 (age 25) - W8DACTIVITYC collapsed and detailed
ecoact25 <- as.numeric(full$W8DACTIVITYC)
ecoact25[ecoact25 %in% c(1,2,4)] <- 1  # work/unemployed
ecoact25[ecoact25 %in% c(5)] <- 4
ecoact25[ecoact25 %in% c(6,7)] <- 4
ecoact25[ecoact25 %in% c(3,8,9,10)] <- 5
ecoact25 <- set_miss(ecoact25)

ecoactadu25 <- as.numeric(full$W8DACTIVITYC)
ecoactadu25[ecoactadu25 %in% c(-9,-8,-1)] <- -3
ecoactadu25 <- as.numeric(ecoactadu25)
ecoactadu25[is.na(ecoactadu25)] <- -3

# Wave 9 (age 32) - W9DACTIVITYC collapsed and detailed
ecoact32 <- as.numeric(full$W9DACTIVITYC)
ecoact32[ecoact32 %in% c(1,2,4)] <- 1
ecoact32[ecoact32 %in% c(5)] <- 4
ecoact32[ecoact32 %in% c(6,7)] <- 4
ecoact32[ecoact32 %in% c(3,8,9,10)] <- 5
ecoact32 <- set_miss(ecoact32)

ecoactadu32 <- as.numeric(full$W9DACTIVITYC)
ecoactadu32[ecoactadu32 %in% c(-9,-8,-1)] <- -3
ecoactadu32 <- as.numeric(ecoactadu32)
ecoactadu32[is.na(ecoactadu32)] <- -3

full$ecoact17 <- ecoact17
full$ecoact18 <- ecoact18
full$ecoact19 <- ecoact19
full$ecoact20 <- ecoact20
full$ecoact25 <- ecoact25
full$ecoact32 <- ecoact32
full$ecoactadu25 <- ecoactadu25
full$ecoactadu32 <- ecoactadu32

write_csv(full, 'output/cleaned_data.csv')
cat('Done')

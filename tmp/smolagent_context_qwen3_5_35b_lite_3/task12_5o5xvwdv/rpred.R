library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(haven)
library(labelled)

# Load all data files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID
all_data <- full_join(wave1, wave4, by = "NSID")
all_data <- full_join(all_data, wave5, by = "NSID")
all_data <- full_join(all_data, wave6, by = "NSID")
all_data <- full_join(all_data, wave7, by = "NSID")
all_data <- full_join(all_data, wave8, by = "NSID")
all_data <- full_join(all_data, wave9, by = "NSID")

# Function to recode NS-SEC to major categories
recode_nssec <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 1,
    x %in% c(3.1, 3.2, 3.3, 3.4) ~ 1,
    x %in% c(4.1, 4.2, 4.3, 4.4) ~ 1,
    x == 5 ~ 2,
    x == 6 ~ 2,
    x %in% c(7.1, 7.2, 7.3, 7.4) ~ 3,
    x %in% c(8.1, 8.2) ~ 4,
    x %in% c(9.1, 9.2) ~ 4,
    x == 10 ~ 5,
    x %in% c(11.1, 11.2) ~ 5,
    x %in% c(12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7) ~ 6,
    x %in% c(13.1, 13.2, 13.3, 13.4, 13.5) ~ 7,
    x %in% c(14.1, 14.2, 14.3) ~ 8,
    x == 15 ~ 9,
    x == 16 ~ 10,
    x == 17 ~ 10,
    TRUE ~ NA_real_
  )
}

# Age 17 (Wave 4)
all_data$nssec17 <- recode_nssec(all_data$W4nsseccatYP)
all_data$nssec17[all_data$W4nsseccatYP == -91] <- -1
all_data$nssec17[all_data$W4nsseccatYP == -99] <- -3
all_data$nssec17[is.na(all_data$nssec17)] <- -3

# Age 18 (Wave 5)
all_data$nssec18 <- recode_nssec(all_data$W5nsseccatYP)
all_data$nssec18[all_data$W5nsseccatYP == -91] <- -1
all_data$nssec18[is.na(all_data$nssec18)] <- -3

# Age 19 (Wave 6)
all_data$nssec19 <- recode_nssec(all_data$w6nsseccatYP)
all_data$nssec19[all_data$w6nsseccatYP == -91] <- -1
all_data$nssec19[is.na(all_data$nssec19)] <- -3

# Age 20 (Wave 7)
all_data$nssec20 <- recode_nssec(all_data$W7NSSECCat)
all_data$nssec20[all_data$W7NSSECCat == -91] <- -1
all_data$nssec20[is.na(all_data$nssec20)] <- -3

# Age 25 (Wave 8)
all_data$nssec25 <- recode_nssec(all_data$W8DNSSEC17)
all_data$nssec25[all_data$W8DNSSEC17 == -9] <- -9
all_data$nssec25[all_data$W8DNSSEC17 == -8] <- -8
all_data$nssec25[all_data$W8DNSSEC17 == -1] <- -1
all_data$nssec25[is.na(all_data$nssec25)] <- -3

# Age 32 (Wave 9)
all_data$nssec32 <- recode_nssec(all_data$W9NSSEC)
all_data$nssec32[all_data$W9NSSEC == -9] <- -9
all_data$nssec32[all_data$W9NSSEC == -8] <- -8
all_data$nssec32[all_data$W9NSSEC == -1] <- -1
all_data$nssec32[is.na(all_data$nssec32)] <- -3

# Select only NSID and the final derived variables
output_data <- all_data %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write output to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

print("Data cleaning complete!")
print(paste("Output file has", nrow(output_data), "rows and", ncol(output_data), "columns"))

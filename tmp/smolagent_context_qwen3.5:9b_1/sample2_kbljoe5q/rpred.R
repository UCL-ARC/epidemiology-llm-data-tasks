library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

input_dir <- "data/input"
output_file <- "data/output/cleaned_data.csv"

if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

wave1 <- read_delim(paste0(input_dir, "/wave_one_lsype_young_person_2020.tab"), delim = "\t")
wave2 <- read_delim(paste0(input_dir, "/wave_two_lsype_young_person_2020.tab"), delim = "\t")
wave4 <- read_delim(paste0(input_dir, "/wave_four_lsype_young_person_2020.tab"), delim = "\t")
wave8 <- read_delim(paste0(input_dir, "/ns8_2015_derived.tab"), delim = "\t")
wave9 <- read_delim(paste0(input_dir, "/ns9_2022_derived_variables.tab"), delim = "\t")

wave1$W1ethnic2YP <- ifelse(is.na(wave1$W1ethnic2YP), -3, wave1$W1ethnic2YP)
wave1$W1ethnic2YP <- ifelse(wave1$W1ethnic2YP %in% c(-999, -92), -9, wave1$W1ethnic2YP)
wave1$W1ethnic2YP <- ifelse(wave1$W1ethnic2YP %in% c(-94, -1), -8, wave1$W1ethnic2YP)

wave2$W2ethnicYP <- ifelse(is.na(wave2$W2ethnicYP), -3, wave2$W2ethnicYP)
wave2$W2ethnicYP <- ifelse(wave2$W2ethnicYP %in% c(-998, -997, -995, -99), -3, wave2$W2ethnicYP)
wave2$W2ethnicYP <- ifelse(wave2$W2ethnicYP %in% c(-92), -9, wave2$W2ethnicYP)
wave2$W2ethnicYP <- ifelse(wave2$W2ethnicYP %in% c(-91, -1), -8, wave2$W2ethnicYP)

wave4$w4ethnic2YP <- ifelse(is.na(wave4$w4ethnic2YP), -3, wave4$w4ethnic2YP)
wave4$w4ethnic2YP <- ifelse(wave4$w4ethnic2YP %in% c(-999, -91), -9, wave4$w4ethnic2YP)
wave4$w4ethnic2YP <- ifelse(wave4$w4ethnic2YP %in% c(-94, -1), -8, wave4$w4ethnic2YP)

wave8$W8DETHN15 <- ifelse(is.na(wave8$W8DETHN15), -3, wave8$W8DETHN15)
wave9$W9DETHN15 <- ifelse(is.na(wave9$W9DETHN15), -3, wave9$W9DETHN15)

full_data <- full_join(wave1, wave2, by = "NSID")
full_data <- full_join(full_data, wave4, by = "NSID")
full_data <- full_join(full_data, wave8, by = "NSID")
full_data <- full_join(full_data, wave9, by = "NSID")

full_data$eth <- ifelse(!is.na(full_data$W1ethnic2YP) & full_data$W1ethnic2YP >= 0, full_data$W1ethnic2YP,
  ifelse(!is.na(full_data$W2ethnicYP) & full_data$W2ethnicYP >= 0, full_data$W2ethnicYP,
  ifelse(!is.na(full_data$w4ethnic2YP) & full_data$w4ethnic2YP >= 0, full_data$w4ethnic2YP,
  ifelse(!is.na(full_data$W8DETHN15) & full_data$W8DETHN15 >= 0, full_data$W8DETHN15,
  ifelse(!is.na(full_data$W9DETHN15) & full_data$W9DETHN15 >= 0, full_data$W9DETHN15, -3)))))

write_csv(full_data, output_file)
print("Done")
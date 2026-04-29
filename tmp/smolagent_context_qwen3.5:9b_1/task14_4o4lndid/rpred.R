library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

rm(list = ls())

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Recode missing values
wave1$W1hous12HH <- ifelse(wave1$W1hous12HH == -999, -3, ifelse(wave1$W1hous12HH == -92, -1, ifelse(wave1$W1hous12HH == -91, -3, ifelse(wave1$W1hous12HH == -1, -8, wave1$W1hous12HH))))

wave2$W2Hous12HH <- ifelse(wave2$W2Hous12HH == -998, -3, ifelse(wave2$W2Hous12HH == -997, -2, ifelse(wave2$W2Hous12HH == -995, -3, ifelse(wave2$W2Hous12HH == -999, -3, ifelse(wave2$W2Hous12HH == -92, -1, ifelse(wave2$W2Hous12HH == -91, -3, ifelse(wave2$W2Hous12HH == -1, -8, wave2$W2Hous12HH)))))))

wave3$W3hous12HH <- ifelse(wave3$W3hous12HH == -999, -3, ifelse(wave3$W3hous12HH == -99, -3, ifelse(wave3$W3hous12HH == -92, -1, ifelse(wave3$W3hous12HH == -91, -3, ifelse(wave3$W3hous12HH == -1, -8, wave3$W3hous12HH)))))

wave4$W4Hous12HH <- ifelse(wave4$W4Hous12HH == -999, -3, ifelse(wave4$W4Hous12HH == -997, -2, ifelse(wave4$W4Hous12HH == -92, -1, ifelse(wave4$W4Hous12HH == -91, -3, ifelse(wave4$W4Hous12HH == -1, -8, wave4$W4Hous12HH)))))

wave5$W5Hous12HH <- ifelse(wave5$W5Hous12HH == -999, -3, ifelse(wave5$W5Hous12HH == -92, -1, ifelse(wave5$W5Hous12HH == -91, -3, ifelse(wave5$W5Hous12HH == -1, -8, wave5$W5Hous12HH))))
wave5$W5Hous12BHH <- ifelse(wave5$W5Hous12BHH == -999, -3, ifelse(wave5$W5Hous12BHH == -92, -1, ifelse(wave5$W5Hous12BHH == -91, -3, ifelse(wave5$W5Hous12BHH == -1, -8, wave5$W5Hous12BHH))))
wave5$W5Hous12CHH <- ifelse(wave5$W5Hous12CHH == -999, -3, ifelse(wave5$W5Hous12CHH == -92, -1, ifelse(wave5$W5Hous12CHH == -91, -3, ifelse(wave5$W5Hous12CHH == -1, -8, wave5$W5Hous12CHH))))

wave6$W6Hous12YP <- ifelse(wave6$W6Hous12YP == -999, -3, ifelse(wave6$W6Hous12YP == -92, -1, ifelse(wave6$W6Hous12YP == -91, -3, ifelse(wave6$W6Hous12YP == -1, -8, wave6$W6Hous12YP))))
wave6$W6Hous12bYP <- ifelse(wave6$W6Hous12bYP == -999, -3, ifelse(wave6$W6Hous12bYP == -92, -1, ifelse(wave6$W6Hous12bYP == -91, -3, ifelse(wave6$W6Hous12bYP == -1, -8, wave6$W6Hous12bYP))))
wave6$W6Hous12cYP <- ifelse(wave6$W6Hous12cYP == -999, -3, ifelse(wave6$W6Hous12cYP == -92, -1, ifelse(wave6$W6Hous12cYP == -91, -3, ifelse(wave6$W6Hous12cYP == -1, -8, wave6$W6Hous12cYP))))

wave7$W7Hous12YP <- ifelse(wave7$W7Hous12YP == -999, -3, ifelse(wave7$W7Hous12YP == -92, -1, ifelse(wave7$W7Hous12YP == -91, -3, ifelse(wave7$W7Hous12YP == -1, -8, wave7$W7Hous12YP))))
wave7$W7Hous12bYP <- ifelse(wave7$W7Hous12bYP == -999, -3, ifelse(wave7$W7Hous12bYP == -92, -1, ifelse(wave7$W7Hous12bYP == -91, -3, ifelse(wave7$W7Hous12bYP == -1, -8, wave7$W7Hous12bYP))))
wave7$W7Hous12cYP <- ifelse(wave7$W7Hous12cYP == -999, -3, ifelse(wave7$W7Hous12cYP == -92, -1, ifelse(wave7$W7Hous12cYP == -91, -3, ifelse(wave7$W7Hous12cYP == -1, -8, wave7$W7Hous12cYP))))

wave8$W8TENURE <- ifelse(wave8$W8TENURE == -9, -1, ifelse(wave8$W8TENURE == -8, -8, ifelse(wave8$W8TENURE == -1, -3, wave8$W8TENURE)))

wave9$W9DTENURE <- ifelse(wave9$W9DTENURE == -8, -8, wave9$W9DTENURE)

# Merge all datasets
master <- full_join(wave1, wave2, by = "NSID")
master <- full_join(master, wave3, by = "NSID")
master <- full_join(master, wave4, by = "NSID")
master <- full_join(master, wave5, by = "NSID")
master <- full_join(master, wave6, by = "NSID")
master <- full_join(master, wave7, by = "NSID")
master <- full_join(master, wave8, by = "NSID")
master <- full_join(master, wave9, by = "NSID")

# Create detailed adolescent variables
master$hownteen14 <- factor(master$W1hous12HH, levels = c(-999, -92, -91, -1, 1, 2, 3, 4, 5, 6, 7), labels = c("Not asked at the fieldwork stage/participated/interviewed", "Refusal", "Don't know/insufficient information", "Not applicable", "Owned outright", "Being bought on a mortgage/bank loan", "Shared ownership (owns & rents property)", "Rented from a Council or New Town", "Rented from a Housing Association", "Rented privately", "Rent free"))

master$hownteen15 <- factor(master$W2Hous12HH, levels = c(-998, -997, -995, -999, -92, -91, -1, 1, 2, 3, 4, 5, 6, 7, 8), labels = c("Not asked at the fieldwork stage/participated/interviewed", "Script error/information lost", "Not asked at the fieldwork stage/participated/interviewed", "Not asked at the fieldwork stage/participated/interviewed", "Refusal", "Don't know/insufficient information", "Not applicable", "Owned outright", "Being bought on a mortgage/bank loan", "Shared ownership (owns & rents property)", "Rented from a Council or New Town", "Rented from a Housing Association", "Rented privately", "Rent free", "Some other arrangement"))

master$hownteen16 <- factor(master$W3hous12HH, levels = c(-999, -99, -92, -91, -1, 1, 2, 3, 4, 5, 6, 7), labels = c("Not asked at the fieldwork stage/participated/interviewed", "Not asked at the fieldwork stage/participated/interviewed", "Refusal", "Don't know/insufficient information", "Not applicable", "Owned outright", "Being bought on a mortgage/ bank loan", "Shared ownership (owns & rents property)", "Rented from a Council or New Town", "Rented from a Housing Association", "Rented privately", "Rent free"))

master$hownteen17 <- factor(master$W4Hous12HH, levels = c(-999, -997, -92, -91, -1, 1, 2, 3, 4, 5, 6, 7, 8), labels = c("Not asked at the fieldwork stage/participated/interviewed", "Script error/information lost", "Refusal", "Don't know/insufficient information", "Not applicable", "Owned outright", "Being bought on a mortgage/ bank loan", "Shared ownership (owns & rents property)", "Rented from a Council or New Town", "Rented from a Housing Association", "Rented privately", "Rent free", "Some other arrangement"))

# Age 18: combine type + subtype
# Get the combined value
combined <- ifelse(master$W5Hous12HH == 1, master$W5Hous12BHH, ifelse(master$W5Hous12HH == 2, master$W5Hous12CHH, master$W5Hous12HH))

# Level mapping: -999, -92, -91, -1, 1, 2, 3, 4, 5
master$hownteen18 <- factor(combined, levels = c(-999, -92, -91, -1, 1, 2, 3, 4, 5), labels = c("Not asked at the fieldwork stage/participated/interviewed", "Refusal", "Not applicable", "Don't know/insufficient information", "Owned outright", "Being bought on a mortgage/ bank loan", "Shared ownership (owns and rents the property)", "Some other arrangement", "Rented from a Council or New town"))

# Create collapsed variables
master$hown14_20 <- master$hownteen17

master$hown25 <- factor(master$W8TENURE, levels = c(-9, -8, -1, 1, 2, 3, 4, 5, 6, 7), labels = c("Refusal", "Don't know/insufficient information", "Not applicable", "Owned outright", "Owned, buying with help of mortgage/loan", "Part rent, part mortgage", "Rent it", "live rent-free", "Other", "Script error/information lost"))

master$hown32 <- factor(master$W9DTENURE, levels = c(-8, 1, 2, 3, 4, 5, 6, 7), labels = c("Don't know/insufficient information", "Owned outright", "Owned, buying with help of mortgage/loan", "Part rent, part mortgage", "Rent it", "live rent-free", "Other", "Script error/information lost"))

# Select final variables
final_vars <- c("NSID", "hown14_20", "hown25", "hown32", "hownteen14", "hownteen15", "hownteen16", "hownteen17", "hownteen18")

cleaned_data <- master %>% select(all_of(final_vars))

write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)
print("Processing complete!")
print(dim(cleaned_data))

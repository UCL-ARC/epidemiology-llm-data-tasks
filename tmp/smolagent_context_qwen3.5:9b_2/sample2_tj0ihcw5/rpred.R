library(haven)
library(dplyr)
library(purrr)
library(readr)
library(labelled)

# Get base directory
cwd <- getwd()
cat("Working directory:", cwd, "\n")

# Define standard missing value codes:
# -9 = Refusal
# -8 = Don't know/insufficient information
# -1 = Item not applicable
# -3 = Not asked at the fieldwork stage/participated/interviewed

# Load the data files from data/input/
wave_1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")

wave_2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")

wave_4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")

wave_8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")

wave_9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

cat("Loaded all datasets.\n")

# Harmonize missing values for Wave 1 (Age 14)
# -999: Missing - household data lost -> -3
# -94: Insufficient information -> -8
# -92: Refused -> -9
# -91: Not applicable -> -1
# -1: Don't know -> -8
wave_1$W1ethnic2YP <- ifelse(wave_1$W1ethnic2YP == -999, -3,
                             ifelse(wave_1$W1ethnic2YP == -94, -8,
                                     ifelse(wave_1$W1ethnic2YP == -92, -9,
                                             ifelse(wave_1$W1ethnic2YP == -91, -1,
                                                     ifelse(wave_1$W1ethnic2YP == -1, -8,
                                                             wave_1$W1ethnic2YP)))))

# Harmonize missing values for Wave 2 (Age 15)
# -998: Interviewer missed question -> -3
# -997: Script error -> -3
# -995: Missing history section data -> -3
# -99: YP not interviewed -> -3
# -92: Refused -> -9
# -91: Not applicable -> -1
# -1: Don't Know -> -8
wave_2$W2ethnicYP <- ifelse(wave_2$W2ethnicYP %in% c(-998, -997, -995, -99), -3,
                             ifelse(wave_2$W2ethnicYP == -92, -9,
                                     ifelse(wave_2$W2ethnicYP == -91, -1,
                                             ifelse(wave_2$W2ethnicYP == -1, -8,
                                                     wave_2$W2ethnicYP))))

# Harmonize missing values for Wave 4 (Age 17)
# -94: Insufficient information -> -8
# -1: Don't know -> -8
wave_4$w4ethnic2YP <- ifelse(wave_4$w4ethnic2YP == -94, -8,
                              ifelse(wave_4$w4ethnic2YP == -1, -8,
                                      wave_4$w4ethnic2YP))

# Harmonize missing values for Wave 8
# -9: Refused -> -9
# -8: Insufficient information -> -8
# -1: Not applicable -> -1
wave_8$W8DETHN15 <- ifelse(wave_8$W8DETHN15 == -9, -9,
                            ifelse(wave_8$W8DETHN15 == -8, -8,
                                    ifelse(wave_8$W8DETHN15 == -1, -1,
                                            wave_8$W8DETHN15)))

# Harmonize missing values for Wave 9
# -8: Insufficient information -> -8
wave_9$W9DETHN15 <- ifelse(wave_9$W9DETHN15 == -8, -8,
                            wave_9$W9DETHN15)

# Now merge all datasets by NSID
all_data <- full_join(wave_1, wave_2, by = "NSID")
cat("After joining wave 1 and wave 2\n")
all_data <- full_join(all_data, wave_4, by = "NSID")
cat("After joining wave 4\n")
all_data <- full_join(all_data, wave_8, by = "NSID")
cat("After joining wave 8\n")
all_data <- full_join(all_data, wave_9, by = "NSID")
cat("After joining wave 9\n")

# For ethnic group variable, prioritize earliest valid response
all_data <- all_data %>%
  mutate(
    eth_final = case_when(
      is.na(W1ethnic2YP) | W1ethnic2YP < 0 ~ NA_real_,
      W1ethnic2YP > 0 ~ W1ethnic2YP,
      is.na(W2ethnicYP) | W2ethnicYP < 0 ~ NA_real_,
      W2ethnicYP > 0 ~ W2ethnicYP,
      is.na(w4ethnic2YP) | w4ethnic2YP < 0 ~ NA_real_,
      w4ethnic2YP > 0 ~ w4ethnic2YP,
      is.na(W8DETHN15) | W8DETHN15 < 0 ~ NA_real_,
      W8DETHN15 > 0 ~ W8DETHN15,
      is.na(W9DETHN15) | W9DETHN15 < 0 ~ NA_real_,
      W9DETHN15 > 0 ~ W9DETHN15,
      TRUE ~ NA_real_
    )
  )

final_eth <- all_data %>% select(NSID, eth_final)

# Output the cleaned data
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(final_eth, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")
cat("Number of observations:", nrow(final_eth), "\n")
cat("Number of variables:", ncol(final_eth), "\n")

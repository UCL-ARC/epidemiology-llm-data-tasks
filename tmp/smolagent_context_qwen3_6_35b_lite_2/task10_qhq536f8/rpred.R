library(dplyr)
library(readr)
library(tidyr)
library(haven)
library(labelled)

# Load files
sweep1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
sweep4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
sweep5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
sweep6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
sweep7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
sweep8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
sweep9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all files
data <- full_join(sweep1, sweep4, by = "NSID") %>%
  full_join(sweep5, by = "NSID") %>%
  full_join(sweep6, by = "NSID") %>%
  full_join(sweep7, by = "NSID") %>%
  full_join(sweep8, by = "NSID") %>%
  full_join(sweep9, by = "NSID")

# Helper function to create labelled vector with numeric labels
make_labelled <- function(x, labels_char) {
  labels_num <- as.numeric(names(labels_char))
  haven::labelled(x, labels = stats::setNames(labels_num, labels_num))
}

# Harmonised 6-category labels
ecoact_labels <- c(`1` = "Employed", `2` = "Self-employed",
                   `3` = "Education", `4` = "Training/Apprenticeship",
                   `5` = "Unemployed", `6` = "Other inactive",
                   `-1` = "Not applicable", `-2` = "Schedule not applicable",
                   `-3` = "Not asked", `-8` = "Insufficient information",
                   `-9` = "Refused")

# Age 17: W4empsYP
ecoact17 <- data$W4empsYP
ecoact17[ecoact17 == 1] <- 1
ecoact17[ecoact17 == 2] <- 1
ecoact17[ecoact17 == 3] <- 5
ecoact17[ecoact17 == 4] <- 4
ecoact17[ecoact17 == 5] <- 3
ecoact17[ecoact17 == 6] <- 6
ecoact17[ecoact17 == 7] <- 6
ecoact17[ecoact17 == 8] <- 6
ecoact17[ecoact17 == 9] <- 6
ecoact17[ecoact17 == -999] <- -2
ecoact17[ecoact17 == -94] <- -8
ecoact17[ecoact17 == -92] <- -9
ecoact17[ecoact17 == -91] <- -1
ecoact17[is.na(ecoact17)] <- -3
data$ecoact17 <- make_labelled(ecoact17, ecoact_labels)

# Age 18: W5mainactYP
ecoact18 <- data$W5mainactYP
ecoact18[ecoact18 == 1] <- 4
ecoact18[ecoact18 == 2] <- 4
ecoact18[ecoact18 == 3] <- 1
ecoact18[ecoact18 == 4] <- 3
ecoact18[ecoact18 == 5] <- 4
ecoact18[ecoact18 == 6] <- 4
ecoact18[ecoact18 == 7] <- 5
ecoact18[ecoact18 == 8] <- 6
ecoact18[ecoact18 == 9] <- 6
ecoact18[ecoact18 == 10] <- 6
ecoact18[ecoact18 == 11] <- 6
ecoact18[ecoact18 == -94] <- -8
ecoact18[is.na(ecoact18)] <- -3
data$ecoact18 <- make_labelled(ecoact18, ecoact_labels)

# Age 19: W6TCurrentAct
ecoact19 <- data$W6TCurrentAct
ecoact19[ecoact19 == 1] <- 3
ecoact19[ecoact19 == 2] <- 3
ecoact19[ecoact19 == 3] <- 1
ecoact19[ecoact19 == 4] <- 4
ecoact19[ecoact19 == 5] <- 4
ecoact19[ecoact19 == 6] <- 6
ecoact19[ecoact19 == 7] <- 6
ecoact19[ecoact19 == 8] <- 5
ecoact19[ecoact19 == 9] <- 6
ecoact19[ecoact19 == 10] <- 4
ecoact19[ecoact19 == 11] <- 6
ecoact19[ecoact19 == -91] <- -2
ecoact19[is.na(ecoact19)] <- -3
data$ecoact19 <- make_labelled(ecoact19, ecoact_labels)

# Age 20: W7TCurrentAct
ecoact20 <- data$W7TCurrentAct
ecoact20[ecoact20 == 1] <- 3
ecoact20[ecoact20 == 2] <- 3
ecoact20[ecoact20 == 3] <- 1
ecoact20[ecoact20 == 4] <- 4
ecoact20[ecoact20 == 5] <- 4
ecoact20[ecoact20 == 6] <- 6
ecoact20[ecoact20 == 7] <- 6
ecoact20[ecoact20 == 8] <- 5
ecoact20[ecoact20 == 9] <- 4
ecoact20[ecoact20 == 10] <- 6
ecoact20[ecoact20 == 11] <- 4
ecoact20[ecoact20 == 12] <- 6
ecoact20[ecoact20 == 13] <- 6
ecoact20[ecoact20 == 14] <- 6
ecoact20[ecoact20 == 15] <- 6
ecoact20[ecoact20 == -91] <- -1
ecoact20[is.na(ecoact20)] <- -3
data$ecoact20 <- make_labelled(ecoact20, ecoact_labels)

# Age 25: W8DACTIVITYC
ecoact25 <- data$W8DACTIVITYC
ecoact25[ecoact25 == 1] <- 1
ecoact25[ecoact25 == 2] <- 2
ecoact25[ecoact25 == 3] <- 6
ecoact25[ecoact25 == 4] <- 5
ecoact25[ecoact25 == 5] <- 3
ecoact25[ecoact25 == 6] <- 4
ecoact25[ecoact25 == 7] <- 4
ecoact25[ecoact25 == 8] <- 6
ecoact25[ecoact25 == 9] <- 6
ecoact25[ecoact25 == 10] <- 6
ecoact25[ecoact25 == -9] <- -9
ecoact25[ecoact25 == -8] <- -8
ecoact25[ecoact25 == -1] <- -1
ecoact25[is.na(ecoact25)] <- -3
data$ecoact25 <- make_labelled(ecoact25, ecoact_labels)

# Age 32: W9DACTIVITYC
ecoact32 <- data$W9DACTIVITYC
ecoact32[ecoact32 == 1] <- 1
ecoact32[ecoact32 == 2] <- 2
ecoact32[ecoact32 == 3] <- 6
ecoact32[ecoact32 == 4] <- 5
ecoact32[ecoact32 == 5] <- 3
ecoact32[ecoact32 == 6] <- 4
ecoact32[ecoact32 == 7] <- 4
ecoact32[ecoact32 == 8] <- 6
ecoact32[ecoact32 == 9] <- 6
ecoact32[ecoact32 == 10] <- 6
ecoact32[ecoact32 == -9] <- -9
ecoact32[ecoact32 == -8] <- -8
ecoact32[ecoact32 == -1] <- -1
ecoact32[is.na(ecoact32)] <- -3
data$ecoact32 <- make_labelled(ecoact32, ecoact_labels)

# Detailed variables for ages 25 and 32
adu_labels <- c(`1` = "Employee - in paid work", `2` = "Self employed",
                `3` = "In unpaid/voluntary work", `4` = "Unemployed",
                `5` = "Education: School/college/university", `6` = "Apprenticeship",
                `7` = "On gov't scheme for employment training",
                `8` = "Sick or disabled", `9` = "Looking after home or family",
                `10` = "Something else",
                `-1` = "Not applicable", `-3` = "Not asked",
                `-8` = "Insufficient information", `-9` = "Refused")

ecoactadu25 <- data$W8DACTIVITYC
ecoactadu25[is.na(ecoactadu25)] <- -3
data$ecoactadu25 <- make_labelled(ecoactadu25, adu_labels)

ecoactadu32 <- data$W9DACTIVITYC
ecoactadu32[is.na(ecoactadu32)] <- -3
data$ecoactadu32 <- make_labelled(ecoactadu32, adu_labels)

# Select only required columns
output <- data %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Create output directory if needed
dir.create("data/output", showWarnings = FALSE)

# Write output
write_csv(output, "data/output/cleaned_data.csv")
cat("Output written successfully.\n")
cat("Number of rows:", nrow(output), "\n")
cat("Columns:", paste(names(output), collapse = ", "), "\n")
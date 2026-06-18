library(dplyr)
library(readr)
library(haven)
library(labelled)
library(tidyr)
library(purrr)

# Load all files
s1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", show_col_types = FALSE)
s9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge all sweeps
data <- s1 %>%
  full_join(s2, by = "NSID") %>%
  full_join(s3, by = "NSID") %>%
  full_join(s4, by = "NSID") %>%
  full_join(s6, by = "NSID") %>%
  full_join(s7, by = "NSID") %>%
  full_join(s8, by = "NSID") %>%
  full_join(s9, by = "NSID")

# Derive drinking indicator for each sweep
# For sweeps 1-7: drinking indicator = (ever variable == 1)
# For sweep 8-9: drinking indicator = (AUDIT frequency > 1, i.e., not "Never")

# Sweep 1 (age 14): special rule - BOTH W1alceverYP = 1 AND W1alcmonYP = 1
data$drink_14 <- as.integer(!is.na(data$W1alceverYP) & !is.na(data$W1alcmonYP) & data$W1alceverYP == 1 & data$W1alcmonYP == 1)

# Sweeps 2-7 (ages 15, 16, 17, 19, 20): ever had drink = Yes (code 1)
data$drink_15 <- as.integer(!is.na(data$W2alceverYP) & data$W2alceverYP == 1)
data$drink_16 <- as.integer(!is.na(data$W3alceverYP) & data$W3alceverYP == 1)
data$drink_17 <- as.integer(!is.na(data$W4AlcEverYP) & data$W4AlcEverYP == 1)
data$drink_19 <- as.integer(!is.na(data$W6AlcEverYP) & data$W6AlcEverYP == 1)
data$drink_20 <- as.integer(!is.na(data$W7AlcEverYP) & data$W7AlcEverYP == 1)

# Sweeps 8-9 (ages 25, 32): AUDIT frequency > 1 (not "Never")
data$drink_25 <- as.integer(!is.na(data$W8AUDIT1) & data$W8AUDIT1 > 1)
data$drink_32 <- as.integer(!is.na(data$W9AUDIT1) & data$W9AUDIT1 > 1)

# Derive alcfst: earliest age at which drinking is observed
alcfst <- data %>%
  rowwise() %>%
  mutate(
    # Find earliest age where drinking is observed
    earliest_drink = {
      ages <- c(14, 15, 16, 17, 19, 20, 25, 32)
      drinkers <- c(drink_14, drink_15, drink_16, drink_17, drink_19, drink_20, drink_25, drink_32)
      
      # Check if any drinking observed
      has_drinking <- any(drinkers == 1, na.rm = TRUE)
      
      if (has_drinking) {
        # Find earliest age where drinking observed
        min(ages[drinkers == 1])
      } else {
        # No drinking observed
        # Check if all indicators are non-missing (i.e., all "No" responses)
        all_non_missing <- !any(is.na(drinkers))
        
        if (all_non_missing) {
          99  # Never had alcohol
        } else {
          -8  # Don't know / insufficient information
        }
      }
    }
  ) %>%
  ungroup()

# Create factor with specified levels and labels
factor_levels <- c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8)
factor_labels <- c("Age 14", "Age 15", "Age 16", "Age 17", "Age 19", "Age 20", "Age 25", "Age 32", "Never had alcohol", "Don't know/insufficient information")

data$alcfst <- factor(alcfst$earliest_drink, levels = factor_levels, labels = factor_labels)

# Keep only NSID and alcfst
data_out <- data %>% select(NSID, alcfst)

# Write output
write_csv(data_out, "data/output/cleaned_data.csv")

# Summary
cat("Number of rows:", nrow(data_out), "\n")
cat("\nDistribution of alcfst:\n")
print(table(data_out$alcfst, useNA = "ifany"))

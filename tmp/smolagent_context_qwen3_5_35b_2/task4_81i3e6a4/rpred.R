# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from the metadata
# Note: Only load files that contain the variables we need

# Load W6 data (Age 19)
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab",
                 delim = "\t",
                 show_col_types = FALSE)

# Load W7 data (Age 20)
w7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab",
                 delim = "\t",
                 show_col_types = FALSE)

# Load W8 data (Age 25)
w8 <- read_delim("data/input/ns8_2015_self_completion.tab",
                 delim = "\t",
                 show_col_types = FALSE)

# Load W9 data (Age 32)
w9 <- read_delim("data/input/ns9_2022_main_interview.tab",
                 delim = "\t",
                 show_col_types = FALSE)

# Load wave one and wave four (for ID completeness, even if they don't have sexuality data)
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab",
                 delim = "\t",
                 show_col_types = FALSE)

w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab",
                 delim = "\t",
                 show_col_types = FALSE)

# Merge all files by NSID
merged <- full_join(w1, w4, by = "NSID")
merged <- full_join(merged, w6, by = "NSID")
merged <- full_join(merged, w7, by = "NSID")
merged <- full_join(merged, w8, by = "NSID")
merged <- full_join(merged, w9, by = "NSID")

# Function to recode sexuality variables
code_sori <- function(x, wave_name) {
  # Map missing value codes based on wave-specific rules
  # -97 and -100 map to -9 (Refused)
  # -92 maps to -9 (Refused)
  # -91 maps to -1 (Not applicable)
  # -1 maps to -8 (Don't know)
  # -9 maps to -9 (Refused)
  # -8 maps to -8 (Don't know)
  # -3 maps to -3 (Not asked)
  # W9SORI value 5 maps to -7 (Prefer not to say)
  
  # Create recoded variable
  recoded <- x
  
  # Handle W9 specific: value 5 -> -7
  if (wave_name == "W9SORI") {
    recoded[x == 5] <- -7
  }
  
  # Map standard missing codes
  recoded[x == -97 | x == -100 | x == -92] <- -9
  recoded[x == -91] <- -1
  recoded[x == -1] <- -8
  recoded[x == -9] <- -9
  recoded[x == -8] <- -8
  recoded[x == -3] <- -3
  
  return(recoded)
}

# Create the four sori variables
# sori19 from W6SexualityYP (Age 19)
merged$sori19 <- code_sori(merged$W6SexualityYP, "W6SexualityYP")

# sori20 from W7SexualityYP (Age 20)
merged$sori20 <- code_sori(merged$W7SexualityYP, "W7SexualityYP")

# sori25 from W8SEXUALITY (Age 25)
merged$sori25 <- code_sori(merged$W8SEXUALITY, "W8SEXUALITY")

# sori32 from W9SORI (Age 32)
merged$sori32 <- code_sori(merged$W9SORI, "W9SORI")

# Convert to factors with labels
labelled_levels <- c("1" = "Heterosexual/straight",
                     "2" = "Gay/lesbian",
                     "3" = "Bisexual",
                     "4" = "Other")

missing_labels <- c("-9" = "Refusal",
                    "-8" = "Don't know / insufficient information",
                    "-7" = "Prefer not to say",
                    "-3" = "Not asked at the fieldwork stage / not interviewed",
                    "-2" = "Schedule not applicable / script error / information lost",
                    "-1" = "Item not applicable")

merged$sori19 <- factor(merged$sori19, levels = c(1, 2, 3, 4, -1, -2, -3, -7, -8, -9),
                        labels = c("Heterosexual/straight", "Gay/lesbian", "Bisexual", "Other",
                                   "Item not applicable", "Schedule not applicable / script error / information lost",
                                   "Not asked at the fieldwork stage / not interviewed",
                                   "Prefer not to say", "Don't know / insufficient information",
                                   "Refusal"))

merged$sori20 <- factor(merged$sori20, levels = c(1, 2, 3, 4, -1, -2, -3, -7, -8, -9),
                        labels = c("Heterosexual/straight", "Gay/lesbian", "Bisexual", "Other",
                                   "Item not applicable", "Schedule not applicable / script error / information lost",
                                   "Not asked at the fieldwork stage / not interviewed",
                                   "Prefer not to say", "Don't know / insufficient information",
                                   "Refusal"))

merged$sori25 <- factor(merged$sori25, levels = c(1, 2, 3, 4, -1, -2, -3, -7, -8, -9),
                        labels = c("Heterosexual/straight", "Gay/lesbian", "Bisexual", "Other",
                                   "Item not applicable", "Schedule not applicable / script error / information lost",
                                   "Not asked at the fieldwork stage / not interviewed",
                                   "Prefer not to say", "Don't know / insufficient information",
                                   "Refusal"))

merged$sori32 <- factor(merged$sori32, levels = c(1, 2, 3, 4, -1, -2, -3, -7, -8, -9),
                        labels = c("Heterosexual/straight", "Gay/lesbian", "Bisexual", "Other",
                                   "Item not applicable", "Schedule not applicable / script error / information lost",
                                   "Not asked at the fieldwork stage / not interviewed",
                                   "Prefer not to say", "Don't know / insufficient information",
                                   "Refusal"))

# Select only NSID and the four sori variables
output <- merged %>% select(NSID, sori19, sori20, sori25, sori32)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

# Print summary
cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")
cat("Total observations:", nrow(output), "\n")
cat("Variables:", names(output), "\n")
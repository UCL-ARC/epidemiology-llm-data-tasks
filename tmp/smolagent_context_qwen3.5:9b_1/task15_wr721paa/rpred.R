library(readr)
library(dplyr)
library(haven)

# Set paths
input_dir <- "data/input/"
output_file <- "data/output/cleaned_data.csv"

# Create output directory if it doesn't exist
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)

# Load all datasets
wave_one <- read_delim(input_dir %>% paste0("wave_one_lsype_young_person_2020.tab"), delim = "\t")
wave_four <- read_delim(input_dir %>% paste0("wave_four_lsype_young_person_2020.tab"), delim = "\t")
ns8 <- read_delim(input_dir %>% paste0("ns8_2015_derived.tab"), delim = "\t")
ns9 <- read_delim(input_dir %>% paste0("ns9_2022_derived_variables.tab"), delim = "\t")

# Filter to only keep NSID and income variables
wave_one <- wave_one %>% select(NSID)
wave_four <- wave_four %>% select(NSID)
ns8 <- ns8 %>% select(NSID, W8DINCB)
ns9 <- ns9 %>% select(NSID, W9DINCB)

# Rename W8DINCB to inc25 and W9DINCB to inc32
ns8 <- ns8 %>% rename(inc25 = W8DINCB)
ns9 <- ns9 %>% rename(inc32 = W9DINCB)

# Merge all datasets using full_join
combined <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Recode missing values for income variables
# Handle NA values - convert to -3 (Not asked)
combined <- combined %>%
  mutate(
    inc25 = ifelse(is.na(inc25), -3, inc25),
    inc32 = ifelse(is.na(inc32), -3, inc32)
  )

# Define level and label vectors
code_levels <- c(-9, -8, -1, -3, -2, 1:16)
code_labels <- c("Refusal", "Don't know/insufficient information", "Not applicable", 
                 "Not asked/interviewed", "Script error/lost",
                 "less than 25", "25 to 50", "50 to 90", "90 to 140", 
                 "140 to 240", "240 to 300", "300 to 350", "350 to 400", 
                 "400 to 500", "500 to 600", "600 to 700", "700 to 800", 
                 "800 to 900", "900 to 1200", "1200 to 1400", "more than 1400")

combined$inc25 <- factor(combined$inc25,
                        levels = code_levels,
                        labels = code_labels)

combined$inc32 <- factor(combined$inc32,
                        levels = code_levels,
                        labels = code_labels)

# Select only required variables
final_data <- combined %>% select(NSID, inc25, inc32)

# Write to CSV
write_csv(final_data, output_file)

# Print status message
message("Dataset cleaned and saved to", output_file)
cat("Number of rows:", nrow(final_data), "\n")
cat("Variables:", names(final_data), "\n")

library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from metadata
# Wave 1 (Age 14)
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")

# Wave 4 (Age 17)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")

# Wave 8 (Age 25) - Derived
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")

# Wave 9 (Age 32) - Derived Variables
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
df <- full_join(wave1, wave4, by = "NSID")
df <- full_join(df, wave8, by = "NSID")
df <- full_join(df, wave9, by = "NSID")

# Create inc25 from W8DINCB (Age 25)
inc25 <- df$W8DINCB
# Convert -1.0 (Not applicable) to -1
inc25[inc25 == -1.0] <- -1

# Create inc32 from W9DINCB (Age 32)
inc32 <- df$W9DINCB
# Convert -1.0 (Not applicable) to -1
inc32[inc32 == -1.0] <- -1

# Add variables to dataframe
df$inc25 <- inc25
df$inc32 <- inc32

# Keep only NSID and the income variables
output <- df %>%
  select(NSID, inc25, inc32)

# Write output
write_csv(output, "data/output/cleaned_data.csv")

cat("Output written successfully\n")
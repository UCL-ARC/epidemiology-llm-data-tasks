library(haven)
library(dplyr)
library(purrr)
library(readr)
library(labelled)

# Load packages

# Read the data files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Full join all datasets by NSID
merged <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Standard missing codes mapping for W8DINCB
# Original: -1 = Not applicable (already standard)
# NA values should become -3 (not asked)

# Create inc25 from W8DINCB
merged <- merged %>%
  mutate(
    inc25 = W8DINCB,
    # Recode NA to -3 (not asked/interviewed)
    inc25 = ifelse(is.na(inc25), -3, inc25),
    inc25 = factor(inc25, levels = c(-3, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
                   labels = c("-3 (Not asked/interviewed)", 
                             "-1 (Not applicable)",
                             "1 (less than 25)", "2 (25 to 50)", "3 (50 to 90)", "4 (90 to 140)", 
                             "5 (140 to 240)", "6 (240 to 300)", "7 (300 to 350)", "8 (350 to 400)", 
                             "9 (400 to 500)", "10 (500 to 600)", "11 (600 to 700)", "12 (700 to 800)", 
                             "13 (800 to 900)", "14 (900 to 1200)", "15 (1200 to 1400)", "16 (more than 1400)"))
  )

# Create inc32 from W9DINCB
merged <- merged %>%
  mutate(
    inc32 = W9DINCB,
    # Recode NA to -3 (not asked/interviewed)
    inc32 = ifelse(is.na(inc32), -3, inc32),
    inc32 = factor(inc32, levels = c(-3, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
                   labels = c("-3 (Not asked/interviewed)", 
                             "-1 (Not applicable)",
                             "1 (less than 25)", "2 (25 to 50)", "3 (50 to 90)", "4 (90 to 140)", 
                             "5 (140 to 240)", "6 (240 to 300)", "7 (300 to 350)", "8 (350 to 400)", 
                             "9 (400 to 500)", "10 (500 to 600)", "11 (600 to 700)", "12 (700 to 800)", 
                             "13 (800 to 900)", "14 (900 to 1200)", "15 (1200 to 1400)", "16 (more than 1400)"))
  )

# Select only required variables
final_data <- merged %>%
  select(NSID, inc25, inc32)

# Write to CSV
write_csv(final_data, "data/output/cleaned_data.csv")

print("Script completed successfully")
print(head(final_data, 10))
print(table(final_data$inc25))
print(table(final_data$inc32))
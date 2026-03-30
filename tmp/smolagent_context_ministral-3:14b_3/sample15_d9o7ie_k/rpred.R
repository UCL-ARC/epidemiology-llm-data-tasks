library(haven)
library(dplyr)
library(readr)

# Load datasets
wave8 <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets by NSID
merged_data <- full_join(wave8, wave9, by = "NSID")

# Select and rename income variables
income_vars <- merged_data %>%
  select(NSID, W8DINCB, W9DINCB) %>%
  rename(inc25 = W8DINCB, inc32 = W9DINCB)

# Define missing value codes and labels
standard_missing <- c(-9, -8, -1, -3, -2)
standard_missing_labels <- c(
  "Refusal", 
  "Don't know/insufficient information", 
  "Item not applicable", 
  "Not asked/interviewed", 
  "Script error/lost"
)

# Define income labels
income_labels <- c(
  "less than 25", "25 to 50", "50 to 90", "90 to 140", "140 to 240", 
  "240 to 300", "300 to 350", "350 to 400", "400 to 500", "500 to 600", 
  "600 to 700", "700 to 800", "800 to 900", "900 to 1200", "1200 to 1400", 
  "more than 1400"
)

# Replace NA with -3 (Not asked/interviewed)
cleaned_data <- income_vars %>%
  mutate(
    inc25 = ifelse(is.na(inc25), -3, inc25),
    inc32 = ifelse(is.na(inc32), -3, inc32)
  )

# Convert to factors with labels
cleaned_data <- cleaned_data %>%
  mutate(
    inc25 = factor(inc25, levels = c(standard_missing, 1:16),
                   labels = c(standard_missing_labels, income_labels)),
    inc32 = factor(inc32, levels = c(standard_missing, 1:16),
                   labels = c(standard_missing_labels, income_labels))
  )

# Write output to CSV
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)
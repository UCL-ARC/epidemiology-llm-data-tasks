library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(haven)
library(labelled)

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets
full_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to collapse fractional NS-SEC codes to major category
collapse_nssec <- function(x) {
  x[is.na(x)] <- -3
  x[x == -99] <- -3
  x[x == -91] <- -1
  x[x == -999] <- -2
  x[x == -92] <- -9
  x[x == -94] <- -8
  x[x == -97] <- -2
  x[x == -100] <- -2
  valid_mask <- x >= 0
  x[valid_mask] <- floor(as.numeric(x[valid_mask]))
  return(as.numeric(x))
}

# Derive nssec17 from W4nsseccatYP (Wave 4, Age 17)
full_data <- full_data %>%
  mutate(nssec17 = collapse_nssec(W4nsseccatYP))

# Derive nssec18 from W5nsseccatYP (Wave 5, Age 18)
full_data <- full_data %>%
  mutate(nssec18 = collapse_nssec(W5nsseccatYP))

# Derive nssec19 from w6nsseccatYP (Wave 6, Age 19)
full_data <- full_data %>%
  mutate(nssec19 = collapse_nssec(w6nsseccatYP))

# Derive nssec20 from W7NSSECCat (Wave 7, Age 20)
full_data <- full_data %>%
  mutate(nssec20 = collapse_nssec(W7NSSECCat))

# Derive nssec25 from W8DNSSEC17 (Wave 8, Age 25)
full_data <- full_data %>%
  mutate(nssec25 = collapse_nssec(W8DNSSEC17))

# Apply special rule for nssec25: if W8DACTIVITYC == 5, set to 15
full_data <- full_data %>%
  mutate(nssec25 = ifelse(W8DACTIVITYC == 5 & !is.na(W8DACTIVITYC), 15, nssec25))

# Derive nssec32 from W9NSSEC (Wave 9, Age 32)
full_data <- full_data %>%
  mutate(nssec32 = collapse_nssec(W9NSSEC))

# Define NS-SEC labels
labels_names <- c("Employers in large organisations",
                  "Higher managerial and administrative occupations",
                  "Higher professional occupations",
                  "Lower professional and higher technical occupations",
                  "Lower managerial and administrative occupations",
                  "Higher supervisory occupations",
                  "Intermediate occupations",
                  "Employers in small establishments",
                  "Own account workers",
                  "Lower supervisory occupations",
                  "Lower technical occupations",
                  "Semi-routine occupations",
                  "Routine occupations",
                  "Never worked and Long-term unemployed",
                  "Full-time students",
                  "Occupations not stated or inadequately described",
                  "Not classifiable for other reasons",
                  "Refusal",
                  "Don\'t know / insufficient information",
                  "Prefer not to say",
                  "Not asked / not interviewed",
                  "Schedule not applicable",
                  "Not applicable")

labels_vals <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, -9, -8, -7, -3, -2, -1)
nssec_labels <- setNames(labels_names, labels_vals)

# Create labelled factors - use labelled::set_value_labels on numeric vector
for (var in c("nssec17", "nssec18", "nssec19", "nssec20", "nssec25", "nssec32")) {
  # Set value labels on numeric vector
  full_data[[var]] <- labelled::set_value_labels(full_data[[var]], nssec_labels)
  # Convert to factor
  full_data[[var]] <- haven::as_factor(full_data[[var]])
}

# Select only ID and final derived variables
output_data <- full_data %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")

# Print summary
cat("\nOutput dimensions:", dim(output_data), "\n")
cat("NSID count:", n_distinct(output_data$NSID), "\n")
for (var in c("nssec17", "nssec18", "nssec19", "nssec20", "nssec25", "nssec32")) {
  cat(var, ":\n")
  print(summary(output_data[[var]]))
}
print(head(output_data))
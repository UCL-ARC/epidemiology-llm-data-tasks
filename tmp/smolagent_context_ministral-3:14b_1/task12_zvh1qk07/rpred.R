
# Load required libraries
library(haven)
library(dplyr)
library(readr)

# Step 1: Load files and merge
wave1 <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- readr::read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets by NSID, keeping all rows from wave1
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Step 2: Rename variables
merged_data <- merged_data %>%
  rename(
    nssec17 = W4nsseccatYP,
    nssec18 = W5nsseccatYP,
    nssec19 = w6nsseccatYP,
    nssec20 = W7NSSECCat,
    nssec25 = W8DNSSEC17,
    nssec32 = W9NSSEC
  )

# Step 3: Define missing value harmonization
harmonize_missing <- function(x, var_name) {
  if (var_name == "nssec17") {
    x[x == -99] <- -3
    x[x == -91] <- -1
  } else if (var_name %in% c("nssec18", "nssec19", "nssec20")) {
    x[x <= 1 & x >= -999] <- -3
  } else if (var_name == "nssec25") {
    x[x == -9] <- -1
    x[x == -8] <- -8
  } else if (var_name == "nssec32") {
    x[x == -9] <- -1
    x[x == -8] <- -8
    x[x == -7] <- -7
    x[x == -3] <- -3
    x[x == -2] <- -2
  }
  x[is.na(x)] <- -3
  return(x)
}

# Function to process NS-SEC variables
process_nssec <- function(data, var_name) {
  # Apply missing value harmonization
  data[[var_name]] <- harmonize_missing(data[[var_name]], var_name)

  # Collapse fractional codes to major categories
  data[[var_name]] <- floor(data[[var_name]])

  # Ensure only valid categories (1-17) are retained
  data[[var_name]][data[[var_name]] >= 1 & data[[var_name]] <= 17] <- data[[var_name]][data[[var_name]] >= 1 & data[[var_name]] <= 17]
  data[[var_name]][!(data[[var_name]] >= 1 & data[[var_name]] <= 17)] <- -3

  return(data)
}

# Apply to each NS-SEC variable
nssec_vars <- c("nssec17", "nssec18", "nssec19", "nssec20", "nssec25", "nssec32")
for (var in nssec_vars) {
  merged_data <- process_nssec(merged_data, var)
}

# Special derivation for Age 25: Full-time student category (15)
if ("W8DACTIVITYC" %in% names(merged_data)) {
  merged_data$nssec25 <- ifelse(merged_data$W8DACTIVITYC == 5 & merged_data$nssec25 != 15, 15, merged_data$nssec25)
}

# Step 4: Define all possible levels and labels for NS-SEC variables
all_levels <- c(-9, -8, -7, -3, -2, -1, 1:17)
nssec_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-1" = "Item not applicable",
  "1" = "Employers in large organisations",
  "2" = "Higher managerial and administrative occupations",
  "3" = "Higher professional occupations",
  "4" = "Lower professional and higher technical occupations",
  "5" = "Lower managerial and administrative occupations",
  "6" = "Higher supervisory occupations",
  "7" = "Intermediate occupations",
  "8" = "Employers in small establishments",
  "9" = "Own account workers",
  "10" = "Lower supervisory occupations",
  "11" = "Lower technical occupations",
  "12" = "Semi-routine occupations",
  "13" = "Routine occupations",
  "14" = "Never worked and Long-term unemployed",
  "15" = "Full-time students",
  "16" = "Occupations not stated or inadequately described",
  "17" = "Not classifiable for other reasons"
)

# Create a mapping from numeric to character for labels
label_map <- function(x) {
  as.character(x)
}

# Step 5: Convert NS-SEC variables to factors with all possible levels
for (var in nssec_vars) {
  # Create factor with all possible levels
  merged_data[[var]] <- factor(
    merged_data[[var]],
    levels = all_levels,
    labels = nssec_labels[label_map(all_levels)]
  )
}

# Step 6: Select only required variables
final_data <- merged_data %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Step 7: Write output
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)

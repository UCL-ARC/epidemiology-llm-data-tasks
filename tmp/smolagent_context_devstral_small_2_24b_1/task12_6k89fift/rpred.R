library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all required datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_five <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")

# Attempt to load wave_nine, handle potential issues
wave_nine_path <- "data/input/ns9_2022_main_interview.tab"
if (file.exists(wave_nine_path)) {
  wave_nine <- read_delim(wave_nine_path, delim = "\t")
} else {
  wave_nine <- NULL
}

# Merge datasets step by step, checking for NSID at each step
merged_data <- wave_one

if ("NSID" %in% colnames(wave_four)) {
  merged_data <- merged_data %>%
    full_join(wave_four, by = "NSID")
} else {
  message("NSID not found in wave_four, skipping")
}

if ("NSID" %in% colnames(wave_five)) {
  merged_data <- merged_data %>%
    full_join(wave_five, by = "NSID")
} else {
  message("NSID not found in wave_five, skipping")
}

if ("NSID" %in% colnames(wave_six)) {
  merged_data <- merged_data %>%
    full_join(wave_six, by = "NSID")
} else {
  message("NSID not found in wave_six, skipping")
}

if ("NSID" %in% colnames(wave_seven)) {
  merged_data <- merged_data %>%
    full_join(wave_seven, by = "NSID")
} else {
  message("NSID not found in wave_seven, skipping")
}

if ("NSID" %in% colnames(wave_eight)) {
  merged_data <- merged_data %>%
    full_join(wave_eight, by = "NSID")
} else {
  message("NSID not found in wave_eight, skipping")
}

if (!is.null(wave_nine) && "NSID" %in% colnames(wave_nine)) {
  merged_data <- merged_data %>%
    full_join(wave_nine, by = "NSID")
} else {
  message("wave_nine not loaded or NSID not found, skipping")
}

# Define a function to collapse fractional NS-SEC codes to major categories
harmonise_nssec <- function(var, wave) {
  # Collapse fractional codes to integer part
  major_cat <- floor(var)
  
  # Apply standard missing-value codes
  major_cat <- ifelse(var %in% c(-9, -8, -7), -9, major_cat)  # Refusal, Don't know, Prefer not to say
  major_cat <- ifelse(var %in% c(-3), -3, major_cat)  # Not asked / not interviewed
  major_cat <- ifelse(var %in% c(-2), -2, major_cat)  # Schedule not applicable / script error
  major_cat <- ifelse(var %in% c(-1), -1, major_cat)  # Item not applicable
  
  # Handle wave-specific missing codes
  if (wave == "wave_four") {
    major_cat <- ifelse(var == -99, -3, major_cat)  # YP Not interviewed
    major_cat <- ifelse(var == -91, -1, major_cat)  # Not applicable
  } else if (wave == "wave_six" | wave == "wave_seven" | wave == "wave_five") {
    major_cat <- ifelse(var == -91, -1, major_cat)  # Not applicable
  } else if (wave == "wave_eight") {
    major_cat <- ifelse(var == -9 | var == -8, -9, major_cat)  # Refused, Insufficient information
    major_cat <- ifelse(var == -1, -1, major_cat)  # Not applicable
  } else if (wave == "wave_nine") {
    major_cat <- ifelse(var >= -9 & var <= -1, -3, major_cat)  # User missing values
  }
  
  return(major_cat)
}

# Derive nssec17 (wave 4)
if ("W4nsseccatYP" %in% colnames(merged_data)) {
  merged_data <- merged_data %>%
    mutate(nssec17 = harmonise_nssec(W4nsseccatYP, "wave_four"))
} else {
  message("W4nsseccatYP not found, creating nssec17 with NA")
  merged_data$nssec17 <- NA
}

# Derive nssec18 (wave 5)
if ("W5nsseccatYP" %in% colnames(merged_data)) {
  merged_data <- merged_data %>%
    mutate(nssec18 = harmonise_nssec(W5nsseccatYP, "wave_five"))
} else {
  message("W5nsseccatYP not found, creating nssec18 with NA")
  merged_data$nssec18 <- NA
}

# Derive nssec19 (wave 6)
if ("w6nsseccatYP" %in% colnames(merged_data)) {
  merged_data <- merged_data %>%
    mutate(nssec19 = harmonise_nssec(w6nsseccatYP, "wave_six"))
} else {
  message("w6nsseccatYP not found, creating nssec19 with NA")
  merged_data$nssec19 <- NA
}

# Derive nssec20 (wave 7)
if ("W7NSSECCat" %in% colnames(merged_data)) {
  merged_data <- merged_data %>%
    mutate(nssec20 = harmonise_nssec(W7NSSECCat, "wave_seven"))
} else {
  message("W7NSSECCat not found, creating nssec20 with NA")
  merged_data$nssec20 <- NA
}

# Derive nssec25 (wave 8) with special handling for full-time education
if ("W8DNSSEC17" %in% colnames(merged_data)) {
  merged_data <- merged_data %>%
    mutate(nssec25 = harmonise_nssec(W8DNSSEC17, "wave_eight"))
  
  # Apply special rule for nssec25: assign category 15 if in full-time education
  if ("W8DACTIVITYC" %in% colnames(merged_data)) {
    merged_data <- merged_data %>%
      mutate(nssec25 = ifelse(W8DACTIVITYC == 5, 15, nssec25))
  }
} else {
  message("W8DNSSEC17 not found, creating nssec25 with NA")
  merged_data$nssec25 <- NA
}

# Derive nssec32 (wave 9)
if (!is.null(wave_nine) && "W9NSSEC" %in% colnames(merged_data)) {
  merged_data <- merged_data %>%
    mutate(nssec32 = harmonise_nssec(W9NSSEC, "wave_nine"))
} else {
  message("W9NSSEC not found, creating nssec32 with NA")
  merged_data$nssec32 <- NA
}

# Create labelled factors for each nssec variable
nssec_labels <- c(
  "1" = "Employers in large organisations",
  "2" = "Higher managerial occupations",
  "3" = "Higher professional occupations",
  "4" = "Lower professional and higher technical occupations",
  "5" = "Lower managerial occupations",
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
  "17" = "Not classifiable for other reasons",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

for (var in c("nssec17", "nssec18", "nssec19", "nssec20", "nssec25", "nssec32")) {
  if (var %in% colnames(merged_data)) {
    merged_data[[var]] <- factor(merged_data[[var]], levels = as.numeric(names(nssec_labels)), labels = nssec_labels)
  }
}

# Select only the ID variable and final derived variables
output_data <- merged_data %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")
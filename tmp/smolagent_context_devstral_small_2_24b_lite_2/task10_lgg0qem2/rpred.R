library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_five <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave_nine <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_five, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Define metadata for each economic activity variable based on the provided metadata
metadata_W4empsYP <- list(
  value_labels = c(
    "-999.0" = "Missing household information - lost",
    "-94.0" = "Insufficient information",
    "-92.0" = "Refused",
    "-91.0" = "Not applicable - still at school",
    "1.0" = "Doing paid work for 30 or more hours a week",
    "2.0" = "Doing paid work for fewer than 30 hours a week",
    "3.0" = "Unemployed/ Looking for a job",
    "4.0" = "On a training course or scheme",
    "5.0" = "In full-time education/ at school",
    "6.0" = "Looking after the family/ household",
    "7.0" = "Retired from work altogether",
    "8.0" = "Sick/ disabled",
    "9.0" = "Other"
  )
)

metadata_W5mainactYP <- list(
  value_labels = c(
    "-94.0" = "Insufficient information",
    "1.0" = "Apprenticeship",
    "2.0" = "Part of week with employer, part of week at college",
    "3.0" = "In paid work",
    "4.0" = "In education",
    "5.0" = "On a training course/scheme",
    "6.0" = "On the Entry to Employment scheme",
    "7.0" = "Unemployed and looking for work",
    "8.0" = "Looking after the family and home",
    "9.0" = "Waiting for a course or job to start",
    "10.0" = "Waiting for exam results",
    "11.0" = "Waiting for the result of a job application"
  )
)

metadata_W6TCurrentAct <- list(
  value_labels = c(
    "-91.0" = "Unable to classify",
    "1.0" = "Doing a course at a university",
    "2.0" = "In education",
    "3.0" = "In paid work",
    "4.0" = "On a training course or scheme",
    "5.0" = "Doing an Apprenticeship",
    "6.0" = "Waiting for a course or job to start",
    "7.0" = "Looking after the family and home",
    "8.0" = "Unemployed and looking for work",
    "9.0" = "Waiting for exam results or result of job application",
    "10.0" = "Spending part of the week with an employer and part of the week at college",
    "11.0" = "Doing voluntary work"
  )
)

metadata_W7TCurrentAct <- list(
  value_labels = c(
    "-91.0" = "Not applicable",
    "1.0" = "University",
    "2.0" = "School/college education",
    "3.0" = "Paid work",
    "4.0" = "Training course/scheme",
    "5.0" = "Apprenticeship",
    "6.0" = "Waiting for a course or job to start",
    "7.0" = "Looking after home/family",
    "8.0" = "Unemployed and looking for work",
    "9.0" = "Part time job and part time college",
    "10.0" = "Voluntary work",
    "11.0" = "Government employment programme",
    "12.0" = "Travelling",
    "13.0" = "Break from work/college",
    "14.0" = "Ill or disabled",
    "15.0" = "Not defined"
  )
)

metadata_W8DACTIVITYC <- list(
  value_labels = c(
    "-9.0" = "Refused",
    "-8.0" = "Insufficient information",
    "-1.0" = "Not applicable",
    "1.0" = "Employee - in paid work",
    "2.0" = "Self employed",
    "3.0" = "In unpaid/voluntary work",
    "4.0" = "Unemployed",
    "5.0" = "Education: School/college/university",
    "6.0" = "Apprenticeship",
    "7.0" = "On gov't scheme for employment training",
    "8.0" = "Sick or disabled",
    "9.0" = "Looking after home or family",
    "10.0" = "Something else"
  )
)

metadata_W9DACTIVITYC <- list(
  value_labels = c(
    "-9.0" = "Refused",
    "-8.0" = "Insufficient information",
    "-1.0" = "Not applicable",
    "1.0" = "Employee - in paid work",
    "2.0" = "Self employed",
    "3.0" = "In unpaid/voluntary work",
    "4.0" = "Unemployed",
    "5.0" = "Education: School/college/university",
    "6.0" = "Apprenticeship",
    "7.0" = "On gov't scheme for employment training",
    "8.0" = "Sick or disabled",
    "9.0" = "Looking after home or family",
    "10.0" = "Something else"
  )
)

# Define a function to harmonize missing values
harmonize_missing <- function(var, metadata) {
  var <- as.numeric(var)
  
  # Initialize with -3 for NA values
  var[is.na(var)] <- -3
  
  # Apply specific label-based mappings
  if (!is.null(metadata$value_labels)) {
    for (code in names(metadata$value_labels)) {
      label <- metadata$value_labels[[code]]
      
      if (grepl("Refused", label)) {
        var[var == as.numeric(code)] <- -9
      } else if (grepl("Insufficient information", label)) {
        var[var == as.numeric(code)] <- -8
      } else if (grepl("Not applicable", label)) {
        var[var == as.numeric(code)] <- -1
      } else if (grepl("Missing household information - lost", label)) {
        var[var == as.numeric(code)] <- -2
      } else if (grepl("Unable to classify", label)) {
        var[var == as.numeric(code)] <- -2
      }
    }
  }
  
  return(var)
}

# Harmonize economic activity variables
merged_data$W4empsYP <- harmonize_missing(merged_data$W4empsYP, metadata_W4empsYP)
merged_data$W5mainactYP <- harmonize_missing(merged_data$W5mainactYP, metadata_W5mainactYP)
merged_data$W6TCurrentAct <- harmonize_missing(merged_data$W6TCurrentAct, metadata_W6TCurrentAct)
merged_data$W7TCurrentAct <- harmonize_missing(merged_data$W7TCurrentAct, metadata_W7TCurrentAct)
merged_data$W8DACTIVITYC <- harmonize_missing(merged_data$W8DACTIVITYC, metadata_W8DACTIVITYC)
merged_data$W9DACTIVITYC <- harmonize_missing(merged_data$W9DACTIVITYC, metadata_W9DACTIVITYC)

# Define a function to collapse economic activity into 6 categories
collapse_ecoact <- function(var, metadata) {
  var <- as.numeric(var)
  
  # Initialize collapsed variable with missing values
  collapsed <- rep(-3, length(var))
  
  # Map categories based on metadata labels
  if (!is.null(metadata$value_labels)) {
    for (code in names(metadata$value_labels)) {
      label <- metadata$value_labels[[code]]
      
      # Category 1: Paid work (30+ hours or any paid work)
      if (grepl("paid work", label, ignore.case = TRUE)) {
        collapsed[var == as.numeric(code)] <- 1
      }
      
      # Category 2: Paid work (<30 hours)
      if (grepl("fewer than 30 hours", label, ignore.case = TRUE)) {
        collapsed[var == as.numeric(code)] <- 2
      }
      
      # Category 3: Unemployed
      if (grepl("Unemployed", label, ignore.case = TRUE)) {
        collapsed[var == as.numeric(code)] <- 3
      }
      
      # Category 4: Training course or scheme
      if (grepl("training course|scheme", label, ignore.case = TRUE)) {
        collapsed[var == as.numeric(code)] <- 4
      }
      
      # Category 5: Education
      if (grepl("education|school|college|university", label, ignore.case = TRUE)) {
        collapsed[var == as.numeric(code)] <- 5
      }
      
      # Category 6: Other (including looking after family, sick/disabled, etc.)
      if (grepl("Looking after|family|household|sick|disabled|other|voluntary|waiting|travelling|break", label, ignore.case = TRUE)) {
        collapsed[var == as.numeric(code)] <- 6
      }
    }
  }
  
  return(collapsed)
}

# Create collapsed economic activity variables
merged_data$ecoact17 <- collapse_ecoact(merged_data$W4empsYP, metadata_W4empsYP)
merged_data$ecoact18 <- collapse_ecoact(merged_data$W5mainactYP, metadata_W5mainactYP)
merged_data$ecoact19 <- collapse_ecoact(merged_data$W6TCurrentAct, metadata_W6TCurrentAct)
merged_data$ecoact20 <- collapse_ecoact(merged_data$W7TCurrentAct, metadata_W7TCurrentAct)
merged_data$ecoact25 <- collapse_ecoact(merged_data$W8DACTIVITYC, metadata_W8DACTIVITYC)
merged_data$ecoact32 <- collapse_ecoact(merged_data$W9DACTIVITYC, metadata_W9DACTIVITYC)

# Create detailed economic activity variables for ages 25 and 32
merged_data$ecoactadu25 <- merged_data$W8DACTIVITYC
merged_data$ecoactadu32 <- merged_data$W9DACTIVITYC

# Select only the required variables for output
output_data <- merged_data %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"
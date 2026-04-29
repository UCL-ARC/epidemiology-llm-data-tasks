# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Set the data directory
data_dir <- "data/input/"
output_file <- "data/output/cleaned_data.csv"

# Function to load tab-delimited files
load_tab_file <- function(filepath) {
  read_delim(filepath, delim = "\t")
}

# Load the required files
wave1 <- load_tab_file(file.path(data_dir, "wave_one_lsype_young_person_2020.tab"))
wave4 <- load_tab_file(file.path(data_dir, "wave_four_lsype_young_person_2020.tab"))
wave8_main <- load_tab_file(file.path(data_dir, "ns8_2015_main_interview.tab"))
wave8_derived <- load_tab_file(file.path(data_dir, "ns8_2015_derived.tab"))
wave9_main <- load_tab_file(file.path(data_dir, "ns9_2022_main_interview.tab"))
wave9_derived <- load_tab_file(file.path(data_dir, "ns9_2022_derived_variables.tab"))

# Create the Wave 8 dataset
wave8_data <- full_join(wave8_main, wave8_derived, by = "NSID")

# Create the Wave 9 dataset
wave9_data <- full_join(wave9_main, wave9_derived, by = "NSID")

# List of W8VCQU variables
w8_voc_vars <- c("W8VCQU0A", "W8VCQU0B", "W8VCQU0C", "W8VCQU0D", "W8VCQU0E", 
                 "W8VCQU0F", "W8VCQU0G", "W8VCQU0H", "W8VCQU0I", "W8VCQU0J", 
                 "W8VCQU0K", "W8VCQU0L", "W8VCQU0M", "W8VCQU0N", "W8VCQU0O", 
                 "W8VCQU0P", "W8VCQU0Q", "W8VCQU0R")

# List of W9ACQU variables
w9_acqu_vars <- c("W9ACQU0A", "W9ACQU0B", "W9ACQU0C", "W9ACQU0D", "W9ACQU0E", 
                  "W9ACQU0F", "W9ACQU0G", "W9ACQU0H", "W9ACQU0I", "W9ACQU0J", 
                  "W9ACQU0K", "W9ACQU0L", "W9ACQU0M", "W9ACQU0N", "W9ACQU0O", 
                  "W9ACQU0P", "W9ACQU0Q", "W9ACQU0R", "W9ACQU0S", "W9ACQU0T", 
                  "W9ACQU0U", "W9ACQU0V")

# List of W9VCQU variables
w9_vqu_vars <- c("W9VCQU0A", "W9VCQU0B", "W9VCQU0C", "W9VCQU0D", "W9VCQU0E", 
                 "W9VCQU0F", "W9VCQU0G", "W9VCQU0H", "W9VCQU0I", "W9VCQU0J", 
                 "W9VCQU0K", "W9VCQU0L", "W9VCQU0M", "W9VCQU0N", "W9VCQU0O", 
                 "W9VCQU0P", "W9VCQU0Q", "W9VCQU0R", "W9VCQU0S", "W9VCQU0T", 
                 "W9VCQU0U", "W9VCQU0V", "W9VCQU0W", "W9VCQU0X", "W9VCQU0Y", 
                 "W9VCQU0Z", "W9VCQUAA", "W9VCQUAB", "W9VCQUAC", "W9VCQUAD", 
                 "W9VCQUAE", "W9VCQUAF", "W9VCQUAG", "W9VCQUAH", "W9VCQUAI")

# Map vocational qualifications to NVQ levels based on metadata
voc_var_level_mapping <- list(
  "W8VCQU0A" = 1,  # Youth training certificate - Entry
  "W8VCQU0B" = 1,  # Key Skills - Entry
  "W8VCQU0C" = 1,  # Basic skills - Entry
  "W8VCQU0D" = 1,  # Entry level qualifications - Entry
  "W8VCQU0E" = 2,  # Modern apprenticeship - NVQ 1-3
  "W8VCQU0F" = 2,  # RSA/OCR/Clerical - NVQ 1-3
  "W8VCQU0G" = 2,  # City and Guilds Certificate - NVQ 1-3
  "W8VCQU0H" = 2,  # GNVQ/GSVQ - NVQ 1-3
  "W8VCQU0I" = 1,  # NVQ/SVQ Level 1-2 - NVQ 1-2
  "W8VCQU0J" = 2,  # NVQ/SVQ Level 3-5 - NVQ 3-5
  "W8VCQU0K" = 0,  # HNC/HND - NVQ 4-5
  "W8VCQU0L" = 1,  # ONC/OND - NVQ 1-3
  "W8VCQU0M" = 1,  # BTEC - NVQ 1-3
  "W8VCQU0N" = 1,  # SCOTVEC - NVQ 1-3
  "W8VCQU0O" = 1,  # Other - NVQ 1-3
  "W8VCQU0P" = 1,  # None of the above
  "W8VCQU0Q" = 99, # Don't know
  "W8VCQU0R" = 99  # Refused
)

# Academic labels for educadtl32
acqu_labels <- c(
  "W9ACQU0A" = "Doctorate or equivalent",
  "W9ACQU0B" = "Masters or equivalent",
  "W9ACQU0C" = "Undergraduate or equivalent",
  "W9ACQU0D" = "Post-graduate Diplomas and Certificates",
  "W9ACQU0E" = "Diplomas in higher education and other higher education qualifications",
  "W9ACQU0F" = "Teaching qualifications for schools or further education (below degree level)",
  "W9ACQU0G" = "A/AS Levels or equivalent",
  "W9ACQU0H" = "Grade A-C, Level 4-9",
  "W9ACQU0I" = "Grade D-G, Level 1-3",
  "W9ACQU0J" = "SCE Higher",
  "W9ACQU0K" = "Scottish Certificate Sixth Year Studies",
  "W9ACQU0L" = "SCE Standard",
  "W9ACQU0M" = "National 4 and 5",
  "W9ACQU0N" = "National 2 and 3",
  "W9ACQU0O" = "Leaving Certificate",
  "W9ACQU0P" = "Junior Certificate grade A-C",
  "W9ACQU0Q" = "Junior Certificate grade D and below",
  "W9ACQU0R" = "Other academic qualifications (including overseas)",
  "W9ACQU0S" = "None of these qualifications",
  "W9ACQU0T" = "Not applicable",
  "W9ACQU0U" = "Refused",
  "W9ACQU0V" = "No answer"
)

# Vocational labels for educvdtl32
vqu_labels <- c(
  "W9VCQU0A" = "Professional qualifications at degree level",
  "W9VCQU0B" = "Nursing or other medical qualifications",
  "W9VCQU0C" = "Level 4 or 5",
  "W9VCQU0D" = "Level 3",
  "W9VCQU0E" = "Level 2",
  "W9VCQU0F" = "Level 1",
  "W9VCQU0G" = "GNVQ Advanced",
  "W9VCQU0H" = "GNVQ Intermediate",
  "W9VCQU0I" = "Level 3",
  "W9VCQU0J" = "Level 2",
  "W9VCQU0K" = "Level Foundation",
  "W9VCQU0L" = "Advanced Craft, Part III",
  "W9VCQU0M" = "Craft, Part II",
  "W9VCQU0N" = "Craft, Part I",
  "W9VCQU0O" = "Level 3",
  "W9VCQU0P" = "Level 2",
  "W9VCQU0Q" = "Level 1",
  "W9VCQU0R" = "Advanced Diploma",
  "W9VCQU0S" = "Higher Diploma",
  "W9VCQU0T" = "RSA Diploma",
  "W9VCQU0U" = "RSA Stage I, II,III",
  "W9VCQU0V" = "Higher Level BTEC",
  "W9VCQU0W" = "BTEC National",
  "W9VCQU0X" = "BTEC First",
  "W9VCQU0Y" = "SCOTVEC National Certificate",
  "W9VCQU0Z" = "SCOTVEC first or general diploma",
  "W9VCQUAA" = "SCOTVEC general diploma",
  "W9VCQUAB" = "SCOTVEC modules",
  "W9VCQUAC" = "HND or HNC",
  "W9VCQUAD" = "OND or ONCM",
  "W9VCQUAE" = "Junior certificate",
  "W9VCQUAF" = "Other vocational qualifications",
  "W9VCQUAG" = "None of these qualifications",
  "W9VCQUAH" = "Don't know",
  "W9VCQUAI" = "Refused"
)

# Create educ25 function with proper NA handling
compute_educ25 <- function(df) {
  result <- integer(nrow(df))
  for (i in 1:nrow(df)) {
    row <- df[i, ]
    
    # Get academic NVQ level
    acad <- as.integer(row$W8DHANVQH)
    
    # Find highest vocational qualification level
    max_voc <- -1
    for (var in w8_voc_vars) {
      val <- as.integer(row[[var]])
      # Proper NA handling
      if (is.na(val) || val %in% c(-9, -8, -1)) next
      if (val == 1) {
        if (var %in% names(voc_var_level_mapping)) {
          lvl <- voc_var_level_mapping[[var]]
          # Handle NA from mapping
          if (!is.na(lvl)) {
            max_voc <- max(max_voc, lvl)
          }
        }
      }
    }
    
    # Determine final level based on highest valid qualification
    if (!is.na(acad) && acad %in% 1:5) {
      if (acad >= 4) {
        result[i] <- 0  # NVQ 4-5
      } else {
        result[i] <- 1  # NVQ 1-3
      }
    } else if (max_voc >= 0) {
      if (max_voc >= 0) {
        result[i] <- 0  # NVQ 4-5
      } else {
        result[i] <- 1  # NVQ 1-3
      }
    } else {
      result[i] <- 4  # None of these qualifications
    }
  }
  result
}

# Create educ32 function with proper NA handling
compute_educ32 <- function(df) {
  result <- integer(nrow(df))
  for (i in 1:nrow(df)) {
    row <- df[i, ]
    
    acad <- as.integer(row$W9DANVQH)
    voc <- as.integer(row$W9DVNVQH)
    
    # Check for valid qualifications (0-5 is valid)
    valid_acad <- !is.na(acad) && acad %in% 0:5
    valid_voc <- !is.na(voc) && voc %in% 0:5
    
    result[i] <- 4  # Default: None of these qualifications
    
    # Use the higher of the two if both are valid
    if (valid_acad && valid_voc) {
      max_qual <- max(acad, voc)
      if (max_qual >= 4) {
        result[i] <- 0  # NVQ 4-5
      } else {
        result[i] <- 1  # NVQ 1-3
      }
    } else if (valid_acad) {
      if (acad >= 4) {
        result[i] <- 0
      } else {
        result[i] <- 1
      }
    } else if (valid_voc) {
      if (voc >= 4) {
        result[i] <- 0
      } else {
        result[i] <- 1
      }
    }
  }
  result
}

# Create educadtl32 function with proper NA handling
compute_educadtl32 <- function(df) {
  result <- character(nrow(df))
  for (i in 1:nrow(df)) {
    row <- df[i, ]
    found <- FALSE
    
    for (var in w9_acqu_vars) {
      val <- as.integer(row[[var]])
      
      # Skip missing codes (-3, -1 are missing)
      if (is.na(val) || val %in% c(-3, -1)) next
      
      # Check for "Yes"
      if (val == 1) {
        result[i] <- acqu_labels[var]
        found <- TRUE
        break
      }
    }
    
    if (!found) {
      result[i] <- "None of these qualifications"
    }
  }
  result
}

# Create educvdtl32 function with proper NA handling
compute_educvdtl32 <- function(df) {
  result <- character(nrow(df))
  for (i in 1:nrow(df)) {
    row <- df[i, ]
    found <- FALSE
    
    for (var in w9_vqu_vars) {
      val <- as.integer(row[[var]])
      
      # Skip missing codes
      if (is.na(val) || val %in% c(-3, -1)) next
      
      # Check for "Yes"
      if (val == 1) {
        result[i] <- vqu_labels[var]
        found <- TRUE
        break
      }
    }
    
    if (!found) {
      result[i] <- "None of these qualifications"
    }
  }
  result
}

# Extract required columns from wave 8 data
wave8_wide <- wave8_data %>%
  select(NSID, W8DHANVQH, all_of(w8_voc_vars))

# Extract required columns from wave 9 data
wave9_wide <- wave9_data %>%
  select(NSID, W9DANVQH, W9DVNVQH, all_of(w9_acqu_vars), all_of(w9_vqu_vars))

# Merge the datasets
final_data <- full_join(wave8_wide, wave9_wide, by = "NSID")

# Apply computations
final_data <- final_data %>%
  mutate(educ25 = compute_educ25(.)) %>%
  mutate(educ32 = compute_educ32(.)) %>%
  mutate(educadtl32 = compute_educadtl32(.)) %>%
  mutate(educvdtl32 = compute_educvdtl32(.))

# Select final columns
result <- final_data %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write to output file
write_csv(result, output_file)

cat("Successfully created", output_file, "\n")
cat("Rows:", nrow(result), "\n")
cat("Columns:", names(result), "\n")

# Print summary
cat("\nSummary of educ25:\n")
print(table(result$educ25))

cat("\nSummary of educ32:\n")
print(table(result$educ32))

cat("\nSummary of educadtl32 (first 10 unique):\n")
print(unique(head(result$educadtl32, 10)))

cat("\nSummary of educvdtl32 (first 10 unique):\n")
print(unique(head(result$educvdtl32, 10)))
}
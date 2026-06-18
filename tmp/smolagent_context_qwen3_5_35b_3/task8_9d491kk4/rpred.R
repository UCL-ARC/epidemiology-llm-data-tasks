library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
ns8_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets
data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(ns8_main, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

# Define collapse function for NVQ scheme
collapse_nvq <- function(nvq) {
  case_when(
    nvq %in% c(4, 5, 95, 96) ~ 0,
    nvq %in% c(1, 2, 3) ~ 1,
    nvq == 0 ~ 2,
    TRUE ~ NA_real_
  )
}

# Step 1: Derive educ25 from W8DHANVQH and W8VCQU* variables
w8_dh <- data$W8DHANVQH

educ25_acad_raw <- case_when(
  w8_dh == 95 ~ 3,
  w8_dh == 96 ~ 4,
  w8_dh %in% c(1, 2, 3, 4, 5) ~ collapse_nvq(w8_dh),
  TRUE ~ w8_dh
)

# For vocational, process each variable sequentially
has_dk <- data$W8VCQU0Q == -8 | data$W8VCQU0R == -8
has_ref <- data$W8VCQU0Q == -9 | data$W8VCQU0R == -9

# Calculate vocational tiers
voc_I <- ifelse(data$W8VCQU0I == 1, 1, 0)
voc_J <- ifelse(data$W8VCQU0J == 1, 3, 0)
voc_K <- ifelse(data$W8VCQU0K == 1, 4, 0)
voc_L <- ifelse(data$W8VCQU0L == 1, 3, 0)
voc_M <- ifelse(data$W8VCQU0M == 1, 3, 0)
voc_N <- ifelse(data$W8VCQU0N == 1, 3, 0)
voc_O <- ifelse(data$W8VCQU0O == 1, 3, 0)

# Maximum tier
voc_max <- pmax(voc_I, voc_J, voc_K, voc_L, voc_M, voc_N, voc_O, na.rm = TRUE)

# Check for "None of the above"
none_above <- ifelse(data$W8VCQU0P == 1, 4, 0)

educ25_voc_raw <- case_when(
  has_dk ~ -8,
  has_ref ~ -9,
  voc_max > 0 ~ voc_max,
  none_above == 4 ~ 4,
  TRUE ~ 2
)

# Combine academic and vocational
educ25_raw <- case_when(
  !is.na(educ25_acad_raw) & !is.na(educ25_voc_raw) ~ pmax(educ25_acad_raw, educ25_voc_raw, na.rm = TRUE),
  !is.na(educ25_acad_raw) ~ educ25_acad_raw,
  TRUE ~ educ25_voc_raw
)

# Convert to factor with collapsed scheme labels
educ25 <- factor(educ25_raw,
  levels = c(0, 1, 2, 3, 4, -9, -8, -1, -3),
  labels = c("NVQ 4-5 equivalent", "NVQ 1-3 equivalent", 
             "Entry level or no qualifications", 
             "Other qualifications not mappable to the NVQ framework",
             "None of these qualifications",
             "Refused", "Don\'t know", "Not applicable", "Not asked at fieldwork stage")
)

# Step 2: Derive educ32 from W9DANVQH and W9DVNVQH
w9_danvqh <- data$W9DANVQH
w9_dvnvqh <- data$W9DVNVQH

educ32_acad_raw <- case_when(
  w9_danvqh == 95 ~ 3,
  w9_danvqh == 96 ~ 4,
  w9_danvqh %in% c(0, 1, 2, 3, 4, 5) ~ collapse_nvq(w9_danvqh),
  TRUE ~ w9_danvqh
)

educ32_voc_raw <- case_when(
  w9_dvnvqh == 95 ~ 3,
  w9_dvnvqh == 96 ~ 4,
  w9_dvnvqh %in% c(0, 1, 2, 3, 4, 5) ~ collapse_nvq(w9_dvnvqh),
  TRUE ~ w9_dvnvqh
)

# Combine - retain higher level, preserve actual negative codes
educ32_raw <- case_when(
  !is.na(educ32_acad_raw) & !is.na(educ32_voc_raw) ~ pmax(educ32_acad_raw, educ32_voc_raw, na.rm = TRUE),
  !is.na(educ32_acad_raw) ~ educ32_acad_raw,
  TRUE ~ educ32_voc_raw
)

educ32 <- factor(educ32_raw,
  levels = c(0, 1, 2, 3, 4, -9, -8, -1, -3),
  labels = c("NVQ 4-5 equivalent", "NVQ 1-3 equivalent",
             "Entry level or no qualifications",
             "Other qualifications not mappable to the NVQ framework",
             "None of these qualifications",
             "Refused", "Don\'t know", "Not applicable", "Not asked at fieldwork stage")
)

# Step 3: Derive educadtl32 (detailed academic qualifications at age 32)
ac_vars <- c("W9ACQU0A", "W9ACQU0B", "W9ACQU0C", "W9ACQU0D", "W9ACQU0E", "W9ACQU0F",
             "W9ACQU0G", "W9ACQU0H", "W9ACQU0I", "W9ACQU0J", "W9ACQU0K", "W9ACQU0L",
             "W9ACQU0M", "W9ACQU0N", "W9ACQU0O", "W9ACQU0P", "W9ACQU0Q", "W9ACQU0R",
             "W9ACQU0S", "W9ACQU0T", "W9ACQU0U", "W9ACQU0V")

ac_labels <- c("Doctorate or equivalent", "Masters or equivalent",
               "Undergraduate or equivalent", "Post-graduate Diplomas and Certificates",
               "Diplomas in higher education and other higher education qualifications",
               "Teaching qualifications for schools or further education (below degree level)",
               "A/AS Levels or equivalent", "Grade A-C, Level 4-9",
               "Grade D-G, Level 1-3", "SCE Higher", "Scottish Certificate Sixth Year Studies",
               "SCE Standard", "National 4 and 5", "National 2 and 3",
               "Leaving Certificate", "Junior Certificate grade A-C",
               "Junior Certificate grade D and below", "Other academic qualifications (including overseas)",
               "None of these qualifications", "Don\'t know", "Refused", "No answer")

# Extract all academic variables into a matrix
ac_matrix <- as.matrix(data[, ac_vars])

# Function to code one row
code_ac_row <- function(row) {
  substantive_n <- 19
  
  # Check for all -1 case (all substantive are -1)
  substantive_vals <- row[1:substantive_n]
  if (all(substantive_vals == -1, na.rm = TRUE) && sum(!is.na(substantive_vals)) > 0) {
    return(-1)
  }
  
  # Check if all substantive are NA or -3
  if (all(is.na(substantive_vals) | substantive_vals == -3)) {
    return(-3)
  }
  
  # Process variables in order
  code <- 0
  
  for (j in seq_along(row)) {
    val <- row[j]
    
    # Skip NA
    if (is.na(val)) next
    
    # Check for -1
    if (val == -1) return(-1)
    
    # Non-substantive codes
    if (val == -8) return(-8)
    if (val == -9) return(-9)
    if (val == -2) return(-2)  # No answer
    
    # Substantive - Yes (code 1)
    if (val == 1) {
      code <- code + 1
    }
  }
  
  # If no substantive "Yes" found, all substantive are "No" (2)
  # Assign "None" code as next integer after last substantive
  if (code == 0) {
    return(20)  # 19 substantive + 1 for "None"
  }
  
  return(code)
}

# Apply to all rows
educadtl32_raw <- apply(ac_matrix, 1, code_ac_row)

# Create labels for factor
ac_sub_labels <- ac_labels[1:19]  # 19 substantive variables
ac_missing_labels <- c("Don\'t know", "Refused", "No answer", "Not asked at fieldwork stage")

educadtl32 <- factor(educadtl32_raw,
  levels = c(1:20, -8, -9, -2, -3),
  labels = c(ac_sub_labels, "None of these qualifications", ac_missing_labels)
)

# Step 4: Derive educvdtl32 (detailed vocational qualifications at age 32)
vc_vars <- c("W9VCQU0A", "W9VCQU0B", "W9VCQU0C", "W9VCQU0D", "W9VCQU0E", "W9VCQU0F",
             "W9VCQU0G", "W9VCQU0H", "W9VCQU0I", "W9VCQU0J", "W9VCQU0K", "W9VCQU0L",
             "W9VCQU0M", "W9VCQU0N", "W9VCQU0O", "W9VCQU0P", "W9VCQU0Q", "W9VCQU0R",
             "W9VCQU0S", "W9VCQU0T", "W9VCQU0U", "W9VCQU0V", "W9VCQU0W", "W9VCQU0X",
             "W9VCQU0Y", "W9VCQU0Z", "W9VCQUAA", "W9VCQUAB", "W9VCQUAC", "W9VCQUAD",
             "W9VCQUAE", "W9VCQUAF", "W9VCQUAG", "W9VCQUAH", "W9VCQUAI")

vc_labels <- c("Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor",
               "Nursing or other medical qualifications (below degree level)",
               "Level 4 or 5", "Level 3", "Level 2", "Level 1",
               "GNVQ Advanced", "GNVQ Intermediate", "Level 3", "Level 2",
               "Level Foundation", "Advanced Craft, Part III", "Craft, Part II",
               "Craft, Part I", "Level 3", "Level 2", "Level 1",
               "Advanced Diploma", "Higher Diploma", "RSA Diploma",
               "RSA Stage I, II,III", "Higher Level BTEC", "BTEC National",
               "BTEC First", "SCOTVEC National Certificate", "SCOTVEC first or general diploma",
               "SCOTVEC general diploma", "SCOTVEC modules", "HND or HNC",
               "OND or ONCM", "Junior certificate", "Other vocational qualifications (including some overseas)",
               "None of these qualifications", "Don\'t know", "Refused")

# Extract all vocational variables into a matrix
vc_matrix <- as.matrix(data[, vc_vars])

# Function to code one row (no No answer for vocational)
code_vc_row <- function(row) {
  substantive_n <- 19
  
  # Check for all -1 case (all substantive are -1)
  substantive_vals <- row[1:substantive_n]
  if (all(substantive_vals == -1, na.rm = TRUE) && sum(!is.na(substantive_vals)) > 0) {
    return(-1)
  }
  
  # Check if all substantive are NA or -3
  if (all(is.na(substantive_vals) | substantive_vals == -3)) {
    return(-3)
  }
  
  # Process variables in order
  code <- 0
  
  for (j in seq_along(row)) {
    val <- row[j]
    
    # Skip NA
    if (is.na(val)) next
    
    # Check for -1
    if (val == -1) return(-1)
    
    # Non-substantive codes (no -2 for vocational)
    if (val == -8) return(-8)
    if (val == -9) return(-9)
    
    # Substantive - Yes (code 1)
    if (val == 1) {
      code <- code + 1
    }
  }
  
  # If no substantive "Yes" found, all substantive are "No" (2)
  # Assign "None" code as next integer after last substantive
  if (code == 0) {
    return(20)  # 19 substantive + 1 for "None"
  }
  
  return(code)
}

# Apply to all rows
educvdtl32_raw <- apply(vc_matrix, 1, code_vc_row)

# Create labels for factor
vc_sub_labels <- vc_labels[1:19]  # 19 substantive variables
vc_missing_labels <- c("Don\'t know", "Refused", "Not asked at fieldwork stage")

educvdtl32 <- factor(educvdtl32_raw,
  levels = c(1:20, -8, -9, -3),
  labels = c(vc_sub_labels, "None of these qualifications", vc_missing_labels)
)

# Create final output with only ID and derived variables
output <- data.frame(
  NSID = data$NSID,
  educ25 = educ25,
  educ32 = educ32,
  educadtl32 = educadtl32,
  educvdtl32 = educvdtl32,
  stringsAsFactors = FALSE
)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Dimensions:", nrow(output), "rows x", ncol(output), "columns\n")
cat("Variables:", paste(names(output), collapse = ", "), "\n")

cat("\nSample data:\n")
print(head(output))

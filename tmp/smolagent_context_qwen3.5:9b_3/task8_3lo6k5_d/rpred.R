library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Define input files
input_files <- c(
  "ns8_2015_main_interview.tab" = "data/input/ns8_2015_main_interview.tab",
  "ns8_2015_derived.tab" = "data/input/ns8_2015_derived.tab",
  "ns9_2022_main_interview.tab" = "data/input/ns9_2022_main_interview.tab",
  "ns9_2022_derived_variables.tab" = "data/input/ns9_2022_derived_variables.tab"
)

# Load all files
ls8_main <- read_delim(input_files[[1]], delim = "\t", col_types = cols(.default = "c"))
ls8_derived <- read_delim(input_files[[2]], delim = "\t", col_types = cols(.default = "c"))
ls9_main <- read_delim(input_files[[3]], delim = "\t", col_types = cols(.default = "c"))
ls9_derived <- read_delim(input_files[[4]], delim = "\t", col_types = cols(.default = "c"))

# Merge files
data_all <- ls8_main %>%
  full_join(ls8_derived, by = "NSID") %>%
  full_join(ls9_main, by = "NSID") %>%
  full_join(ls9_derived, by = "NSID")

# Get W8 vocational variable names
w8_voc_vars <- grep("^W8VCQU", names(data_all), value = TRUE)

# Get W9 academic variable names
w9_acqu_vars <- grep("^W9ACQU", names(data_all), value = TRUE)

# Get W9 vocational variable names
w9_vcqu_vars <- grep("^W9VCQU", names(data_all), value = TRUE)

# Get W9 derived variable names
w9_danvqh <- "W9DANVQH"
w9_dvnvqh <- "W9DVNVQH"
w8_dhanvqh <- "W8DHANVQH"

# Mapping for W8DHANVQH to our 0-4 scale (based on requirements)
# 0: NVQ 4-5 equivalent -> codes 4,5 map to 0
# 1: NVQ 1-3 equivalent -> codes 1,2,3 map to 1
# 2: Entry level or no qualifications -> N/A for W8DHANVQH
# 3: Other qualifications -> code 95 maps to 3
# 4: None of these qualifications -> code 96 maps to 4

map_w8_dhanvqh <- function(x) {
  if (is.na(x)) return(NA_integer_)
  if (x %in% c(-9, -8, -3, -2, -1)) return(x)
  if (x == 96) return(4)
  if (x == 95) return(3)
  if (x >= 4 && x <= 5) return(0)
  if (x >= 1 && x <= 3) return(1)
  return(x)
}

# Map W8DHANVQH
educ25_w8dh <- map_w8_dhanvqh(as.integer(data_all[[w8_dhanvqh]]))

# Function to classify vocational qualifications
# Returns: 0 = NVQ 4-5, 1 = NVQ 1-3, 2 = Entry level, NA = no valid qualification
classify_voc_level <- function(row) {
  voc_vals <- row[w8_voc_vars]
  
  # Find "Yes" responses (value = 1)
  yes_indices <- which(voc_vals == 1)
  
  if (length(yes_indices) == 0) {
    return(NA_integer_)
  }
  
  # NVQ 4-5 equivalents
  nvq45_names <- c("HNC/HND", "NVQ/SVQ - Level 3 - 5")
  # NVQ 1-3 equivalents
  nvq13_names <- c("NVQ/SVQ - Level 1 - 2")
  # Entry level qualifications
  entry_names <- c("Youth training certificate", "Key Skills", "Basic skills", "Entry level qualifications (Wales)", 
                   "Modern apprenticeship/trade apprenticeship", "RSA/OCR/Clerical and commercial qualifications",
                   "City and Guilds Certificate", "GNVQ/GSVQ", "Other vocational, technical or professional")
  
  # Check for highest level first
  for (idx in yes_indices) {
    var_name <- w8_voc_vars[idx]
    if (var_name %in% nvq45_names) return(0)
    if (var_name %in% nvq13_names) return(1)
    if (var_name %in% entry_names) return(2)
  }
  return(NA_integer_)
}

# Create matrix for W8 vocational data
w8_voc_matrix <- as.matrix(data_all[, w8_voc_vars])
w8_voc_matrix[is.na(w8_voc_matrix)] <- 0  # Treat NA as No for classification

educ25_voc <- apply(w8_voc_matrix, 1, classify_voc_level)

# Combine W8DHANVQH and W8 vocational: take highest valid level
# Rules: When both have valid qualifications, take the highest
# If one is NA, use the other
combine_levels <- function(academic, vocational) {
  if (is.na(academic) && is.na(vocational)) return(NA_integer_)
  if (is.na(vocational)) return(academic)
  if (is.na(academic)) return(vocational)
  
  # Both have values
  # If either is 4 (None of these) and other is valid, use the valid one
  # If both indicate "None of these" (4), return 4
  if (academic == 4 && vocational == 4) return(4)
  if (academic == 4) return(vocational)  # vocational is valid
  if (vocational == 4) return(academic)  # academic is valid
  
  # Both are valid qualifications (not 4), take higher
  return(max(academic, vocational))
}

educ25 <- combine_levels(educ25_w8dh, educ25_voc)

# Handle final missing values
educ25[educ25 == -9 | educ25 == -8 | educ25 == -3 | educ25 == -2 | educ25 == -1] <- -9  # Consolidate to -9 for display

# Mapping for W9 derived variables (same structure but with 0 = Entry Level)
map_w9_derived <- function(x) {
  if (is.na(x)) return(NA_integer_)
  if (x %in% c(-9, -8, -1)) return(x)
  if (x == 96) return(4)
  if (x == 95) return(3)
  if (x >= 4 && x <= 5) return(0)
  if (x == 0) return(2)
  if (x >= 1 && x <= 3) return(1)
  return(x)
}

educ32_w9dan <- map_w9_derived(as.integer(data_all[[w9_danvqh]]))
educ32_w9dv <- map_w9_derived(as.integer(data_all[[w9_dvnvqh]]))

# Combine W9 academic and vocational qualifications
educ32 <- combine_levels(educ32_w9dan, educ32_w9dv)

# Clean up missing codes
educ32[educ32 == -9 | educ32 == -8 | educ32 == -1] <- -9

# Create educadtl32: Map first "Yes" from W9ACQU* to descriptive label
get_acqu_label <- function(row) {
  acqu_vals <- row[w9_acqu_vars]
  
  # Check for Yes (value = 1) in priority order
  for (var in w9_acqu_vars) {
    if (acqu_vals[var] == 1) {
      # Extract the descriptive part by removing prefix
      # Variable names are like W9ACQU0A, W9ACQU0B, etc.
      # The label in metadata starts after "Academic qualifications gained - "
      base <- sub("W9ACQU", "", var)
      # Map code to label based on priority
      acqu_labels <- c(
        "Doctorate or equivalent", "Masters or equivalent", "Undergraduate or equivalent",
        "Post-graduate Diplomas and Certificates", "Diplomas in higher education and other higher education qualifications",
        "Teaching qualifications for schools or further education (below degree level)", "A/AS Levels or equivalent",
        "Grade A-C, Level 4-9", "Grade D-G, Level 1-3", "SCE Higher", "Scottish Certificate Sixth Year Studies",
        "SCE Standard", "National 4 and 5", "National 2 and 3", "Leaving Certificate",
        "Junior Certificate grade A-C", "Junior Certificate grade D and below",
        "Other academic qualifications (including overseas)", "None of these qualifications"
      )
      return(acqu_labels[base])
    }
  }
  
  # Check for Don't know, Refused, No answer - these are missing-like and should be preserved
  for (var in w9_acqu_vars) {
    if (acqu_vals[var] == -3 || acqu_vals[var] == -1) {
      return(acqu_vals[var])
    }
  }
  
  # If no Yes found and no missing-like codes, check if all are No
  if (all(acqu_vals[w9_acqu_vars] == 2)) {
    return("None of these qualifications")
  }
  
  # Return first missing-like value if any
  missing_vals <- c(-3, -1)
  for (var in w9_acqu_vars) {
    if (acqu_vals[var] %in% missing_vals) return(acqu_vals[var])
  }
  
  return(NA_character_)
}

acqu_matrix <- as.matrix(data_all[, w9_acqu_vars])
acqu_matrix[is.na(acqu_matrix)] <- 0
educadtl32 <- apply(acqu_matrix, 1, get_acqu_label)

# Create educvdtl32: Map first "Yes" from W9VCQU* to descriptive label
get_vcqu_label <- function(row) {
  vcqu_vals <- row[w9_vcqu_vars]
  
  for (var in w9_vcqu_vars) {
    if (vcqu_vals[var] == 1) {
      base <- sub("W9VCQU", "", var)
      vcqu_labels <- c(
        "Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor",
        "Nursing or other medical qualifications (below degree level)", "Level 4 or 5", "Level 3",
        "Level 2", "Level 1", "GNVQ Advanced", "GNVQ Intermediate", "Level 3",
        "Level 2", "Level Foundation", "Advanced Craft, Part III", "Craft, Part II", "Craft, Part I",
        "Level 3", "Level 2", "Level 1", "Advanced Diploma", "Higher Diploma", "RSA Diploma",
        "RSA Stage I, II,III", "Higher Level BTEC", "BTEC National", "BTEC First",
        "SCOTVEC National Certificate", "SCOTVEC first or general diploma", "SCOTVEC general diploma",
        "SCOTVEC modules", "HND or HNC", "OND or ONCM", "Junior certificate",
        "Other vocational qualifications (including some overseas)", "None of these qualifications"
      )
      return(vcqu_labels[base])
    }
  }
  
  for (var in w9_vcqu_vars) {
    if (vcqu_vals[var] %in% c(-3, -1)) return(vcqu_vals[var])
  }
  
  if (all(vcqu_vals[w9_vcqu_vars] == 2)) {
    return("None of these qualifications")
  }
  
  return(NA_character_)
}

vcqu_matrix <- as.matrix(data_all[, w9_vcqu_vars])
vcqu_matrix[is.na(vcqu_matrix)] <- 0
educvdtl32 <- apply(vcqu_matrix, 1, get_vcqu_label)

# Create final dataset with exactly 5 variables
final_data <- data.frame(
  NSID = as.character(data_all$NSID),
  educ25 = as.character(educ25),
  educ32 = as.character(educ32),
  educadtl32 = as.character(educadtl32),
  educvdtl32 = as.character(educvdtl32)
)

# Write to CSV
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)

cat("Script completed successfully.\n")
cat("Output written to data/output/cleaned_data.csv\n")
cat("Dimensions:", nrow(final_data), " x ", ncol(final_data), "\n")

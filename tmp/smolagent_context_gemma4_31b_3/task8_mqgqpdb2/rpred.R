library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_main_interview.tab',
  'ns9_2022_derived_variables.tab'
)

all_data <- list()
for (f in files) {
  all_data[[f]] <- read_delim(paste0('data/input/', f), delim = "\t", col_types = cols(.default = "c"))
}

# Merge all datasets
df <- all_data[[1]]
for (i in 2:length(all_data)) {
  df <- full_join(df, all_data[[i]], by = "NSID")
}

# Convert numeric columns to numeric
df <- df %>% mutate(across(everything(), ~ type.convert(., as.is = TRUE)))

# --- Derivation Logic for educ25 ---
# W8DHANVQH mapping to NVQ scheme
# 1: L1, 2: L2, 3: L3, 4: L4, 5: L5, 95: Other, 96: None
# Collapsed Scheme: 0 = NVQ 4-5, 1 = NVQ 1-3, 2 = Entry, 3 = Other, 4 = None

map_nvq_to_collapsed <- function(val) {
  if (is.na(val)) return(NA)
  if (val == 4 || val == 5) return(0)
  if (val == 1 || val == 2 || val == 3) return(1)
  if (val == 95) return(3)
  if (val == 96) return(4)
  return(NA)
}

# Vocational tick-boxes W8VCQU0A-R
# Mapping based on labels (inference: HNC/HND/L4-5 -> 0, others -> 1, etc.)
# Detailed mapping for W8VCQU variables
voc_map_w8 <- list(
  "W8VCQU0K" = 0, "W8VCQU0L" = 0, "W8VCQU0J" = 1, "W8VCQU0H" = 1, "W8VCQU0I" = 1,
  "W8VCQU0A" = 1, "W8VCQU0B" = 1, "W8VCQU0C" = 1, "W8VCQU0D" = 2, "W8VCQU0E" = 1,
  "W8VCQU0F" = 1, "W8VCQU0G" = 1, "W8VCQU0M" = 1, "W8VCQU0N" = 1, "W8VCQU0O" = 3,
  "W8VCQU0P" = 4
)

# We need to find the highest qualification
# NVQ levels: 0 (highest) < 1 < 2 < 3 < 4

calc_educ25 <- function(row) {
  highest <- 4
  has_valid <- FALSE
  
  # Academic
  acad_val <- map_nvq_to_collapsed(row[['W8DHANVQH']])
  if (!is.na(acad_val)) {
    highest <- min(highest, acad_val)
    has_valid <- TRUE
  }
  
  # Vocational
  for (var in names(voc_map_w8)) {
    if (!is.na(row[[var]]) && row[[var]] == 1) {
      highest <- min(highest, voc_map_w8[[var]])
      has_valid <- TRUE
    }
  }
  
  # Special missing for vocational
  if (!has_valid) {
    if (!is.na(row[['W8VCQU0Q']]) && row[['W8VCQU0Q']] == 1) return(-8)
    if (!is.na(row[['W8VCQU0R']]) && row[['W8VCQU0R']] == 1) return(-9)
    return(-3)
  }
  
  return(highest)
}

df$educ25 <- apply(df, 1, calc_educ25)

# --- Derivation Logic for educ32 ---
# Combine W9DANVQH and W9DVNVQH
map_nvq_32 <- function(val) {
  if (is.na(val)) return(NA)
  if (val == 4 || val == 5) return(0)
  if (val == 1 || val == 2 || val == 3) return(1)
  if (val == 0) return(2)
  if (val == 95) return(3)
  if (val == 96) return(4)
  return(val) # preserve negative codes
}

calc_educ32 <- function(row) {
  a <- map_nvq_32(row[['W9DANVQH']])
  v <- map_nvq_32(row[['W9DVNVQH']])
  
  # If both valid substantive
  if (!is.na(a) && a >= 0 && !is.na(v) && v >= 0) return(min(a, v))
  if (!is.na(a) && a >= 0) return(a)
  if (!is.na(v) && v >= 0) return(v)
  
  # Missing codes
  if (!is.na(a) && a < 0) return(a)
  if (!is.na(v) && v < 0) return(v)
  return(-3)
}

df$educ32 <- apply(df, 1, calc_educ32)

# --- Derivation Logic for educadtl32 ---
acad_vars <- c("W9ACQU0A", "W9ACQU0B", "W9ACQU0C", "W9ACQU0D", "W9ACQU0E", "W9ACQU0F", "W9ACQU0G", "W9ACQU0H", "W9ACQU0I", "W9ACQU0J", "W9ACQU0K", "W9ACQU0L", "W9ACQU0M", "W9ACQU0N", "W9ACQU0O", "W9ACQU0P", "W9ACQU0Q", "W9ACQU0R", "W9ACQU0S")

calc_dtl <- function(row, vars, non_sub_map, prefix) {
  substantive_found <- FALSE
  idx <- 1
  
  for (v in vars) {
    val <- row[[v]]
    if (is.na(val)) next
    if (val == 1) {
      substantive_found <- TRUE
      return(idx)
    }
    if (val == 2) {
      # skip, but we need to track idx for substantive logic
      # The requirement says "assign integer code 1 for the first substantive indicator, code 2 for the next..."
      # Actually, it implies a sequential count of indicators that are 'Yes'
      # Wait: "assign integer code 1 for the first substantive indicator [that is Yes]" 
      # Let's refine: iterate through vars, if Yes, return current count of substantive vars seen so far?
      # No: "assign integer code 1 for the first substantive indicator, code 2 for the next..."
      # This usually means if the 1st var is Yes -> 1, if 1st is No and 2nd is Yes -> 1.
      # Let's re-read: "assign integer code 1 for the first substantive indicator, code 2 for the next, and so on"
      # This means if multiple are Yes, it's usually the highest. But the prompt says "first substantive indicator".
      # Let's assume it means: if W9ACQU0A==1 return 1, else if W9ACQU0B==1 return 2... no, that's not "first".
      # "first substantive indicator" usually refers to the order in metadata. 
      # If W9ACQU0A is Yes, that's the first. If W9ACQU0A is No and W9ACQU0B is Yes, that's the first one that is Yes.
      # Actually, the prompt says "assign integer code 1 for the first substantive indicator, code 2 for the next".
      # This implies if multiple are Yes, we might need a list? No, "assign integer code".
      # Usually, this means: find the first one that is 'Yes' and return its position.
      # Let's check the 'None' rule: "when all substantive indicators are No (2), assign the 'None of these qualifications' code (the integer after the last substantive code)".
      # This means if there are 19 substantive vars, and all are No, return 20.
      # So if the first 'Yes' is at position 5, return 5.
    }
  }
}

# Corrected dtl logic
process_dtl <- function(row, vars, non_sub_vars) {
  # 1. Check for non-substantive first? No, usually substantive takes priority or is checked in order.
  # Let's follow: "scan the... tick-box variables in metadata order"
  
  # Substantive indicators are those not in non_sub_vars
  sub_vars <- vars[!(vars %in% non_sub_vars)]
  
  # Find first 'Yes'
  for (i in 1:length(sub_vars)) {
    val <- row[[sub_vars[i]]]
    if (!is.na(val) && val == 1) return(i)
  }
  
  # If all are No (2) or missing (but not -1)
  all_no <- TRUE
  for (v in sub_vars) {
    val <- row[[v]]
    if (is.na(val) || val != 2) { all_no <- FALSE; break }
  }
  if (all_no) return(length(sub_vars) + 1)
  
  # Non-substantive mapping
  for (v in non_sub_vars) {
    val <- row[[v]]
    if (!is.na(val) && val == 1) {
      if (grepl("Don't know", v) || (v == "W9ACQU0T")) return(-8)
      if (grepl("Refused", v) || (v == "W9ACQU0U")) return(-9)
      if (grepl("No answer", v) || (v == "W9ACQU0V")) return(-2)
    }
  }
  
  # -1 and missing
  # Check if any -1 exists
  for (v in vars) {
    if (!is.na(row[[v]]) && row[[v]] == -1) return(-1)
  }
  
  return(-3)
}

# We need to apply this to academic and vocational
acad_all_vars <- c("W9ACQU0A", "W9ACQU0B", "W9ACQU0C", "W9ACQU0D", "W9ACQU0E", "W9ACQU0F", "W9ACQU0G", "W9ACQU0H", "W9ACQU0I", "W9ACQU0J", "W9ACQU0K", "W9ACQU0L", "W9ACQU0M", "W9ACQU0N", "W9ACQU0O", "W9ACQU0P", "W9ACQU0Q", "W9ACQU0R", "W9ACQU0S", "W9ACQU0T", "W9ACQU0U", "W9ACQU0V")
acad_non_sub <- c("W9ACQU0T", "W9ACQU0U", "W9ACQU0V")

voc_all_vars <- c("W9VCQU0A", "W9VCQU0B", "W9VCQU0C", "W9VCQU0D", "W9VCQU0E", "W9VCQU0F", "W9VCQU0G", "W9VCQU0H", "W9VCQU0I", "W9VCQU0J", "W9VCQU0K", "W9VCQU0L", "W9VCQU0M", "W9VCQU0N", "W9VCQU0O", "W9VCQU0P", "W9VCQU0Q", "W9VCQU0R", "W9VCQU0S", "W9VCQU0T", "W9VCQU0U", "W9VCQU0V", "W9VCQU0W", "W9VCQU0X", "W9VCQU0Y", "W9VCQU0Z", "W9VCQUAA", "W9VCQUAB", "W9VCQUAC", "W9VCQUAD", "W9VCQUAE", "W9VCQUAF", "W9VCQUAG", "W9VCQUAH", "W9VCQUAI")
voc_non_sub <- c("W9VCQUAH", "W9VCQUAI")

df$educadtl32 <- apply(df, 1, function(r) process_dtl(r, acad_all_vars, acad_non_sub))
df$educvdtl32 <- apply(df, 1, function(r) process_dtl(r, voc_all_vars, voc_non_sub))

# --- Final Labeling ---
# educ25 & educ32
nvq_labels <- c("0" = "NVQ 4–5 equivalent", "1" = "NVQ 1–3 equivalent", "2" = "Entry level or no qualifications", "3" = "Other qualifications not mappable to the NVQ framework", "4" = "None of these qualifications", "-9" = "Refused", "-8" = "Don't know / insufficient information", "-7" = "Prefer not to say", "-3" = "Not asked at the fieldwork stage / not interviewed", "-2" = "Schedule not applicable / script error / information lost", "-1" = "Item not applicable")

df$educ25 <- factor(df$educ25, levels = as.numeric(names(nvq_labels)), labels = nvq_labels)
df$educ32 <- factor(df$educ32, levels = as.numeric(names(nvq_labels)), labels = nvq_labels)

# Labels for dtl
get_dtl_labels <- function(vars, non_sub_vars, prefix) {
  sub_vars <- vars[!(vars %in% non_sub_vars)]
  labels <- c()
  for (v in sub_vars) {
    # Need to get variable_label from metadata
    # This is hard in script, we will manually provide based on the provided metadata
    # I will use a placeholder and refine if needed, but I have the metadata in the prompt
  }
}

# Since I can't easily access the metadata dictionary inside the R loop without redefining it,
# I will define the labels manually based on the prompt's metadata list.

acad_labels_raw <- c(
  "Doctorate or equivalent", "Masters or equivalent", "Undergraduate or equivalent", "Post-graduate Diplomas and Certificates", "Diplomas in higher education and other higher education qualifications", "Teaching qualifications for schools or further education (below degree level)", "A/AS Levels or equivalent", "Grade A-C, Level 4-9", "Grade D-G, Level 1-3", "SCE Higher", "Scottish Certificate Sixth Year Studies", "SCE Standard", "National 4 and 5", "National 2 and 3", "Leaving Certificate", "Junior Certificate grade A-C", "Junior Certificate grade D and below", "Other academic qualifications (including overseas)", "None of these qualifications"
)

voc_labels_raw <- c(
  "Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor", "Nursing or other medical qualifications (below degree level)", "Level 4 or 5", "Level 3", "Level 2", "Level 1", "GNVQ Advanced", "GNVQ Intermediate", "Level 3", "Level 2", "Level Foundation", "Advanced Craft, Part III", "Craft, Part II", "Craft, Part I", "Level 3", "Level 2", "Level 1", "Advanced Diploma", "Higher Diploma", "RSA Diploma", "RSA Stage I, II,III", "Higher Level BTEC", "BTEC National", "BTEC First", "SCOTVEC National Certificate", "SCOTVEC first or general diploma", "SCOTVEC general diploma", "SCOTVEC modules", "HND or  HNC", "OND or ONCM", "Junior certificate", "Other vocational qualifications (including some overseas)", "None of these qualifications"
)

full_acad_labels <- c(acad_labels_raw, "Don't know", "Refused", "No answer")
# Wait, the logic was: 1..N are substantive, N+1 is 'None', then missing codes.
# Let's adjust the labels to match the integer codes generated by process_dtl.

# The codes produced: 1..18 (substantive), 19 (None), -8, -9, -2, -1, -3
# Re-mapping to factor

final_acad_labels <- setNames(c(acad_labels_raw, "Don't know", "Refused", "No answer"), 
                               c(1:19, -8, -9, -2))
# Let's just use the raw labels and append the missing ones

# For a safer approach, let's create the factor manually
df$educadtl32 <- factor(df$educadtl32, 
                        levels = c(1:19, -8, -9, -2, -1, -3), 
                        labels = c(acad_labels_raw, "Don't know", "Refused", "No answer", "Not applicable", "Not asked at fieldwork stage"))

df$educvdtl32 <- factor(df$educvdtl32, 
                        levels = c(1:33, -8, -9, -2, -1, -3), 
                        labels = c(voc_labels_raw, "Don't know", "Refused", "No answer", "Not applicable", "Not asked at fieldwork stage"))

# Final Selection
final_df <- df %>% select(NSID, educ25, educ32, educadtl32, educvdtl32)
write_csv(final_df, "data/output/cleaned_data.csv")

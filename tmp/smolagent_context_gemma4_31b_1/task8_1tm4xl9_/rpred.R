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
  all_data[[f]] <- read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character()))
}

# Merge datasets
df <- all_data[[1]]
for (i in 2:length(all_data)) {
  df <- full_join(df, all_data[[i]], by = 'NSID')
}

# Define NVQ mapping for W8VCQU variables
# NVQ 4-5 equivalent: HNC/HND, ONC/OND
# NVQ 1-3 equivalent: Youth training, Key Skills, Basic skills, Entry level (Wales), Modern apprenticeship, RSA/OCR, City and Guilds, GNVQ/GSVQ, NVQ L1-2, NVQ L3-5, BTEC/BEC, SCOTVEC
# Entry level or no qualifications: None of the above
# Other: Other vocational

voc_mapping_w8 <- list(
  nvq45 = c('W8VCQU0K', 'W8VCQU0L'),
  nvq13 = c('W8VCQU0A', 'W8VCQU0B', 'W8VCQU0C', 'W8VCQU0D', 'W8VCQU0E', 'W8VCQU0F', 'W8VCQU0G', 'W8VCQU0H', 'W8VCQU0I', 'W8VCQU0J', 'W8VCQU0M', 'W8VCQU0N'),
  entry = c('W8VCQU0P'),
  other = c('W8VCQU0O')
)

# --- Derive educ25 ---
calc_educ25 <- function(row) {
  # Academic NVQ from W8DHANVQH
  acad_nvq <- row[['W8DHANVQH']]
  
  # Vocational NVQ from W8VCQU variables
  voc_nvq <- 4 # Default to 'None of these' (since we look for highest)
  
  # Check for Refused/Don't know in vocational tick-boxes
  if (!is.na(row[['W8VCQU0R']]) && row[['W8VCQU0R']] == 1) return(-9)
  if (!is.na(row[['W8VCQU0Q']]) && row[['W8VCQU0Q']] == 1) return(-8)
  
  # Check for substantive qualifications (1 = Yes)
  # Highest tier first: NVQ 4-5
  if (any(sapply(voc_mapping_w8$nvq45, function(v) !is.na(row[[v]]) && row[[v]] == 1))) {
    voc_nvq <- 0
  } else if (any(sapply(voc_mapping_w8$nvq13, function(v) !is.na(row[[v]]) && row[[v]] == 1))) {
    voc_nvq <- 1
  } else if (any(sapply(voc_mapping_w8$entry, function(v) !is.na(row[[v]]) && row[[v]] == 1))) {
    voc_nvq <- 2
  } else if (any(sapply(voc_mapping_w8$other, function(v) !is.na(row[[v]]) && row[[v]] == 1))) {
    voc_nvq <- 3
  } else {
    voc_nvq <- 4
  }
  
  # Map academic NVQ (W8DHANVQH)
  # 1-3 -> 1, 4-5 -> 0, 95 -> 3, 96 -> 4, others (missing) -> skip
  acad_val <- NA
  if (!is.na(acad_nvq)) {
    if (acad_nvq >= 4 && acad_nvq <= 5) acad_val <- 0
    else if (acad_nvq >= 1 && acad_nvq <= 3) acad_val <- 1
    else if (acad_nvq == 95) acad_val <- 3
    else if (acad_nvq == 96) acad_val <- 4
    else if (acad_nvq == -9) return(-9)
    else if (acad_nvq == -8) return(-8)
    else if (acad_nvq == -1) acad_val <- NA
  }
  
  # Retain highest qualification
  res <- min(acad_val, voc_nvq, na.rm = TRUE)
  if (is.infinite(res)) return(-3)
  return(res)
}

df$educ25 <- apply(df, 1, calc_educ25)

# --- Derive educ32 ---
calc_educ32 <- function(row) {
  acad <- row[['W9DANVQH']]
  voc <- row[['W9DVNVQH']]
  
  # Map to 0-4 scheme
  map_val <- function(v) {
    if (is.na(v)) return(NA)
    if (v >= 4 && v <= 5) return(0)
    if (v >= 0 && v <= 3) return(1)
    if (v == 96) return(4)
    if (v == 95) return(3)
    return(v) # Keep negative codes
  }
  
  a_m <- map_val(acad)
  v_m <- map_val(voc)
  
  # If both are valid substantive
  if (!is.na(a_m) && a_m >= 0 && !is.na(v_m) && v_m >= 0) {
    return(min(a_m, v_m))
  }
  # If only one is substantive
  if (!is.na(a_m) && a_m >= 0) return(a_m)
  if (!is.na(v_m) && v_m >= 0) return(v_m)
  
  # If neither is substantive, preserve the actual negative code
  # Priority: if one is missing (-3), take the other
  if (!is.na(a_m) && a_m != -3) return(a_m)
  if (!is.na(v_m) && v_m != -3) return(v_m)
  
  return(-3)
}

df$educ32 <- apply(df, 1, calc_educ32)

# --- Derive educadtl32 ---
acad_vars <- c(
  'W9ACQU0A', 'W9ACQU0B', 'W9ACQU0C', 'W9ACQU0D', 'W9ACQU0E', 'W9ACQU0F', 
  'W9ACQU0G', 'W9ACQU0H', 'W9ACQU0I', 'W9ACQU0J', 'W9ACQU0K', 'W9ACQU0L', 
  'W9ACQU0M', 'W9ACQU0N', 'W9ACQU0O', 'W9ACQU0P', 'W9ACQU0Q', 'W9ACQU0R', 'W9ACQU0S'
)
acad_labels <- c(
  'Doctorate or equivalent', 'Masters or equivalent', 'Undergraduate or equivalent', 
  'Post-graduate Diplomas and Certificates', 'Diplomas in higher education and other higher education qualifications', 
  'Teaching qualifications for schools or further education (below degree level)', 'A/AS Levels or equivalent', 
  'Grade A-C, Level 4-9', 'Grade D-G, Level 1-3', 'SCE Higher', 
  'Scottish Certificate Sixth Year Studies', 'SCE Standard', 'National 4 and 5', 
  'National 2 and 3', 'Leaving Certificate', 'Junior Certificate grade A-C', 
  'Junior Certificate grade D and below', 'Other academic qualifications (including overseas)', 'None of these qualifications'
)

calc_adtl32 <- function(row) {
  subst_count <- 0
  for (v in acad_vars) {
    val <- row[[v]]
    if (!is.na(val) && val == 1) {
      subst_count <- subst_count + 1
      return(subst_count)
    }
  }
  # If all substantive are 'No' (2), check if 'None' (W9ACQU0S) is Yes
  if (!is.na(row[['W9ACQU0S']]) && row[['W9ACQU0S']] == 1) return(length(acad_vars))
  
  # Non-substantive
  if (!is.na(row[['W9ACQU0T']]) && row[['W9ACQU0T']] == 1) return(-8)
  if (!is.na(row[['W9ACQU0U']]) && row[['W9ACQU0U']] == 1) return(-9)
  if (!is.na(row[['W9ACQU0V']]) && row[['W9ACQU0V']] == 1) return(-2)
  
  # -1 and -3
  # This is tricky; check the first variable for -1 or -3
  first_val <- row[['W9ACQU0A']]
  if (!is.na(first_val) && first_val == -1) return(-1)
  return(-3)
}

df$educadtl32 <- apply(df, 1, calc_adtl32)

# --- Derive educvdtl32 ---
voc_vars <- c(
  'W9VCQU0A', 'W9VCQU0B', 'W9VCQU0C', 'W9VCQU0D', 'W9VCQU0E', 'W9VCQU0F', 
  'W9VCQU0G', 'W9VCQU0H', 'W9VCQU0I', 'W9VCQU0J', 'W9VCQU0K', 'W9VCQU0L', 
  'W9VCQU0M', 'W9VCQU0N', 'W9VCQU0O', 'W9VCQU0P', 'W9VCQU0Q', 'W9VCQU0R', 
  'W9VCQU0S', 'W9VCQU0T', 'W9VCQU0U', 'W9VCQU0V', 'W9VCQU0W', 'W9VCQU0X', 
  'W9VCQU0Y', 'W9VCQU0Z', 'W9VCQUAA', 'W9VCQUAB', 'W9VCQUAC', 'W9VCQUAD', 
  'W9VCQUAE', 'W9VCQUAF', 'W9VCQUAG'
)
voc_labels <- c(
  'Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor', 
  'Nursing or other medical qualifications (below degree level)', 'Level 4 or 5', 'Level 3', 'Level 2', 'Level 1', 
  'GNVQ Advanced', 'GNVQ Intermediate', 'Level 3', 'Level 2', 'Level Foundation', 'Advanced Craft, Part III', 
  'Craft, Part II', 'Craft, Part I', 'Level 3', 'Level 2', 'Level 1', 'Advanced Diploma', 'Higher Diploma', 
  'RSA Diploma', 'RSA Stage I, II,III', 'Higher Level BTEC', 'BTEC National', 'BTEC First', 
  'SCOTVEC National Certificate', 'SCOTVEC first or general diploma', 'SCOTVEC general diploma', 'SCOTVEC modules', 
  'HND or  HNC', 'OND or ONCM', 'Junior certificate', 'Other vocational qualifications (including some overseas)', 
  'None of these qualifications'
)

calc_vdtl32 <- function(row) {
  subst_count <- 0
  for (v in voc_vars) {
    val <- row[[v]]
    if (!is.na(val) && val == 1) {
      subst_count <- subst_count + 1
      return(subst_count)
    }
  }
  if (!is.na(row[['W9VCQUAG']]) && row[['W9VCQUAG']] == 1) return(length(voc_vars))
  
  if (!is.na(row[['W9VCQUAH']]) && row[['W9VCQUAH']] == 1) return(-8)
  if (!is.na(row[['W9VCQUAI']]) && row[['W9VCQUAI']] == 1) return(-9)
  
  first_val <- row[['W9VCQU0A']]
  if (!is.na(first_val) && first_val == -1) return(-1)
  return(-3)
}

df$educvdtl32 <- apply(df, 1, calc_vdtl32)

# Final Factor Formatting
common_labels <- c('0' = 'NVQ 4–5 equivalent', '1' = 'NVQ 1–3 equivalent', '2' = 'Entry level or no qualifications', '3' = 'Other qualifications not mappable to the NVQ framework', '4' = 'None of these qualifications')

df$educ25 <- factor(df$educ25, levels = c(0,1,2,3,4, -9, -8, -7, -3, -2, -1), 
                    labels = c(common_labels[1:5], 'Refusal', 'Don\'t know', 'Prefer not to say', 'Not asked', 'Not applicable', 'Script error'))
df$educ32 <- factor(df$educ32, levels = c(0,1,2,3,4, -9, -8, -7, -3, -2, -1), 
                    labels = c(common_labels[1:5], 'Refusal', 'Don\'t know', 'Prefer not to say', 'Not asked', 'Not applicable', 'Script error'))

# Detailed Academic Labels
full_acad_labels <- c(acad_labels, 'Refusal', 'Don\'t know', 'Prefer not to say', 'Not asked', 'Not applicable', 'Script error')
df$educadtl32 <- factor(df$educadtl32, levels = c(1:length(acad_labels), -9, -8, -7, -3, -2, -1), labels = full_acad_labels)

# Detailed Vocational Labels
full_voc_labels <- c(voc_labels, 'Refusal', 'Don\'t know', 'Prefer not to say', 'Not asked', 'Not applicable', 'Script error')
df$educvdtl32 <- factor(df$educvdtl32, levels = c(1:length(voc_labels), -9, -8, -7, -3, -2, -1), labels = full_voc_labels)

# Save Output
write_csv(df %>% select(NSID, educ25, educ32, educadtl32, educvdtl32), 'data/output/cleaned_data.csv')

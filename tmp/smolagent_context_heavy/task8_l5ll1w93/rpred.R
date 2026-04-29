library(haven)
library(dplyr)
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

data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t', show_col_types = FALSE))
names(data_list) <- files

full_df <- data_list[[1]] %>% 
  select(NSID) %>% 
  full_join(data_list[[2]] %>% select(NSID), by = 'NSID') %>% 
  full_join(data_list[[3]], by = 'NSID') %>% 
  full_join(data_list[[4]], by = 'NSID') %>% 
  full_join(data_list[[5]], by = 'NSID') %>% 
  full_join(data_list[[6]], by = 'NSID')

# Helper for NVQ Mapping
map_nvq_level <- function(val) {
  if (is.na(val)) return(NA)
  if (val == 4 || val == 5) return(0)
  if (val == 1 || val == 2 || val == 3) return(1)
  if (val == 0) return(2)
  if (val == 95) return(3)
  if (val == 96) return(4)
  return(NA)
}

# 2. Derivation of educ25
calc_educ25 <- function(row) {
  acad_val <- as.numeric(row[['W8DHANVQH']])
  acad_mapped <- map_nvq_level(acad_val)
  
  voc_tier <- NA
  # Use %in% 1 to handle NAs safely
  if (row[['W8VCQU0K']] %in% 1 || row[['W8VCQU0L']] %in% 1) {
    voc_tier <- 0
  } else if (row[['W8VCQU0H']] %in% 1 || row[['W8VCQU0I']] %in% 1 || row[['W8VCQU0J']] %in% 1 || 
             row[['W8VCQU0M']] %in% 1 || row[['W8VCQU0N']] %in% 1) {
    voc_tier <- 1
  } else if (row[['W8VCQU0A']] %in% 1 || row[['W8VCQU0B']] %in% 1 || row[['W8VCQU0C']] %in% 1 || 
             row[['W8VCQU0D']] %in% 1) {
    voc_tier <- 2
  } else if (row[['W8VCQU0E']] %in% 1 || row[['W8VCQU0F']] %in% 1 || row[['W8VCQU0G']] %in% 1 || 
             row[['W8VCQU0O']] %in% 1) {
    voc_tier <- 3
  } else if (row[['W8VCQU0P']] %in% 1) {
    voc_tier <- 4
  }

  # Highest valid qualification (lowest numeric value = highest tier)
  res <- min(acad_mapped, voc_tier, na.rm = TRUE)
  if (is.infinite(res)) res <- NA
  
  if (is.na(res)) {
    if (row[['W8VCQU0R']] %in% 1) return(-9)
    if (row[['W8VCQU0Q']] %in% 1) return(-8)
    if (is.na(acad_val)) return(-3)
    return(map_nvq_level(acad_val))
  }
  
  return(res)
}

full_df$educ25 <- apply(full_df, 1, calc_educ25)

# 3. Derivation of educ32
calc_educ32 <- function(row) {
  a <- as.numeric(row[['W9DANVQH']])
  v <- as.numeric(row[['W9DVNVQH']])
  
  a_sub <- if(!is.na(a) && a >= 0) map_nvq_level(a) else NA
  v_sub <- if(!is.na(v) && v >= 0) map_nvq_level(v) else NA
  
  res <- min(a_sub, v_sub, na.rm = TRUE)
  if (is.infinite(res)) {
    if (!is.na(a) && a < 0) return(a)
    if (!is.na(v) && v < 0) return(v)
    return(-3)
  }
  return(res)
}

full_df$educ32 <- apply(full_df, 1, calc_educ32)

# 4. Derivation of educadtl32
calc_adtl32 <- function(row) {
  sub_vars <- paste0('W9ACQU0', LETTERS[1:18])
  found_idx <- NA
  
  for (i in 1:18) {
    val <- row[[sub_vars[i]]]
    if (!is.na(val) && val == 1) {
      found_idx <- i
      break
    }
  }
  
  if (!is.na(found_idx)) return(found_idx)
  
  all_no <- TRUE
  for (i in 1:18) {
    val <- row[[sub_vars[i]]]
    if (is.na(val) || val != 2) { all_no <- FALSE; break }
  }
  if (all_no) return(19)
  
  if (row[['W9ACQU0T']] %in% 1) return(-8)
  if (row[['W9ACQU0U']] %in% 1) return(-9)
  if (row[['W9ACQU0V']] %in% 1) return(-2)
  
  if (any(row[sub_vars] == -1, na.rm = TRUE)) return(-1)
  
  return(-3)
}

full_df$educadtl32_raw <- apply(full_df, 1, calc_adtl32)

# 5. Derivation of educvdtl32
calc_vdtl32 <- function(row) {
  sub_vars <- c('W9VCQU0A', 'W9VCQU0B', 'W9VCQU0C', 'W9VCQU0D', 'W9VCQU0E', 'W9VCQU0F', 'W9VCQU0G', 'W9VCQU0H', 'W9VCQU0I', 'W9VCQU0J', 'W9VCQU0K', 'W9VCQU0L', 'W9VCQU0M', 'W9VCQU0N', 'W9VCQU0O', 'W9VCQU0P', 'W9VCQU0Q', 'W9VCQU0R', 'W9VCQU0S', 'W9VCQU0T', 'W9VCQU0U', 'W9VCQU0V', 'W9VCQU0W', 'W9VCQU0X', 'W9VCQU0Y', 'W9VCQU0Z', 'W9VCQUAA', 'W9VCQUAB', 'W9VCQUAC', 'W9VCQUAD', 'W9VCQUAE', 'W9VCQUAF', 'W9VCQUAG')
  sub_vars_sub <- sub_vars[1:32]
  
  found_idx <- NA
  for (i in 1:32) {
    val <- row[[sub_vars_sub[i]]]
    if (!is.na(val) && val == 1) {
      found_idx <- i
      break
    }
  }
  
  if (!is.na(found_idx)) return(found_idx)
  
  all_no <- TRUE
  for (i in 1:32) {
    val <- row[[sub_vars_sub[i]]]
    if (is.na(val) || val != 2) { all_no <- FALSE; break }
  }
  if (all_no) return(33)
  
  if (row[['W9VCQUAH']] %in% 1) return(-8)
  if (row[['W9VCQUAI']] %in% 1) return(-9)
  
  if (any(row[sub_vars_sub] == -1, na.rm = TRUE)) return(-1)
  
  return(-3)
}

full_df$educvdtl32_raw <- apply(full_df, 1, calc_vdtl32)

# Labels
acad_labels <- c('Doctorate or equivalent', 'Masters or equivalent', 'Undergraduate or equivalent', 'Post-graduate Diplomas and Certificates', 'Diplomas in higher education and other higher education qualifications', 'Teaching qualifications for schools or further education (below degree level)', 'A/AS Levels or equivalent', 'Grade A-C, Level 4-9', 'Grade D-G, Level 1-3', 'SCE Higher', 'Scottish Certificate Sixth Year Studies', 'SCE Standard', 'National 4 and 5', 'National 2 and 3', 'Leaving Certificate', 'Junior Certificate grade A-C', 'Junior Certificate grade D and below', 'Other academic qualifications (including overseas)', 'None of these qualifications')

voc_labels <- c('Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor', 'Nursing or other medical qualifications (below degree level)', 'Level 4 or 5', 'Level 3', 'Level 2', 'Level 1', 'GNVQ Advanced', 'GNVQ Intermediate', 'Level 3', 'Level 2', 'Level Foundation', 'Advanced Craft, Part III', 'Craft, Part II', 'Craft, Part I', 'Level 3', 'Level 2', 'Level 1', 'Advanced Diploma', 'Higher Diploma', 'RSA Diploma', 'RSA Stage I, II,III', 'Higher Level BTEC', 'BTEC National', 'BTEC First', 'SCOTVEC National Certificate', 'SCOTVEC first or general diploma', 'SCOTVEC general diploma', 'SCOTVEC modules', 'HND or HNC', 'OND or ONCM', 'Junior certificate', 'Other vocational qualifications (including some overseas)', 'None of these qualifications')

missing_labels <- c('-1' = 'Not applicable', '-2' = 'Schedule not applicable / script error / information lost', '-3' = 'Not asked at the fieldwork stage / not interviewed', '-7' = 'Prefer not to say', '-8' = "Don't know / insufficient information", '-9' = 'Refusal')

full_df <- full_df %>% 
  mutate(
    educ25 = factor(educ25, levels = c(0, 1, 2, 3, 4, -1, -2, -3, -7, -8, -9), labels = c('NVQ 4–5 equivalent', 'NVQ 1–3 equivalent', 'Entry level or no qualifications', 'Other qualifications not mappable to the NVQ framework', 'None of these qualifications', missing_labels['-1'], missing_labels['-2'], missing_labels['-3'], missing_labels['-7'], missing_labels['-8'], missing_labels['-9'])),
    educ32 = factor(educ32, levels = c(0, 1, 2, 3, 4, -1, -2, -3, -7, -8, -9), labels = c('NVQ 4–5 equivalent', 'NVQ 1–3 equivalent', 'Entry level or no qualifications', 'Other qualifications not mappable to the NVQ framework', 'None of these qualifications', missing_labels['-1'], missing_labels['-2'], missing_labels['-3'], missing_labels['-7'], missing_labels['-8'], missing_labels['-9'])),
    educadtl32 = factor(educadtl32_raw, levels = c(1:19, -1, -2, -3, -7, -8, -9), labels = c(acad_labels, missing_labels['-1'], missing_labels['-2'], missing_labels['-3'], missing_labels['-7'], missing_labels['-8'], missing_labels['-9'])),
    educvdtl32 = factor(educvdtl32_raw, levels = c(1:33, -1, -2, -3, -7, -8, -9), labels = c(voc_labels, missing_labels['-1'], missing_labels['-2'], missing_labels['-3'], missing_labels['-7'], missing_labels['-8'], missing_labels['-9']))
  )

final_output <- full_df %>% select(NSID, educ25, educ32, educadtl32, educvdtl32)
write_csv(final_output, 'data/output/cleaned_data.csv')

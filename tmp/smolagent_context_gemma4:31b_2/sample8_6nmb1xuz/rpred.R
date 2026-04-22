library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c('ns8_2015_main_interview.tab', 'ns8_2015_derived.tab', 'ns9_2022_main_interview.tab', 'ns9_2022_derived_variables.tab')
data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t', show_col_types = FALSE))
names(data_list) <- files

merged_df <- data_list[[1]] %>%
  full_join(data_list[[2]], by = 'NSID') %>%
  full_join(data_list[[3]], by = 'NSID') %>%
  full_join(data_list[[4]], by = 'NSID')

# Helper for NVQ collapsing
collapse_nvq <- function(val) {
  if (is.na(val)) return(-3)
  if (val == 4 || val == 5) return(0)
  if (val == 1 || val == 2 || val == 3) return(1)
  if (val == 0 || val == 96) return(2)
  if (val == 95) return(3)
  if (val %in% c(-9, -8, -3, -2, -1)) return(val)
  return(4) # None of these
}

# Mapping W8 vocational tick-boxes to raw NVQ levels
# Updated to handle NA values explicitly to avoid 'missing value where TRUE/FALSE needed'
map_w8_voc_to_raw <- function(row) {
  v_j <- row[['W8VCQU0J']]; v_k <- row[['W8VCQU0K']]; v_l <- row[['W8VCQU0L']]
  if (!is.na(v_j) && v_j == 1 || !is.na(v_k) && v_k == 1 || !is.na(v_l) && v_l == 1) return(4)
  
  v_i <- row[['W8VCQU0I']]; v_e <- row[['W8VCQU0E']]; v_h <- row[['W8VCQU0H']]
  v_a <- row[['W8VCQU0A']]; v_b <- row[['W8VCQU0B']]; v_c <- row[['W8VCQU0C']]
  v_d <- row[['W8VCQU0D']]; v_f <- row[['W8VCQU0F']]; v_g <- row[['W8VCQU0G']]
  v_m <- row[['W8VCQU0M']]; v_n <- row[['W8VCQU0N']]
  
  if (!is.na(v_i) && v_i == 1 || !is.na(v_e) && v_e == 1 || !is.na(v_h) && v_h == 1 || 
      !is.na(v_a) && v_a == 1 || !is.na(v_b) && v_b == 1 || !is.na(v_c) && v_c == 1 || 
      !is.na(v_d) && v_d == 1 || !is.na(v_f) && v_f == 1 || !is.na(v_g) && v_g == 1 || 
      !is.na(v_m) && v_m == 1 || !is.na(v_n) && v_n == 1) return(2)
  
  v_p <- row[['W8VCQU0P']]
  if (!is.na(v_p) && v_p == 1) return(96)
  
  v_o <- row[['W8VCQU0O']]
  if (!is.na(v_o) && v_o == 1) return(95)
  
  return(NA)
}

# Process educ25
merged_df <- merged_df %>%
  rowwise() %>%
  mutate(
    voc_raw25 = map_w8_voc_to_raw(cur_data()),
    acad_raw25 = W8DHANVQH,
    highest_raw25 = {
      vals <- c(voc_raw25, acad_raw25)
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) {
        if (!is.na(W8DHANVQH) && W8DHANVQH %in% c(-9, -8, -1)) W8DHANVQH else -3
      } else {
        if (any(vals >= 4 & vals <= 5)) 4
        else if (any(vals >= 1 & vals <= 3)) 2
        else if (any(vals == 0)) 0
        else if (any(vals == 95)) 95
        else if (any(vals == 96)) 96
        else -3
      }
    },
    educ25_num = collapse_nvq(highest_raw25)
  ) %>%
  ungroup()

# Process educ32
merged_df <- merged_df %>%
  rowwise() %>%
  mutate(
    highest_raw32 = {
      vals <- c(W9DANVQH, W9DVNVQH)
      vals <- vals[!is.na(vals) & !vals %in% c(-9, -8, -1)]
      if (length(vals) == 0) {
        if (!is.na(W9DANVQH) && W9DANVQH %in% c(-9, -8, -1)) W9DANVQH
        else if (!is.na(W9DVNVQH) && W9DVNVQH %in% c(-9, -8, -1)) W9DVNVQH
        else -3
      } else {
        if (any(vals >= 4 & vals <= 5)) 4
        else if (any(vals >= 1 & vals <= 3)) 2
        else if (any(vals == 0)) 0
        else if (any(vals == 95)) 95
        else if (any(vals == 96)) 96
        else -3
      }
    },
    educ32_num = collapse_nvq(highest_raw32)
  ) %>%
  ungroup()

# Metadata lookups
meta_acad_labels <- list(
  'W9ACQU0A' = 'Doctorate or equivalent', 'W9ACQU0B' = 'Masters or equivalent', 'W9ACQU0C' = 'Undergraduate or equivalent',
  'W9ACQU0D' = 'Post-graduate Diplomas and Certificates', 'W9ACQU0E' = 'Diplomas in higher education and other higher education qualifications',
  'W9ACQU0F' = 'Teaching qualifications for schools or further education (below degree level)', 'W9ACQU0G' = 'A/AS Levels or equivalent',
  'W9ACQU0H' = 'Grade A-C, Level 4-9', 'W9ACQU0I' = 'Grade D-G, Level 1-3', 'W9ACQU0J' = 'SCE Higher',
  'W9ACQU0K' = 'Scottish Certificate Sixth Year Studies', 'W9ACQU0L' = 'SCE Standard', 'W9ACQU0M' = 'National 4 and 5',
  'W9ACQU0N' = 'National 2 and 3', 'W9ACQU0O' = 'Leaving Certificate', 'W9ACQU0P' = 'Junior Certificate grade A-C',
  'W9ACQU0Q' = 'Junior Certificate grade D and below', 'W9ACQU0R' = 'Other academic qualifications (including overseas)'
)

meta_voc_labels <- list(
  'W9VCQU0A' = 'Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor',
  'W9VCQU0B' = 'Nursing or other medical qualifications (below degree level)', 'W9VCQU0C' = 'Level 4 or 5', 'W9VCQU0D' = 'Level 3',
  'W9VCQU0E' = 'Level 2', 'W9VCQU0F' = 'Level 1', 'W9VCQU0G' = 'GNVQ Advanced', 'W9VCQU0H' = 'GNVQ Intermediate',
  'W9VCQU0I' = 'Level 3', 'W9VCQU0J' = 'Level 2', 'W9VCQU0K' = 'Level Foundation', 'W9VCQU0L' = 'Advanced Craft, Part III',
  'W9VCQU0M' = 'Craft, Part II', 'W9VCQU0N' = 'Craft, Part I', 'W9VCQU0O' = 'Level 3', 'W9VCQU0P' = 'Level 2',
  'W9VCQU0Q' = 'Level 1', 'W9VCQU0R' = 'Advanced Diploma', 'W9VCQU0S' = 'Higher Diploma', 'W9VCQU0T' = 'RSA Diploma',
  'W9VCQU0U' = 'RSA Stage I, II,III', 'W9VCQU0V' = 'Higher Level BTEC', 'W9VCQU0W' = 'BTEC National', 'W9VCQU0X' = 'BTEC First',
  'W9VCQU0Y' = 'SCOTVEC National Certificate', 'W9VCQU0Z' = 'SCOTVEC first or general diploma', 'W9VCQUAA' = 'SCOTVEC general diploma',
  'W9VCQUAB' = 'SCOTVEC modules', 'W9VCQUAC' = 'HND or HNC', 'W9VCQUAD' = 'OND or ONCM', 'W9VCQUAE' = 'Junior certificate',
  'W9VCQUAF' = 'Other vocational qualifications (including some overseas)'
)

merged_df <- merged_df %>%
  rowwise() %>%
  mutate(
    educadtl32 = {
      val <- NA
      for (v in names(meta_acad_labels)) {
        if (!is.na(get(v)) && get(v) == 1) { val <- meta_acad_labels[[v]]; break }
      }
      if (is.na(val)) {
        all_no <- TRUE
        for (v in names(meta_acad_labels)) {
          if (!is.na(get(v)) && get(v) != 2) all_no <- FALSE
        }
        if (all_no) val <- "None of these qualifications"
      }
      val
    },
    educvdtl32 = {
      val <- NA
      for (v in names(meta_voc_labels)) {
        if (!is.na(get(v)) && get(v) == 1) { val <- meta_voc_labels[[v]]; break }
      }
      if (is.na(val)) {
        all_no <- TRUE
        for (v in names(meta_voc_labels)) {
          if (!is.na(get(v)) && get(v) != 2) all_no <- FALSE
        }
        if (all_no) val <- "None of these qualifications"
      }
      val
    }
  ) %>%
  ungroup()

# Factor levels
nvq_levels <- c("0", "1", "2", "3", "4", "-1", "-2", "-3", "-8", "-9")
nvq_labels <- c("NVQ 4–5 equivalent qualifications", "NVQ 1–3 equivalent qualifications", "Entry level or no qualifications", "Other qualifications not mappable to NVQ framework", "None of these qualifications", "Not applicable", "Not applicable", "Null", "Don't know", "Refused")

merged_df <- merged_df %>%
  mutate(
    educ25 = factor(educ25_num, levels = nvq_levels, labels = nvq_labels),
    educ32 = factor(educ32_num, levels = nvq_levels, labels = nvq_labels)
  )

final_df <- merged_df %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

write_csv(final_df, 'data/output/cleaned_data.csv')
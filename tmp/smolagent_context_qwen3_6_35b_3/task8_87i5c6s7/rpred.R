library(dplyr)
library(readr)
library(haven)
library(labelled)
library(tidyr)
library(purrr)

# Read all data files
wf1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim='\t', show_col_types=FALSE)
wf4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim='\t', show_col_types=FALSE)
ns8_main <- read_delim('data/input/ns8_2015_main_interview.tab', delim='\t', show_col_types=FALSE)
ns8_derived <- read_delim('data/input/ns8_2015_derived.tab', delim='\t', show_col_types=FALSE)
ns9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim='\t', show_col_types=FALSE)
ns9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim='\t', show_col_types=FALSE)

# Merge all datasets
merged <- wf1 %>%
  full_join(wf4, by='NSID') %>%
  full_join(ns8_main, by='NSID') %>%
  full_join(ns8_derived, by='NSID') %>%
  full_join(ns9_main, by='NSID') %>%
  full_join(ns9_derived, by='NSID')

print(paste('Total rows:', nrow(merged)))

# Check actual column names for W9VCQU variables
vcq_cols <- grep('^W9VCQU', colnames(merged), value=TRUE)
print(vcq_cols)
print(paste('Number of VCQ columns:', length(vcq_cols)))

# === educ25 derivation ===

# Recode W8DHANVQH to NVQ scheme
merged <- merged %>%
  mutate(W8DHANVQH_n = case_when(
    W8DHANVQH %in% c(1,2,3) ~ 1,
    W8DHANVQH %in% c(4,5) ~ 0,
    W8DHANVQH == 95 ~ 3,
    W8DHANVQH == 96 ~ 4,
    TRUE ~ W8DHANVQH
  ))

# Tier mapping for W8VCQU* variables
tier_map <- list(
  'W8VCQU0A' = 2, 'W8VCQU0B' = 2, 'W8VCQU0C' = 2, 'W8VCQU0D' = 2,
  'W8VCQU0E' = 1, 'W8VCQU0F' = 1, 'W8VCQU0G' = 1, 'W8VCQU0H' = 1,
  'W8VCQU0I' = 1, 'W8VCQU0J' = 0, 'W8VCQU0K' = 0, 'W8VCQU0L' = 0,
  'W8VCQU0M' = 1, 'W8VCQU0N' = 0, 'W8VCQU0O' = 3, 'W8VCQU0P' = 2,
  'W8VCQU0Q' = -8, 'W8VCQU0R' = -9
)

# Recode each W8VCQU* variable to NVQ tier
for (vname in names(tier_map)) {
  tier <- tier_map[[vname]]
  merged <- merged %>%
    mutate(!!paste0(vname, '_nvq') := case_when(
      .data[[vname]] == 1 ~ tier,
      .data[[vname]] == 0 ~ 2,
      .data[[vname]] %in% c(-9, -8, -1, -3) ~ .data[[vname]],
      TRUE ~ NA_real_
    ))
}

# Combine: take highest valid qualification (lowest numeric code)
vcq_names <- names(tier_map)
nvq_cols <- paste0(vcq_names, '_nvq')

merged <- merged %>%
  rowwise() %>%
  mutate(educ25 = {
    vals <- c(W8DHANVQH_n, !!!syms(nvq_cols))
    valid <- vals[!is.na(vals)]
    if (length(valid) == 0) -3
    else min(valid)
  }) %>%
  ungroup()

# Make educ25 a labelled factor
educ25_levels <- c(0, 1, 2, 3, 4, -9, -8, -7, -3, -2, -1)
educ25_labels <- c('NVQ 4-5 equivalent', 'NVQ 1-3 equivalent',
                   'Entry level or no qualifications',
                   'Other qualifications not mappable to the NVQ framework',
                   'None of these qualifications',
                   'Refusal', "Don't know", 'Prefer not to say',
                   'Not asked at fieldwork stage',
                   'Schedule not applicable / script error / information lost',
                   'Item not applicable')
merged$educ25 <- factor(merged$educ25, levels=educ25_levels, labels=educ25_labels)

# === educ32 derivation ===

# Helper function for NVQ mapping
map_to_nqv <- function(x) {
  result <- ifelse(is.na(x), NA_real_, x)
  result <- ifelse(x %in% c(1, 2, 3), 1, result)
  result <- ifelse(x %in% c(4, 5), 0, result)
  result <- ifelse(x == 95, 3, result)
  result <- ifelse(x == 96, 4, result)
  return(result)
}

# Create a helper data frame with both NVQ codes
merged <- merged %>%
  mutate(a_n = map_to_nqv(W9DANVQH),
         v_n = map_to_nqv(W9DVNVQH))

# For each row, find the minimum valid NVQ code, or return the original source code if both are NA
merged$educ32 <- mapply(function(a, v, a_n, v_n) {
  valid <- c(a_n, v_n)
  valid <- valid[!is.na(valid)]
  
  if (length(valid) == 0) {
    # Return the original code from whichever source is available
    if (!is.na(a)) return(a)
    else if (!is.na(v)) return(v)
    else return(-3)
  } else {
    return(min(valid))
  }
}, merged$W9DANVQH, merged$W9DVNVQH, merged$a_n, merged$v_n)

# Remove intermediate columns
merged <- merged %>% select(-a_n, -v_n)

# Make educ32 a labelled factor
merged$educ32 <- factor(merged$educ32, levels=educ25_levels, labels=educ25_labels)

# === educadtl32 derivation ===

# Academic tick-box variables in metadata order
acq_vars <- c('W9ACQU0A', 'W9ACQU0B', 'W9ACQU0C', 'W9ACQU0D', 'W9ACQU0E',
              'W9ACQU0F', 'W9ACQU0G', 'W9ACQU0H', 'W9ACQU0I', 'W9ACQU0J',
              'W9ACQU0K', 'W9ACQU0L', 'W9ACQU0M', 'W9ACQU0N', 'W9ACQU0O',
              'W9ACQU0P', 'W9ACQU0Q', 'W9ACQU0R', 'W9ACQU0S', 'W9ACQU0T',
              'W9ACQU0U', 'W9ACQU0V')

# Substantive indicators: A-R (indices 1-18)
# Code 19: None of these qualifications
acq_labels <- c(
  'Doctorate or equivalent',
  'Masters or equivalent',
  'Undergraduate or equivalent',
  'Post-graduate Diplomas and Certificates',
  'Diplomas in higher education and other higher education qualifications',
  'Teaching qualifications for schools or further education (below degree level)',
  'A/AS Levels or equivalent',
  'Grade A-C, Level 4-9',
  'Grade D-G, Level 1-3',
  'SCE Higher',
  'Scottish Certificate Sixth Year Studies',
  'SCE Standard',
  'National 4 and 5',
  'National 2 and 3',
  'Leaving Certificate',
  'Junior Certificate grade A-C',
  'Junior Certificate grade D and below',
  'Other academic qualifications (including overseas)',
  'None of these qualifications'
)

# For each row, compute the code
merged$educadtl32 <- sapply(seq_len(nrow(merged)), function(r) {
  vals <- as.numeric(unlist(merged[r, acq_vars]))
  
  # 1. Check for non-substantive indicators (T, U, V = indices 20, 21, 22)
  if (!is.na(vals[20]) && vals[20] == 1) return(-8)  # Don't know
  if (!is.na(vals[21]) && vals[21] == 1) return(-9)  # Refused
  if (!is.na(vals[22]) && vals[22] == 1) return(-2)  # No answer
  
  # 2. Check for -1 (Not applicable)
  if (any(vals == -1, na.rm=TRUE)) return(-1)
  
  # 3. Check for -3 or missing
  if (any(is.na(vals) | vals == -3)) return(-3)
  
  # 4. Scan substantive indicators (A-R = indices 1-18) in order
  for (i in 1:18) {
    if (!is.na(vals[i]) && vals[i] == 1) return(i)
  }
  
  # 5. If all A-R are No (2), assign code 19 (None of these qualifications)
  return(19)
})

# Make educadtl32 a labelled factor
# Levels: 1-19, -1, -2, -3, -8, -9 (24 levels)
# Labels: 19 substantive labels + 5 missing labels (24 labels)
acq_levels <- c(1:19, -1, -2, -3, -8, -9)
final_acq_labels <- c(acq_labels, 'Item not applicable',
                       'Schedule not applicable / script error / information lost',
                       'Not asked at fieldwork stage',
                       "Don't know", 'Refused')

merged$educadtl32 <- factor(as.character(merged$educadtl32),
                             levels=as.character(acq_levels),
                             labels=final_acq_labels)

# === educvdtl32 derivation ===

# Use the actual column names from the merged data
vcq_vars <- vcq_cols  # Already extracted above

# Substantive indicators: all except the last 3 (AG, AH, AI)
# Code 33: None of these qualifications
vcq_labels <- c(
  'Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor',
  'Nursing or other medical qualifications (below degree level)',
  'Level 4 or 5',
  'Level 3',
  'Level 2',
  'Level 1',
  'GNVQ Advanced',
  'GNVQ Intermediate',
  'Level 3',
  'Level 2',
  'Level Foundation',
  'Advanced Craft, Part III',
  'Craft, Part II',
  'Craft, Part I',
  'Level 3',
  'Level 2',
  'Level 1',
  'Advanced Diploma',
  'Higher Diploma',
  'RSA Diploma',
  'RSA Stage I, II,III',
  'Higher Level BTEC',
  'BTEC National',
  'BTEC First',
  'SCOTVEC National Certificate',
  'SCOTVEC first or general diploma',
  'SCOTVEC general diploma',
  'SCOTVEC modules',
  'HND or HNC',
  'OND or ONCM',
  'Junior certificate',
  'Other vocational qualifications (including some overseas)',
  'None of these qualifications'
)

# For each row, compute the code
merged$educvdtl32 <- sapply(seq_len(nrow(merged)), function(r) {
  vals <- as.numeric(unlist(merged[r, vcq_vars]))
  n_sub <- 32  # Number of substantive indicators (35 total - 3 non-substantive)
  
  # 1. Check for non-substantive indicators (AH, AI = indices 34, 35)
  if (!is.na(vals[34]) && vals[34] == 1) return(-8)  # Don't know
  if (!is.na(vals[35]) && vals[35] == 1) return(-9)  # Refused
  
  # 2. Check for -1 (Not applicable)
  if (any(vals == -1, na.rm=TRUE)) return(-1)
  
  # 3. Check for -3 or missing
  if (any(is.na(vals) | vals == -3)) return(-3)
  
  # 4. Scan substantive indicators (A-AF = indices 1-32) in order
  for (i in 1:n_sub) {
    if (!is.na(vals[i]) && vals[i] == 1) return(i)
  }
  
  # 5. If all A-AF are No (2), assign code 33 (None of these qualifications)
  return(n_sub + 1)
})

# Make educvdtl32 a labelled factor
# Levels: 1-33, -1, -3, -8, -9 (37 levels)
# Labels: 33 substantive labels + 4 missing labels (37 labels)
vcq_levels <- c(1:33, -1, -3, -8, -9)
final_vcq_labels <- c(vcq_labels, 'Item not applicable',
                       'Not asked at fieldwork stage',
                       "Don't know", 'Refused')

merged$educvdtl32 <- factor(as.character(merged$educvdtl32),
                             levels=as.character(vcq_levels),
                             labels=final_vcq_labels)

# === Output ===

out <- merged %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

dir.create('data/output', showWarnings=FALSE, recursive=TRUE)
write_csv(out, 'data/output/cleaned_data.csv')

cat('Done. Output shape:', nrow(out), 'x', ncol(out), '\n')
cat('educ25 levels:', paste(levels(merged$educ25), collapse=', '), '\n')
cat('educadtl32 levels:', paste(levels(merged$educadtl32), collapse=', '), '\n')
cat('educvdtl32 levels:', paste(levels(merged$educvdtl32), collapse=', '), '\n')
print(table(merged$educ25, useNA='ifany'))
print(table(merged$educ32, useNA='ifany'))
print(table(merged$educadtl32, useNA='ifany'))
print(table(merged$educvdtl32, useNA='ifany'))

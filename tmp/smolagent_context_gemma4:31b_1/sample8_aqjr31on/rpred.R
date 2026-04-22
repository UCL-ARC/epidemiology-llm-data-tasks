library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# The previous error showed 0 rows being read and then a massive join failure.
# This usually happens when read_delim() doesn't find data or thinks columns are all characters
# and the NSID column ends up as NA or empty, causing a Cartesian product.
# Let's ensure we read NSID as character to avoid precision issues and ensure data is actually read.

read_subset <- function(file, vars) {
  path <- paste0('data/input/', file)
  # Read only the header to find columns
  header <- read_delim(path, delim = '\t', n_max = 0)
  all_cols <- names(header)
  target_cols <- intersect(vars, all_cols)
  
  # Read data specifying NSID as character and others as numeric
  read_delim(path, delim = '\t', col_types = cols(NSID = col_character(), .default = col_double()), col_select = all_of(target_cols))
}

vars_w8_main <- c('NSID', 'W8VCQU0A', 'W8VCQU0B', 'W8VCQU0C', 'W8VCQU0D', 'W8VCQU0E', 'W8VCQU0F', 'W8VCQU0G', 'W8VCQU0H', 'W8VCQU0I', 'W8VCQU0J', 'W8VCQU0K', 'W8VCQU0L', 'W8VCQU0M', 'W8VCQU0N', 'W8VCQU0O', 'W8VCQU0P', 'W8VCQU0Q', 'W8VCQU0R')
vars_w8_der <- c('NSID', 'W8DHANVQH')
vars_w9_main_acad <- paste0('W9ACQU', c('0A', '0B', '0C', '0D', '0E', '0F', '0G', '0H', '0I', '0J', '0K', '0L', '0M', '0N', '0O', '0P', '0Q', '0R', '0S', '0T', '0U', '0V'))
vars_w9_main_voc <- paste0('W9VCQU', c('0A', '0B', '0C', '0D', '0E', '0F', '0G', '0H', '0I', '0J', '0K', '0L', '0M', '0N', '0O', '0P', '0Q', '0R', '0S', '0T', '0U', '0V', '0W', '0X', '0Y', '0Z', 'AA', 'AB', 'AC', 'AD', 'AE', 'AF', 'AG', 'AH', 'AI'))
vars_w9_main <- c('NSID', vars_w9_main_acad, vars_w9_main_voc)
vars_w9_der <- c('NSID', 'W9DANVQH', 'W9DVNVQH')

df8_main <- read_subset('ns8_2015_main_interview.tab', vars_w8_main)
df8_der <- read_subset('ns8_2015_derived.tab', vars_w8_der)
df9_main <- read_subset('ns9_2022_main_interview.tab', vars_w9_main)
df9_der <- read_subset('ns9_2022_derived_variables.tab', vars_w9_der)

# Merge sequentially with NSID check
full_df <- df8_main %>%
  full_join(df8_der, by = 'NSID') %>%
  full_join(df9_main, by = 'NSID') %>%
  full_join(df9_der, by = 'NSID')

# NVQ mapping logic for educ25
full_df <- full_df %>% mutate(educ25_raw = pmap_dbl(list(W8DHANVQH, W8VCQU0J, W8VCQU0K, W8VCQU0L, W8VCQU0I, W8VCQU0H, W8VCQU0P), function(a, vJ, vK, vL, vI, vH, vP) {
  vals <- c()
  if(!is.na(a)) vals <- c(vals, a)
  if(!is.na(vJ) && vJ == 1) vals <- c(vals, 4)
  if(!is.na(vK) && vK == 1) vals <- c(vals, 4)
  if(!is.na(vL) && vL == 1) vals <- c(vals, 4)
  if(!is.na(vI) && vI == 1) vals <- c(vals, 2)
  if(!is.na(vH) && vH == 1) vals <- c(vals, 3)
  if(!is.na(vP) && vP == 1) vals <- c(vals, 96)
  
  if(length(vals) == 0) return(-3)
  if(any(vals == 4 | vals == 5)) return(0)
  if(any(vals == 1 | vals == 2 | vals == 3)) return(1)
  if(any(vals == 0 | vals == 96)) return(2)
  if(any(vals == 95)) return(3)
  
  m_codes <- vals[vals < 0]
  if(length(m_codes) > 0) return(m_codes[1])
  return(4)
}))

# NVQ mapping logic for educ32
full_df <- full_df %>% mutate(educ32_raw = pmap_dbl(list(W9DANVQH, W9DVNVQH), function(a, v) {
  vals <- c()
  if(!is.na(a)) vals <- c(vals, a)
  if(!is.na(v)) vals <- c(vals, v)
  
  if(length(vals) == 0) return(-3)
  if(any(vals == 4 | vals == 5)) return(0)
  if(any(vals == 1 | vals == 2 | vals == 3)) return(1)
  if(any(vals == 0 | vals == 96)) return(2)
  if(any(vals == 95)) return(3)
  
  m_codes <- vals[vals < 0]
  if(length(m_codes) > 0) return(m_codes[1])
  return(4)
}))

meta_acad <- c(
  W9ACQU0A = "Doctorate or equivalent", W9ACQU0B = "Masters or equivalent", W9ACQU0C = "Undergraduate or equivalent",
  W9ACQU0D = "Post-graduate Diplomas and Certificates", W9ACQU0E = "Diplomas in higher education and other higher education qualifications",
  W9ACQU0F = "Teaching qualifications for schools or further education (below degree level)", W9ACQU0G = "A/AS Levels or equivalent",
  W9ACQU0H = "Grade A-C, Level 4-9", W9ACQU0I = "Grade D-G, Level 1-3", W9ACQU0J = "SCE Higher",
  W9ACQU0K = "Scottish Certificate Sixth Year Studies", W9ACQU0L = "SCE Standard", W9ACQU0M = "National 4 and 5",
  W9ACQU0N = "National 2 and 3", W9ACQU0O = "Leaving Certificate", W9ACQU0P = "Junior Certificate grade A-C",
  W9ACQU0Q = "Junior Certificate grade D and below", W9ACQU0R = "Other academic qualifications (including overseas)",
  W9ACQU0S = "None of these qualifications", W9ACQU0T = "Don't know", W9ACQU0U = "Refused", W9ACQU0V = "No answer"
)

meta_voc <- c(
  W9VCQU0A = "Professional qualifications at degree level", W9VCQU0B = "Nursing or other medical qualifications",
  W9VCQU0C = "Level 4 or 5", W9VCQU0D = "Level 3", W9VCQU0E = "Level 2", W9VCQU0F = "Level 1",
  W9VCQU0G = "GNVQ Advanced", W9VCQU0H = "GNVQ Intermediate", W9VCQU0I = "Level 3",
  W9VCQU0J = "Level 2", W9VCQU0K = "Level Foundation", W9VCQU0L = "Advanced Craft, Part III",
  W9VCQU0M = "Craft, Part II", W9VCQU0N = "Craft, Part I", W9VCQU0O = "Level 3",
  W9VCQU0P = "Level 2", W9VCQU0Q = "Level 1", W9VCQU0R = "Advanced Diploma",
  W9VCQU0S = "Higher Diploma", W9VCQU0T = "RSA Diploma", W9VCQU0U = "RSA Stage I, II,III",
  W9VCQU0V = "Higher Level BTEC", W9VCQU0W = "BTEC National", W9VCQU0X = "BTEC First",
  W9VCQU0Y = "SCOTVEC National Certificate", W9VCQU0Z = "SCOTVEC first or general diploma",
  W9VCQUAA = "SCOTVEC general diploma", W9VCQUAB = "SCOTVEC modules", W9VCQUAC = "HND or HNC",
  W9VCQUAD = "OND or ONCM", W9VCQUAE = "Junior certificate", W9VCQUAF = "Other vocational qualifications",
  W9VCQUAG = "None of these qualifications", W9VCQUAH = "Don't know", W9VCQUAI = "Refused"
)

get_detail <- function(row, mapping) {
  vars <- names(mapping)
  for(v in vars) {
    val <- row[[v]]
    if(!is.na(val) && val == 1) return(mapping[[v]])
  }
  if(all(row[vars] == 2, na.rm = TRUE)) return("None of these qualifications")
  return(NA_character_)
}

full_df$educadtl32 <- apply(full_df, 1, function(r) get_detail(r, meta_acad))
full_df$educvdtl32 <- apply(full_df, 1, function(r) get_detail(r, meta_voc))

final_df <- full_df %>%
  mutate(
    educ25 = factor(educ25_raw, levels = c(0, 1, 2, 3, 4, -1, -2, -3, -8, -9)),
    educ32 = factor(educ32_raw, levels = c(0, 1, 2, 3, 4, -1, -2, -3, -8, -9))
  ) %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

write_csv(final_df, 'data/output/cleaned_data.csv')

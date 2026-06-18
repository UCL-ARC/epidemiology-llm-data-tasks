library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# Standard missing conversion
standardize_missing <- function(x){
  x[is.na(x)] <- -3
  return(x)
}

# Load files
wave1      <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
wave4      <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
ns8_main   <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t', col_types = cols(.default = 'c'))
ns8_derived<- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = cols(.default = 'c'))
ns9_main   <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', col_types = cols(.default = 'c'))
ns9_derived<- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = cols(.default = 'c'))

# Convert numeric columns where they exist
if('W8DHANVQH' %in% names(ns8_derived)){
  ns8_derived <- ns8_derived %>% mutate(W8DHANVQH = as.numeric(W8DHANVQH))
}
if('W9DANVQH' %in% names(ns9_derived)){
  ns9_derived <- ns9_derived %>% mutate(W9DANVQH = as.numeric(W9DANVQH))
}
if('W9DVNVQH' %in% names(ns9_derived)){
  ns9_derived <- ns9_derived %>% mutate(W9DVNVQH = as.numeric(W9DVNVQH))
}
# Convert all variables starting with W8VCQU in ns8_main
if(any(grepl('^W8VCQU', names(ns8_main)))){
  ns8_main <- ns8_main %>% mutate(across(starts_with('W8VCQU'), as.numeric))
}
# Convert variables starting with W9ACQU and W9VCQU in ns9_main
if(any(grepl('^W9ACQU', names(ns9_main)))){
  ns9_main <- ns9_main %>% mutate(across(starts_with('W9ACQU'), as.numeric))
}
if(any(grepl('^W9VCQU', names(ns9_main)))){
  ns9_main <- ns9_main %>% mutate(across(starts_with('W9VCQU'), as.numeric))
}

# Merge all datasets by NSID
full_df <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(ns8_main, by = 'NSID') %>%
  full_join(ns8_derived, by = 'NSID') %>%
  full_join(ns9_main, by = 'NSID') %>%
  full_join(ns9_derived, by = 'NSID')

# Education at age 25 (wave 8)
full_df <- full_df %>%
  mutate(educ25_raw = case_when(
    W8DHANVQH %in% c(1,2,3,4,5) ~ W8DHANVQH,
    W8DHANVQH %in% c(95,96) ~ 0,
    W8DHANVQH %in% c(-9,-8,-1) ~ W8DHANVQH,
    TRUE ~ NA_real_
  ))
full_df$educ25_raw <- standardize_missing(full_df$educ25_raw)
educ25_labels <- c('No NVQ'=0,'NVQ Level 1'=1,'NVQ Level 2'=2,'NVQ Level 3'=3,'NVQ Level 4'=4,'NVQ Level 5'=5)
full_df$educ25 <- labelled(full_df$educ25_raw, labels = educ25_labels)

# Education at age 32 (wave 9)
full_df <- full_df %>%
  mutate(
    nvq_ac = case_when(
      W9DANVQH %in% c(0,1,2,3,4,5) ~ W9DANVQH,
      W9DANVQH %in% c(95,96) ~ 0,
      W9DANVQH %in% c(-9,-8,-1) ~ W9DANVQH,
      TRUE ~ NA_real_
    ),
    nvq_voc = case_when(
      W9DVNVQH %in% c(0,1,2,3,4,5) ~ W9DVNVQH,
      W9DVNVQH %in% c(95,96) ~ 0,
      W9DVNVQH %in% c(-9,-8,-1) ~ W9DVNVQH,
      TRUE ~ NA_real_
    ),
    educ32_raw = case_when(
      !is.na(nvq_ac) ~ nvq_ac,
      is.na(nvq_ac) & !is.na(nvq_voc) ~ nvq_voc,
      TRUE ~ NA_real_
    )
  )
full_df$educ32_raw <- standardize_missing(full_df$educ32_raw)
full_df$educ32 <- labelled(full_df$educ32_raw, labels = educ25_labels)

# Helper to create detailed qualification variable
create_detail_var <- function(df, vars, labels, yes_code=1, no_code=0){
  df <- df %>% mutate(across(all_of(vars), as.numeric))
  df <- df %>% mutate(across(all_of(vars), ~ ifelse(. %in% c(-9,-8,-1), NA_real_, .)))
  raw <- apply(df[vars], 1, function(row){
    idx_yes <- which(row == yes_code)
    if(length(idx_yes) > 0){
      return(idx_yes[1])
    }
    if(all(row == no_code, na.rm = TRUE)){
      return(length(labels))
    }
    return(NA_integer_)
  })
  labelled(raw, labels = setNames(1:length(labels), labels))
}

# Academic detailed qualifications at age 32
academic_vars <- c('W9ACQU0A','W9ACQU0B','W9ACQU0C','W9ACQU0D','W9ACQU0E','W9ACQU0F','W9ACQU0G','W9ACQU0H','W9ACQU0I','W9ACQU0J','W9ACQU0K','W9ACQU0L','W9ACQU0M','W9ACQU0N','W9ACQU0O','W9ACQU0P','W9ACQU0Q','W9ACQU0R','W9ACQU0S','W9ACQU0T','W9ACQU0U','W9ACQU0V')
academic_labels <- c('Doctorate','Masters','Undergraduate','PostGradDiplomas','Diplomas','Teaching','ASLevels','Grade A-C Level 4-9','Grade D-G Level 1-3','SCE Higher','Scottish Certificate Sixth Year','SCE Standard','National 4 and 5','National 2 and 3','Leaving Certificate','Junior Certificate A-C','Junior Certificate D and below','Other academic qualifications','None of these qualifications')
full_df$educadtl32 <- create_detail_var(full_df, academic_vars, academic_labels)

# Vocational detailed qualifications at age 32
voc_vars <- c('W9VCQU0A','W9VCQU0B','W9VCQU0C','W9VCQU0D','W9VCQU0E','W9VCQU0F','W9VCQU0G','W9VCQU0H','W9VCQU0I','W9VCQU0J','W9VCQU0K','W9VCQU0L','W9VCQU0M','W9VCQU0N','W9VCQU0O','W9VCQU0P','W9VCQU0Q','W9VCQU0R','W9VCQU0S','W9VCQU0T','W9VCQU0U','W9VCQU0V','W9VCQU0W','W9VCQU0X','W9VCQU0Y','W9VCQU0Z','W9VCQUAA','W9VCQUAB','W9VCQUAC','W9VCQUAD','W9VCQUAE','W9VCQUAF','W9VCQUAG','W9VCQUAH','W9VCQUAI')
voc_labels <- c('Professional qualifications at degree level','Nursing or other medical qualifications (below degree level)','Level 4 or 5','Level 3','Level 2','Level 1','GNVQ Advanced','GNVQ Intermediate','Level Foundation','Advanced Craft','Craft Part II','Craft Part I','Advanced Diploma','Higher Diploma','RSA Diploma','RSA Stage I, II, III','Higher Level BTEC','BTEC National','BTEC First','SCOTVEC National Certificate','SCOTVEC First or general diploma','SCOTVEC General Diploma','SCOTVEC Modules','HND or HNC','OND or ONCM','Junior certificate','Other vocational qualifications (including some overseas)','None of these qualifications')
full_df$educvdtl32 <- create_detail_var(full_df, voc_vars, voc_labels)

# Final dataset
final_df <- full_df %>% select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write CSV
write_csv(final_df, 'data/output/cleaned_data.csv')

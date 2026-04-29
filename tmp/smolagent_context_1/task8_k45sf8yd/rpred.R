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

full_df <- reduce(data_list, full_join, by = 'NSID')

# 2. NVQ Mapping Helper
map_nvq <- function(val) {
  case_when(
    val >= 4 & val <= 5 ~ 0,
    val >= 1 & val <= 3 ~ 1,
    val == 0 ~ 2,
    val == 96 ~ 4,
    val == 95 ~ 3,
    TRUE ~ NA_real_
  )
}

# --- educ25 ---
acad_nvq_25 <- map_nvq(full_df$W8DHANVQH)

# Use a more robust row-wise logic for vocational 25
# Variables: W8VCQU0I(9), 0J(10), 0K(11), 0L(12), 0P(16)
voc_nvq_25 <- apply(full_df %>% select(W8VCQU0A:W8VCQU0P), 1, function(x) {
  # Ensure we handle NAs in the comparison
  t0_val <- any(x[10] == 1 | x[11] == 1 | x[12] == 1, na.rm = TRUE)
  t1_val <- any(x[9] == 1, na.rm = TRUE)
  t4_val <- any(x[16] == 1, na.rm = TRUE)
  
  if(!is.na(t0_val) && t0_val) return(0)
  if(!is.na(t1_val) && t1_val) return(1)
  if(!is.na(t4_val) && t4_val) return(4)
  return(2)
})

combine_highest <- function(a, v) {
  res <- rep(NA, length(a))
  for(i in 1:length(a)) {
    vals <- c(a[i], v[i])
    vals <- vals[!is.na(vals)]
    if(length(vals) > 0) res[i] <- min(vals)
  }
  return(res)
}

full_df$educ25_val <- combine_highest(acad_nvq_25, voc_nvq_25)

full_df$educ25 <- case_when(
  full_df$W8VCQU0R == 1 ~ -9,
  full_df$W8VCQU0Q == 1 ~ -8,
  is.na(full_df$educ25_val) ~ -3,
  TRUE ~ full_df$educ25_val
)

# --- educ32 ---
full_df$educ32_val <- apply(full_df %>% select(W9DANVQH, W9DVNVQH), 1, function(x) {
  mapped <- map_nvq(x)
  mapped <- mapped[!is.na(mapped)]
  if(length(mapped) == 0) return(NA)
  return(min(mapped))
})

full_df$educ32 <- case_when(
  !is.na(full_df$educ32_val) ~ full_df$educ32_val,
  full_df$W9DANVQH == -9 | full_df$W9DVNVQH == -9 ~ -9,
  full_df$W9DANVQH == -8 | full_df$W9DVNVQH == -8 ~ -8,
  full_df$W9DANVQH == -1 | full_df$W9DVNVQH == -1 ~ -1,
  TRUE ~ -3
)

# --- educadtl32 ---
acad_vars <- c("W9ACQU0A", "W9ACQU0B", "W9ACQU0C", "W9ACQU0D", "W9ACQU0E", "W9ACQU0F", "W9ACQU0G", "W9ACQU0H", "W9ACQU0I", "W9ACQU0J", "W9ACQU0K", "W9ACQU0L", "W9ACQU0M", "W9ACQU0N", "W9ACQU0O", "W9ACQU0P", "W9ACQU0Q", "W9ACQU0R")
full_df$educadtl32 <- apply(full_df, 1, function(r) {
  vals <- as.numeric(r[acad_vars])
  first_yes <- which(vals == 1)[1]
  if(!is.na(first_yes)) return(first_yes)
  if(!is.na(as.numeric(r["W9ACQU0T"])) && as.numeric(r["W9ACQU0T"]) == 1) return(-8)
  if(!is.na(as.numeric(r["W9ACQU0U"])) && as.numeric(r["W9ACQU0U"]) == 1) return(-9)
  if(!is.na(as.numeric(r["W9ACQU0V"])) && as.numeric(r["W9ACQU0V"]) == 1) return(-2)
  if(all(vals == 2, na.rm=TRUE)) return(19)
  if(all(vals == -1, na.rm=TRUE)) return(-1)
  return(-3)
})

# --- educvdtl32 ---
voc_vars <- c("W9VCQU0A", "W9VCQU0B", "W9VCQU0C", "W9VCQU0D", "W9VCQU0E", "W9VCQU0F", "W9VCQU0G", "W9VCQU0H", "W9VCQU0I", "W9VCQU0J", "W9VCQU0K", "W9VCQU0L", "W9VCQU0M", "W9VCQU0N", "W9VCQU0O", "W9VCQU0P", "W9VCQU0Q", "W9VCQU0R", "W9VCQU0S", "W9VCQU0T", "W9VCQU0U", "W9VCQU0V", "W9VCQU0W", "W9VCQU0X", "W9VCQU0Y", "W9VCQU0Z", "W9VCQUAA", "W9VCQUAB", "W9VCQUAC", "W9VCQUAD", "W9VCQUAE", "W9VCQUAF")
full_df$educvdtl32 <- apply(full_df, 1, function(r) {
  vals <- as.numeric(r[voc_vars])
  first_yes <- which(vals == 1)[1]
  if(!is.na(first_yes)) return(first_yes)
  if(!is.na(as.numeric(r["W9VCQUAH"])) && as.numeric(r["W9VCQUAH"]) == 1) return(-8)
  if(!is.na(as.numeric(r["W9VCQUAI"])) && as.numeric(r["W9VCQUAI"]) == 1) return(-9)
  if(all(vals == 2, na.rm=TRUE)) return(length(voc_vars) + 1)
  if(all(vals == -1, na.rm=TRUE)) return(-1)
  return(-3)
})

# --- Factor Labelling ---
miss_labels <- c("-9" = "Refusal", "-8" = "Don't know / insufficient information", "-7" = "Prefer not to say", "-3" = "Not asked at the fieldwork stage / not interviewed", "-2" = "Schedule not applicable / script error / information lost", "-1" = "Item not applicable")
nvq_labels <- c("0" = "NVQ 4–5 equivalent", "1" = "NVQ 1–3 equivalent", "2" = "Entry level or no qualifications", "3" = "Other qualifications not mappable to NVQ framework", "4" = "None of these qualifications")

full_df$educ25 <- factor(full_df$educ25, levels = c(0:4, -9, -8, -7, -3, -2, -1), labels = c(nvq_labels, miss_labels))
full_df$educ32 <- factor(full_df$educ32, levels = c(0:4, -9, -8, -7, -3, -2, -1), labels = c(nvq_labels, miss_labels))

acad_labels <- c("Doctorate or equivalent", "Masters or equivalent", "Undergraduate or equivalent", "Post-graduate Diplomas and Certificates", "Diplomas in higher education and other higher education qualifications", "Teaching qualifications for schools or further education (below degree level)", "A/AS Levels or equivalent", "Grade A-C, Level 4-9", "Grade D-G, Level 1-3", "SCE Higher", "Scottish Certificate Sixth Year Studies", "SCE Standard", "National 4 and 5", "National 2 and 3", "Leaving Certificate", "Junior Certificate grade A-C", "Junior Certificate grade D and below", "Other academic qualifications (including overseas)", "None of these qualifications")
full_df$educadtl32 <- factor(full_df$educadtl32, levels = c(1:19, -9, -8, -7, -3, -2, -1), labels = c(acad_labels, miss_labels))

voc_labels <- c("Professional qualifications at degree level", "Nursing or other medical qualifications (below degree level)", "Level 4 or 5", "Level 3", "Level 2", "Level 1", "GNVQ Advanced", "GNVQ Intermediate", "Level 3", "Level 2", "Level Foundation", "Advanced Craft, Part III", "Craft, Part II", "Craft, Part I", "Level 3", "Level 2", "Level 1", "Advanced Diploma", "Higher Diploma", "RSA Diploma", "RSA Stage I, II, III", "Higher Level BTEC", "BTEC National", "BTEC First", "SCOTVEC National Certificate", "SCOTVEC first or general diploma", "SCOTVEC general diploma", "SCOTVEC modules", "HND or HNC", "OND or ONCM", "Junior certificate", "Other vocational qualifications", "None of these qualifications")
full_df$educvdtl32 <- factor(full_df$educvdtl32, levels = c(1:33, -9, -8, -7, -3, -2, -1), labels = c(voc_labels, miss_labels))

final_output <- full_df %>% select(NSID, educ25, educ32, educadtl32, educvdtl32)
write_csv(final_output, "data/output/cleaned_data.csv")

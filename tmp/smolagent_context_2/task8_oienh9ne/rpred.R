library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files_to_load <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_main_interview.tab',
  'ns9_2022_derived_variables.tab'
)

data_list <- map(files_to_load, ~read_delim(paste0('data/input/', .x), delim = '\t'))

full_cohort <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_cohort <- full_join(full_cohort, data_list[[i]], by = 'NSID')
}

# Helper for NVQ mapping
map_nvq_level <- function(val) {
  if (is.na(val)) return(NA)
  if (val == 4 || val == 5) return(0)
  if (val == 1 || val == 2 || val == 3) return(1)
  if (val == 0 || val == 96) return(2)
  if (val == 95) return(3)
  return(NA)
}

# --- educ25 ---
tier0_vars <- c('W8VCQU0J', 'W8VCQU0K', 'W8VCQU0L', 'W8VCQU0M')
tier1_vars <- c('W8VCQU0I')
tier2_vars <- c('W8VCQU0A', 'W8VCQU0B', 'W8VCQU0C', 'W8VCQU0D', 'W8VCQU0E', 'W8VCQU0F', 'W8VCQU0G', 'W8VCQU0H')
tier4_vars <- c('W8VCQU0P')

full_cohort <- full_cohort %>%
  mutate(
    v_tier = case_when(
      if_any(all_of(tier0_vars), ~ . == 1) ~ 0,
      if_any(all_of(tier1_vars), ~ . == 1) ~ 1,
      if_any(all_of(tier2_vars), ~ . == 1) ~ 2,
      if_any(all_of(tier4_vars), ~ . == 1) ~ 4,
      TRUE ~ NA_real_
    ),
    a_tier = map_dbl(W8DHANVQH, map_nvq_level),
    educ25_val = pmin(a_tier, v_tier, na.rm = TRUE),
    educ25_val = ifelse(is.infinite(educ25_val), NA, educ25_val),
    educ25_val = case_when(
      !is.na(educ25_val) ~ educ25_val,
      W8VCQU0Q == 1 ~ -8,
      W8VCQU0R == 1 ~ -9,
      W8DHANVQH == -9 ~ -9,
      W8DHANVQH == -8 ~ -8,
      W8DHANVQH == -1 ~ -1,
      TRUE ~ -3
    )
  )

# --- educ32 ---
full_cohort <- full_cohort %>%
  mutate(
    a32_tier = map_dbl(W9DANVQH, map_nvq_level),
    v32_tier = map_dbl(W9DVNVQH, map_nvq_level),
    educ32_val = pmin(a32_tier, v32_tier, na.rm = TRUE),
    educ32_val = ifelse(is.infinite(educ32_val), NA, educ32_val),
    educ32_val = case_when(
      !is.na(educ32_val) ~ educ32_val,
      W9DANVQH == -9 ~ -9,
      W9DVNVQH == -9 ~ -9,
      W9DANVQH == -8 ~ -8,
      W9DVNVQH == -8 ~ -8,
      W9DANVQH == -1 ~ -1,
      W9DVNVQH == -1 ~ -1,
      TRUE ~ -3
    )
  )

# Factor labels
nvq_labels <- c("0" = "NVQ 4–5 equivalent", "1" = "NVQ 1–3 equivalent", "2" = "Entry level or no qualifications", "3" = "Other qualifications not mappable to NVQ framework", "4" = "None of these qualifications")
missing_labels <- c("-9" = "Refused", "-8" = "Don't know / insufficient information", "-7" = "Prefer not to say", "-3" = "Not asked at the fieldwork stage / not interviewed", "-2" = "Schedule not applicable / script error / information lost", "-1" = "Item not applicable")
all_educ_labels <- c(nvq_labels, missing_labels)

full_cohort <- full_cohort %>%
  mutate(
    educ25 = factor(educ25_val, levels = as.numeric(names(all_educ_labels)), labels = all_educ_labels),
    educ32 = factor(educ32_val, levels = as.numeric(names(all_educ_labels)), labels = all_educ_labels)
  )

# --- educadtl32 and educvdtl32 ---
sub_acad <- sort(names(full_cohort)[grep("W9ACQU0[A-S]", names(full_cohort))])
non_sub_acad <- c("W9ACQU0T", "W9ACQU0U", "W9ACQU0V")

sub_voc <- sort(names(full_cohort)[grep("W9VCQU0[A-Z]", names(full_cohort))])
sub_voc <- sub_voc[!(sub_voc %in% c("W9VCQUAH", "W9VCQUAI"))]
non_sub_voc <- c("W9VCQUAH", "W9VCQUAI")

get_dtl_val <- function(row, sub_vars, non_sub_vars) {
  for (i in seq_along(sub_vars)) {
    val <- row[[sub_vars[i]]]
    if (!is.na(val) && val == 1) return(i)
  }
  sub_vals <- row[sub_vars]
  if (length(sub_vals) > 0 && all(!is.na(sub_vals)) && all(sub_vals == 2)) return(length(sub_vars))
  if (length(sub_vals) > 0 && all(!is.na(sub_vals)) && all(sub_vals == -1)) return(-1)
  if (!is.na(row[[non_sub_vars[1]]]) && row[[non_sub_vars[1]]] == 1) return(-8)
  if (!is.na(row[[non_sub_vars[2]]]) && row[[non_sub_vars[2]]] == 1) return(-9)
  if (length(non_sub_vars) == 3 && !is.na(row[[non_sub_vars[3]]]) && row[[non_sub_vars[3]]] == 1) return(-2)
  return(-3)
}

full_cohort <- full_cohort %>%
  rowwise() %>%
  mutate(
    educadtl32_val = get_dtl_val(cur_data(), sub_acad, non_sub_acad),
    educvdtl32_val = get_dtl_val(cur_data(), sub_voc, non_sub_voc)
  ) %>% ungroup()

acad_sub_labels <- c(
  "Doctorate or equivalent", "Masters or equivalent", "Undergraduate or equivalent", 
  "Post-graduate Diplomas and Certificates", "Diplomas in higher education and other higher education qualifications", 
  "Teaching qualifications for schools or further education (below degree level)", "A/AS Levels or equivalent", 
  "Grade A-C, Level 4-9", "Grade D-G, Level 1-3", "SCE Higher", 
  "Scottish Certificate Sixth Year Studies", "SCE Standard", "National 4 and 5", 
  "National 2 and 3", "Leaving Certificate", "Junior Certificate grade A-C", 
  "Junior Certificate grade D and below", "Other academic qualifications (including overseas)", 
  "None of these qualifications"
)

voc_sub_labels <- c(
  "Professional qualifications at degree level", "Nursing or other medical qualifications (below degree level)", 
  "Level 4 or 5", "Level 3", "Level 2", "Level 1", "GNVQ Advanced", "GNVQ Intermediate", 
  "Level 3", "Level 2", "Level Foundation", "Advanced Craft, Part III", "Craft, Part II", 
  "Craft, Part I", "Level 3", "Level 2", "Level 1", "Advanced Diploma", "Higher Diploma", 
  "RSA Diploma", "RSA Stage I, II,III", "Higher Level BTEC", "BTEC National", "BTEC First", 
  "SCOTVEC National Certificate", "SCOTVEC first or general diploma", "SCOTVEC general diploma", 
  "SCOTVEC modules", "HND or HNC", "OND or ONCM", "Junior certificate", "Other vocational qualifications"
)

full_cohort <- full_cohort %>%
  mutate(
    educadtl32 = factor(educadtl32_val, levels = c(1:19, -8, -9, -2, -3), 
                        labels = c(acad_sub_labels, "Don't know / insufficient information", "Refused", "Schedule not applicable / script error / information lost", "Not asked at the fieldwork stage / not interviewed")),
    educvdtl32 = factor(educvdtl32_val, levels = c(1:32, -8, -9, -2, -3), 
                        labels = c(voc_sub_labels, "Don't know / insufficient information", "Refused", "Schedule not applicable / script error / information lost", "Not asked at the fieldwork stage / not interviewed"))
  )

final_data <- full_cohort %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

write_csv(final_data, 'data/output/cleaned_data.csv')

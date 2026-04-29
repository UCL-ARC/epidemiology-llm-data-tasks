library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab'
)

w1 <- readr::read_delim(paste0('data/input/', files[1]), delim = '\t', col_types = readr::cols(.default = 'c'))
w2 <- readr::read_delim(paste0('data/input/', files[2]), delim = '\t', col_types = readr::cols(.default = 'c'))
w4 <- readr::read_delim(paste0('data/input/', files[3]), delim = '\t', col_types = readr::cols(.default = 'c'))

vars_to_num <- c('W1hiqualmum', 'W1hiqualdad', 'W2hiqualmum', 'W2hiqualdad', 'w4hiqualmum', 'w4hiqualdad')

w1 <- w1 %>% mutate(across(any_of(vars_to_num), as.numeric))
w2 <- w2 %>% mutate(across(any_of(vars_to_num), as.numeric))
w4 <- w4 %>% mutate(across(any_of(vars_to_num), as.numeric))

data_merged <- w1 %>%
  full_join(w2, by = 'NSID') %>%
  full_join(w4, by = 'NSID')

# 6 & 7. Missing Value Harmonisation
harmonise_missing <- function(val) {
  if (is.na(val)) return(-3)
  if (val == -999) return(-2)
  if (val == -99) return(-3)
  if (val == -98) return(-3)
  if (val == -94) return(-8)
  if (val == -92) return(-9)
  if (val == -91) return(-1)
  if (val == -1) return(-8)
  return(val)
}

# 9. Consolidation Logic
consolidate_edu <- function(w1_val, w2_val, w4_val) {
  vals <- c(w1_val, w2_val, w4_val)
  h_vals <- map_dbl(vals, ~harmonise_missing(.x))
  
  pos_vals <- h_vals[h_vals > 0]
  if (length(pos_vals) > 0) return(pos_vals[1])
  
  neg_vals <- h_vals[h_vals < 0]
  if (length(neg_vals) > 0) return(neg_vals[1])
  
  return(-3)
}

data_final <- data_merged %>%
  rowwise() %>%
  mutate(
    educdtlma = consolidate_edu(W1hiqualmum, W2hiqualmum, w4hiqualmum),
    educdtlpa = consolidate_edu(W1hiqualdad, W2hiqualdad, w4hiqualdad)
  ) %>% 
  ungroup()

# 8. Response Category Harmonisation (Collapsed NVQ scheme)
# Using Vectorized approach to avoid "condition has length > 1" error
map_to_nvq_vec <- function(detailed_val) {
  res <- rep(-3, length(detailed_val))
  res[detailed_val < 0] <- detailed_val[detailed_val < 0]
  res[detailed_val >= 1 & detailed_val <= 4] <- 0
  res[detailed_val >= 5 & detailed_val <= 17] <- 1
  res[detailed_val == 18] <- 2
  res[detailed_val == 19] <- 3
  res[detailed_val == 20] <- 4
  return(res)
}

data_final <- data_final %>%
  mutate(
    educma = map_to_nvq_vec(educdtlma),
    educpa = map_to_nvq_vec(educdtlpa)
  )

# 10. Labels
det_labels <- c(
  '1' = 'Higher Degree', '2' = 'First Degree', '3' = 'HE Diploma', '4' = 'HNC/HND/NVQ4',
  '5' = 'Teaching qualification, non-degree', '6' = 'Nursing qualification, non-degree', '7' = 'A Levels',
  '8' = 'OND/ONC', '9' = 'City and guilds part III, NVQ3', '10' = 'CSYS', '11' = 'Scottish Higher Grade',
  '12' = 'AS Level', '13' = 'Trade apprenticeship', '14' = 'City and guilds part II, NVQ2',
  '15' = 'GCSE grade A-C and equivalent', '16' = 'GCSE grade D-E and equivalent', '17' = 'City and guilds part I, NVQ1',
  '18' = 'Youth training, skill seekers', '19' = 'Qualification, level unspecified', '20' = 'No qualification mentioned',
  '-9' = 'Refusal', '-8' = 'Don\'t know / insufficient information', '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed', '-2' = 'Schedule not applicable / script error / information lost', '-1' = 'Item not applicable'
)

coll_labels <- c(
  '0' = 'NVQ 4–5: Higher Degree, First Degree, HE Diploma, HNC/HND/NVQ4',
  '1' = 'NVQ 1–3: everything from teaching/nursing qualifications through City & Guilds Part I / NVQ1',
  '2' = 'Youth training / skill seekers (training below NVQ level)',
  '3' = 'Qualification, level unspecified',
  '4' = 'No qualification mentioned',
  '-9' = 'Refusal', '-8' = 'Don\'t know / insufficient information', '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed', '-2' = 'Schedule not applicable / script error / information lost', '-1' = 'Item not applicable'
)

data_final <- data_final %>%
  mutate(
    educdtlma = factor(educdtlma, levels = as.numeric(names(det_labels)), labels = det_labels),
    educdtlpa = factor(educdtlpa, levels = as.numeric(names(det_labels)), labels = det_labels),
    educma = factor(educma, levels = as.numeric(names(coll_labels)), labels = coll_labels),
    educpa = factor(educpa, levels = as.numeric(names(coll_labels)), labels = coll_labels)
  )

final_output <- data_final %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

readr::write_csv(final_output, 'data/output/cleaned_data.csv')

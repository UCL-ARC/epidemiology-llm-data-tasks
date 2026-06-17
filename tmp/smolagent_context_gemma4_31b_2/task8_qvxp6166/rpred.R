library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
files <- c('wave_one_lsype_young_person_2020.tab', 'wave_four_lsype_young_person_2020.tab', 'ns8_2015_main_interview.tab', 'ns8_2015_derived.tab', 'ns9_2022_main_interview.tab', 'ns9_2022_derived_variables.tab')
data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t', show_col_types = FALSE))
names(data_list) <- files

# Create full cohort frame
full_frame <- data_list[[1]] %>% select(NSID) %>%
  full_join(data_list[[2]] %>% select(NSID), by = 'NSID') %>%
  full_join(data_list[[3]] %>% select(NSID), by = 'NSID') %>%
  full_join(data_list[[4]] %>% select(NSID), by = 'NSID') %>%
  full_join(data_list[[5]] %>% select(NSID), by = 'NSID') %>%
  full_join(data_list[[6]] %>% select(NSID), by = 'NSID')

# Join necessary variables
full_frame <- full_frame %>%
  left_join(data_list[[3]] %>% select(NSID, starts_with('W8VCQU')), by = 'NSID') %>%
  left_join(data_list[[4]] %>% select(NSID, W8DHANVQH), by = 'NSID') %>%
  left_join(data_list[[5]] %>% select(NSID, starts_with('W9ACQU'), starts_with('W9VCQU')), by = 'NSID') %>%
  left_join(data_list[[6]] %>% select(NSID, W9DANVQH, W9DVNVQH), by = 'NSID')

# Helper function for row-wise operation to avoid apply() in mutate
get_voc_level_25 <- function(df) {
  sapply(1:nrow(df), function(i) {
    row <- df[i, ]
    if (!is.na(row[['W8VCQU0Q']]) && row[['W8VCQU0Q']] == 1) return(-8)
    if (!is.na(row[['W8VCQU0R']]) && row[['W8VCQU0R']] == 1) return(-9)
    if ((!is.na(row[['W8VCQU0K']]) && row[['W8VCQU0K']] == 1) || (!is.na(row[['W8VCQU0L']]) && row[['W8VCQU0L']] == 1) || (!is.na(row[['W8VCQU0J']]) && row[['W8VCQU0J']] == 1)) return(0)
    voc_1_3 <- c('W8VCQU0A', 'W8VCQU0B', 'W8VCQU0C', 'W8VCQU0D', 'W8VCQU0E', 'W8VCQU0F', 'W8VCQU0G', 'W8VCQU0H', 'W8VCQU0I', 'W8VCQU0M', 'W8VCQU0N', 'W8VCQU0O')
    if (any(!is.na(row[voc_1_3]) & row[voc_1_3] == 1)) return(1)
    if (!is.na(row[['W8VCQU0P']]) && row[['W8VCQU0P']] == 1) return(4)
    return(NA_real_)
  })
}

# --- educ25 ---
full_frame <- full_frame %>%
  mutate(
    acad_level_25 = case_when(
      W8DHANVQH %in% 4:5 ~ 0, W8DHANVQH %in% 1:3 ~ 1, W8DHANVQH == 96 ~ 4, W8DHANVQH == 95 ~ 3, 
      W8DHANVQH == -9 ~ -9, W8DHANVQH == -8 ~ -8, W8DHANVQH == -1 ~ -1, TRUE ~ NA_real_
    )
  )

full_frame$voc_level_25 <- get_voc_level_25(full_frame)

full_frame <- full_frame %>%
  mutate(
    educ25 = pmax(acad_level_25, voc_level_25, na.rm = TRUE),
    educ25 = case_when(is.na(educ25) ~ -3, TRUE ~ educ25)
  )

# --- educ32 ---
full_frame <- full_frame %>%
  mutate(
    acad_level_32 = case_when(
      W9DANVQH %in% 4:5 ~ 0, W9DANVQH %in% 1:3 ~ 1, W9DANVQH == 0 ~ 2, W9DANVQH == 96 ~ 4, W9DANVQH == 95 ~ 3, 
      TRUE ~ W9DANVQH
    ),
    voc_level_32 = case_when(
      W9DVNVQH %in% 4:5 ~ 0, W9DVNVQH %in% 1:3 ~ 1, W9DVNVQH == 0 ~ 2, W9DVNVQH == 96 ~ 4, W9DVNVQH == 95 ~ 3, 
      TRUE ~ W9DVNVQH
    ),
    educ32 = pmax(acad_level_32, voc_level_32, na.rm = TRUE),
    educ32 = case_when(is.na(educ32) ~ -3, TRUE ~ educ32)
  )

# --- educadtl32 ---
acad_vars <- c('W9ACQU0A', 'W9ACQU0B', 'W9ACQU0C', 'W9ACQU0D', 'W9ACQU0E', 'W9ACQU0F', 'W9ACQU0G', 'W9ACQU0H', 'W9ACQU0I', 'W9ACQU0J', 'W9ACQU0K', 'W9ACQU0L', 'W9ACQU0M', 'W9ACQU0N', 'W9ACQU0O', 'W9ACQU0P', 'W9ACQU0Q', 'W9ACQU0R', 'W9ACQU0S')
full_frame$educadtl32 <- sapply(1:nrow(full_frame), function(i) {
  row <- full_frame[i, ]
  if (!is.na(row[['W9ACQU0T']]) && row[['W9ACQU0T']] == 1) return(-8)
  if (!is.na(row[['W9ACQU0U']]) && row[['W9ACQU0U']] == 1) return(-9)
  if (!is.na(row[['W9ACQU0V']]) && row[['W9ACQU0V']] == 1) return(-2)
  sub_vals <- as.numeric(row[acad_vars])
  if (all(is.na(sub_vals) | sub_vals == -1)) return(-1)
  if (all(is.na(sub_vals) | sub_vals == -3)) return(-3)
  found <- which(sub_vals == 1)
  if (length(found) > 0) return(found[1])
  if (all(sub_vals == 2, na.rm = TRUE)) return(length(acad_vars) + 1)
  return(-3)
})

# --- educvdtl32 ---
voc_vars <- c('W9VCQU0A', 'W9VCQU0B', 'W9VCQU0C', 'W9VCQU0D', 'W9VCQU0E', 'W9VCQU0F', 'W9VCQU0G', 'W9VCQU0H', 'W9VCQU0I', 'W9VCQU0J', 'W9VCQU0K', 'W9VCQU0L', 'W9VCQU0M', 'W9VCQU0N', 'W9VCQU0O', 'W9VCQU0P', 'W9VCQU0Q', 'W9VCQU0R', 'W9VCQU0S', 'W9VCQU0T', 'W9VCQU0U', 'W9VCQU0V', 'W9VCQU0W', 'W9VCQU0X', 'W9VCQU0Y', 'W9VCQU0Z', 'W9VCQUAA', 'W9VCQUAB', 'W9VCQUAC', 'W9VCQUAD', 'W9VCQUAE', 'W9VCQUAF', 'W9VCQUAG')
full_frame$educvdtl32 <- sapply(1:nrow(full_frame), function(i) {
  row <- full_frame[i, ]
  if (!is.na(row[['W9VCQUAH']]) && row[['W9VCQUAH']] == 1) return(-8)
  if (!is.na(row[['W9VCQUAI']]) && row[['W9VCQUAI']] == 1) return(-9)
  sub_vals <- as.numeric(row[voc_vars])
  if (all(is.na(sub_vals) | sub_vals == -1)) return(-1)
  if (all(is.na(sub_vals) | sub_vals == -3)) return(-3)
  found <- which(sub_vals == 1)
  if (length(found) > 0) return(found[1])
  if (all(sub_vals == 2, na.rm = TRUE)) return(length(voc_vars) + 1)
  return(-3)
})

# Final output
final_data <- full_frame %>% select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Apply NVQ factor labels
nvq_labels <- c("0" = "NVQ 4–5 equivalent", "1" = "NVQ 1–3 equivalent", "2" = "Entry level or no qualifications", "3" = "Other qualifications not mappable to the NVQ framework", "4" = "None of these qualifications")
final_data$educ25 <- factor(final_data$educ25, levels = as.numeric(names(nvq_labels)), labels = nvq_labels)
final_data$educ32 <- factor(final_data$educ32, levels = as.numeric(names(nvq_labels)), labels = nvq_labels)

write_csv(final_data, 'data/output/cleaned_data.csv')
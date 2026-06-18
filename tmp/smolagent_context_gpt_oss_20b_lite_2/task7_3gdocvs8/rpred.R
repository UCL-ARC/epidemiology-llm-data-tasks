library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# Function to convert wave‑specific missing codes to the standard set and
# replace R NA with the "not asked" code (-3).
map_missing <- function(x){
  num <- suppressWarnings(as.numeric(x))
  # Specific wave codes
  num[num == -94] <- NA          # Insufficient information
  num[num == -91] <- -1           # Not applicable (not studying)
  num[num == -92] <- -9           # Refusal
  num[num == -99] <- NA          # Not asked at fieldwork stage
  num[num == -100] <- NA
  num[num == -997] <- NA
  num[num == -998] <- NA
  num[num == -999] <- NA
  # Preserve standard missing codes
  standard_codes <- c(-9,-8,-7,-3,-2,-1)
  # Any other negative value that is not a standard code becomes NA
  num[num < -1 & !num %in% standard_codes] <- NA
  # Replace NA with "not asked"
  num[is.na(num)] <- -3
  return(num)
}

# Mapping tables for the six educational‑aim categories
# 1 = NVQ4/5 (collapsed), 2 = NVQ3, 3 = NVQ2, 4 = NVQ1, 5 = A/AS or GCSE, 6 = Not studying
w4_map <- c(`1`=2, `2`=1, `3`=5, `4`=5, `5`=3, `6`=2, `7`=3, `8`=5, `9`=4, `10`=5, `11`=4, `12`=5, `13`=NA, `14`=6)
w6_map <- c(`1`=1, `2`=1, `3`=1, `4`=1, `5`=2, `6`=1, `7`=5, `8`=5, `9`=3, `10`=3, `11`=5, `12`=4, `13`=4, `14`=NA, `15`=NA, `16`=6)
w7_map <- c(`-94`=NA, `-91`=6, `1`=4, `2`=4, `3`=3, `4`=5, `5`=3, `6`=2, `7`=5, `8`=1, `9`=5, `10`=1, `11`=1, `12`=1, `13`=1, `14`=NA)

# ---------- Load Wave 1 (ID only) ----------

df_wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab',
                       delim = '\t', col_types = cols(.default = col_character())) %>%
  select(NSID)

# ---------- Load Wave 4 (age 17) ----------

df_wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab',
                       delim = '\t', col_types = cols(.default = col_character())) %>%
  mutate(w4saim = map_missing(w4saim)) %>%
  mutate(educaim17 = w4_map[as.character(w4saim)]) %>%
  select(NSID, educaim17)

# ---------- Load Wave 6 (age 19) ----------

df_wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab',
                       delim = '\t', col_types = cols(.default = col_character())) %>%
  mutate(W6Saim = map_missing(W6Saim)) %>%
  mutate(educaim19 = w6_map[as.character(W6Saim)]) %>%
  select(NSID, educaim19)

# ---------- Load Wave 7 (age 20) ----------

df_wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab',
                       delim = '\t', col_types = cols(.default = col_character())) %>%
  mutate(W7SAim = map_missing(W7SAim)) %>%
  mutate(educaim20 = w7_map[as.character(W7SAim)]) %>%
  select(NSID, educaim20)

# ---------- Load Wave 8 (age 25) ----------

df_wave8 <- read_delim('data/input/ns8_2015_main_interview.tab',
                       delim = '\t', col_types = cols(.default = col_character())) %>%
  mutate(W8ACTIVITY05 = map_missing(W8ACTIVITY05)) %>%
  mutate(W8VCQUC0J = map_missing(W8VCQUC0J))

# Academic qualification variables: W8ACQUC0A – W8ACQUC0U (A–U)
acad_vars8 <- paste0('W8ACQUC0', LETTERS[1:21])
acad_vars8 <- acad_vars8[acad_vars8 %in% colnames(df_wave8)]

# Convert academic vars and replace "Not asked" (-3) with 0 for flagging
for(v in acad_vars8){
  df_wave8[[v]] <- map_missing(df_wave8[[v]])
  df_wave8[[v]][df_wave8[[v]] == -3] <- 0
}

# Matrix of academic responses
acad_mat8 <- df_wave8 %>% select(all_of(acad_vars8)) %>% as.matrix()

# Flag if any academic qualification is reported
acad_any_flag <- rowSums(acad_mat8) > 0

# Derive educaim25
# Rules: not studying (0) -> 6; NVQ3 (W8VCQUC0J==1) -> 1; any academic -> 5; otherwise NA

df_wave8 <- df_wave8 %>%
  mutate(educaim25 = case_when(
    W8ACTIVITY05 == 0 ~ 6,
    W8VCQUC0J == 1 ~ 1,
    acad_any_flag ~ 5,
    TRUE ~ NA_real_
  )) %>%
  select(NSID, educaim25)

# ---------- Load Wave 9 (age 32) ----------

df_wave9 <- read_delim('data/input/ns9_2022_main_interview.tab',
                       delim = '\t', col_types = cols(.default = col_character())) %>%
  mutate(W9ECONACT2 = map_missing(W9ECONACT2))

# Define category variable lists (subset to those present in the data)
cat1_vars <- c('W9ACQUC0A','W9ACQUC0B','W9ACQUC0C','W9ACQUC0D','W9ACQUC0E','W9VCQUC0A','W9VCQUC0C','W9VCQUC0D','W9VCQUC0E','W9VCQUC0F','W9VCQUC0G','W9VCQUC0H','W9VCQUC0I')
cat2_vars <- c('W9VCQUC0J','W9VCQUC0K','W9VCQUC0L','W9VCQUC0M','W9VCQUC0N','W9VCQUC0O','W9VCQUC0P','W9VCQUC0Q','W9VCQUC0R','W9VCQUC0S','W9VCQUC0T','W9VCQUC0U','W9VCQUC0V','W9VCQUC0W','W9VCQUC0X','W9VCQUC0Y','W9VCQUC0Z','W9VCQUCAA','W9VCQUCAB','W9VCQUCAC','W9VCQUCAD','W9VCQUCAE','W9VCQUCAF','W9VCQUCAG','W9VCQUCAH','W9VCQUCAI')
cat3_vars <- c('W9VCQUC0B')
cat4_vars <- c('W9ACQUC0L','W9ACQUC0M')
cat5_vars <- c('W9ACQUC0I')

existing_cat_vars <- setdiff(c(cat1_vars,cat2_vars,cat3_vars,cat4_vars,cat5_vars), setdiff(colnames(df_wave9), colnames(df_wave9)))
# Determine which category variables actually exist
existing_cat_vars <- intersect(c(cat1_vars,cat2_vars,cat3_vars,cat4_vars,cat5_vars), colnames(df_wave9))

# Convert all potential category vars to numeric and replace "Not asked" (-3) with 0
for(v in existing_cat_vars){
  df_wave9[[v]] <- map_missing(df_wave9[[v]])
  df_wave9[[v]][df_wave9[[v]] == -3] <- 0
}

# Helper to compute flag for a list of variables
compute_flag <- function(vars){
  if(length(vars)==0) return(rep(FALSE, nrow(df_wave9)))
  mat <- df_wave9 %>% select(all_of(vars)) %>% as.matrix()
  rowSums(mat) > 0
}

cat1_flag <- compute_flag(intersect(cat1_vars, existing_cat_vars))
cat2_flag <- compute_flag(intersect(cat2_vars, existing_cat_vars))
cat3_flag <- compute_flag(intersect(cat3_vars, existing_cat_vars))
cat4_flag <- compute_flag(intersect(cat4_vars, existing_cat_vars))
cat5_flag <- compute_flag(intersect(cat5_vars, existing_cat_vars))

# Studying status: W9ECONACT2 6 or 7 represent full‑time or part‑time education
studying <- df_wave9$W9ECONACT2 %in% c(6,7)

# Derive educaim32

df_wave9 <- df_wave9 %>%
  mutate(educaim32 = case_when(
    !studying ~ 6,
    cat1_flag ~ 1,
    cat2_flag ~ 2,
    cat3_flag ~ 3,
    cat4_flag ~ 4,
    cat5_flag ~ 5,
    TRUE ~ NA_real_
  )) %>%
  select(NSID, educaim32)

# ---------- Merge all waves ----------

df_merged <- df_wave1 %>%
  full_join(df_wave4, by='NSID') %>%
  full_join(df_wave6, by='NSID') %>%
  full_join(df_wave7, by='NSID') %>%
  full_join(df_wave8, by='NSID') %>%
  full_join(df_wave9, by='NSID')

# ---------- Write cleaned data ----------
write_csv(df_merged, 'data/output/cleaned_data.csv')

library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns9_2022_main_interview.tab'
)

load_tab <- function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character()))
}

data_list <- map(files, load_tab)

# Merge datasets
full_df <- reduce(data_list, full_join, by = 'NSID')

# Harmonisation Mapping for educaim
map_w4 <- function(x) {
  case_when(
    x == 10 ~ 1, x == 9 ~ 1, x == 11 ~ 1, x == 8 ~ 1, # L1/GCSE
    x == 5 ~ 2, x == 6 ~ 2, x == 7 ~ 2, # L2
    x == 1 ~ 3, x == 2 ~ 3, x == 3 ~ 3, x == 4 ~ 3, # L3
    x == 12 ~ 7, x == 13 ~ 7, x == 14 ~ 6, # Other/Not studying
    x <= -1 ~ x, # Keep missing
    TRUE ~ -3
  )
}

map_w6 <- function(x) {
  case_when(
    x == 12 ~ 1, x == 13 ~ 1, x == 11 ~ 1, # L1
    x == 9 ~ 2, x == 10 ~ 2, # L2
    x == 5 ~ 3, x == 6 ~ 3, x == 7 ~ 3, x == 8 ~ 3, # L3
    x == 3 ~ 4, x == 1 ~ 4, # L4/5
    x == 2 ~ 5, x == 4 ~ 5, # Degree
    x == 14 ~ 7, x == 15 ~ 7, x == 16 ~ 6, # Other/Not studying
    x <= -1 ~ x, # Keep missing
    TRUE ~ -3
  )
}

map_w7 <- function(x) {
  case_when(
    x == 1 ~ 1, x == 2 ~ 1, x == 4 ~ 1, # L1
    x == 3 ~ 2, x == 5 ~ 2, # L2
    x == 6 ~ 3, x == 7 ~ 3, x == 8 ~ 3, x == 9 ~ 3, # L3
    x == 10 ~ 4, x == 13 ~ 4, # L4/5
    x == 11 ~ 5, x == 12 ~ 5, # Degree
    x == 14 ~ 7, # Other
    x == -91 ~ 6, # Not studying
    x == -94 ~ -8, # Insufficient info
    x <= -1 ~ x, # Other missing
    TRUE ~ -3
  )
}

map_w8 <- function(df) {
  res <- rep(-3, nrow(df))
  deg_plus <- (df$W8ACQUC0A == 1 | df$W8ACQUC0B == 1 | df$W8ACQUC0C == 1 | df$W8ACQUC0E == 1)
  l45 <- (df$W8VCQUC0J == 1 | df$W8VCQUC0K == 1)
  l3 <- (df$W8ACQUC0F == 1 | df$W8ACQUC0G == 1 | df$W8ACQUC0H == 1 | df$W8ACQUC0I == 1 | df$W8ACQUC0J == 1 | df$W8ACQUC0K == 1 | df$W8VCQUC0D == 1)
  l2 <- (df$W8ACQUC0L == 1 | df$W8VCQUC0E == 1 | df$W8VCQUC0J == 1)
  l1 <- (df$W8ACQUC0M == 1 | df$W8ACQUC0N == 1 | df$W8VCQUC0A == 1 | df$W8VCQUC0B == 1 | df$W8VCQUC0C == 1)
  not_studying <- (df$W8ACTIVITY05 == 0)
  
  res[deg_plus] <- 5
  res[l45 & res == -3] <- 4
  res[l3 & res == -3] <- 3
  res[l2 & res == -3] <- 2
  res[l1 & res == -3] <- 1
  res[not_studying & res == -3] <- 6
  return(res)
}

map_w9 <- function(df) {
  res <- rep(-3, nrow(df))
  deg_plus <- (df$W9ACQUC0A == 1 | df$W9ACQUC0B == 1 | df$W9ACQUC0C == 1 | df$W9ACQUC0D == 1 | df$W9ACQUC0E == 1)
  l45 <- (df$W9VCQUC0C == 1 | df$W9VCQUC0D == 1 | df$W9VCQUC0R == 1 | df$W9VCQUC0S == 1)
  l3 <- (df$W9ACQUC0F == 1 | df$W9ACQUC0G == 1 | df$W9VCQUC0D == 1 | df$W9VCQUC0I == 1 | df$W9VCQUC0O == 1)
  l2 <- (df$W9ACQUC0H == 1 | df$W9VCQUC0E == 1 | df$W9VCQUC0J == 1 | df$W9VCQUC0P == 1)
  l1 <- (df$W9ACQUC0I == 1 | df$W9ACQUC0J == 1 | df$W9ACQUC0K == 1 | df$W9ACQUC0L == 1 | df$W9ACQUC0M == 1 | df$W9ACQUC0N == 1 | df$W9VCQUC0F == 1 | df$W9VCQUC0G == 1 | df$W9VCQUC0H == 1 | df$W9VCQUC0K == 1)
  not_studying <- (df$W9ECONACT2 != 6 & df$W9ECONACT2 != 7 & df$W9ECONACT2 >= 1)
  
  res[deg_plus] <- 5
  res[l45 & res == -3] <- 4
  res[l3 & res == -3] <- 3
  res[l2 & res == -3] <- 2
  res[l1 & res == -3] <- 1
  res[not_studying & res == -3] <- 6
  return(res)
}

full_df <- full_df %>%
  mutate(
    educaim17 = map_w4(w4saim),
    educaim19 = map_w6(W6Saim),
    educaim20 = map_w7(W7SAim)
  )

full_df$educaim25 <- map_w8(full_df)
full_df$educaim32 <- map_w9(full_df)

final_df <- full_df %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32) %>%
  mutate(across(starts_with('educaim'), ~ { 
    x <- .x
    x[is.na(x)] <- -3
    x
  }))

edu_labels <- c(
  '1' = 'Level 1',
  '2' = 'Level 2',
  '3' = 'Level 3',
  '4' = 'Level 4/5',
  '5' = 'Degree+',
  '6' = 'Not studying',
  '7' = 'Other',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked',
  '-2' = 'Not applicable',
  '-1' = 'Item not applicable'
)

# Convert labels to numeric keys for val_labels
edu_labels_num <- setNames(as.numeric(names(edu_labels)), names(edu_labels))

final_df <- final_df %>%
  mutate(across(starts_with('educaim'), ~ {
    var <- .x
    val_labels(var) <- edu_labels_num
    as_factor(var)
  }))

write_csv(final_df, 'data/output/cleaned_data.csv')

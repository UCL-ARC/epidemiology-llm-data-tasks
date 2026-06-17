library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns9_2022_main_interview.tab'
)

data_list <- lapply(files, function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(.default = "c"))
})

names(data_list) <- files

# Merge datasets
full_frame <- data_list[[1]] %>% 
  select(NSID) %>% 
  full_join(data_list[[2]] %>% select(NSID, w4saim), by = 'NSID') %>% 
  full_join(data_list[[3]] %>% select(NSID, W6Saim), by = 'NSID') %>% 
  full_join(data_list[[4]] %>% select(NSID, W7SAim), by = 'NSID') %>% 
  full_join(data_list[[5]] %>% select(NSID, W8ACTIVITY05, starts_with('W8ACQUC0'), starts_with('W8VCQUC')), by = 'NSID') %>% 
  full_join(data_list[[6]] %>% select(NSID, W9ECONACT2, starts_with('W9ACQUC0'), starts_with('W9VCQUC')), by = 'NSID')

# Helper to map to 6-category NVQ scheme
# 1: Level 1, 2: Level 2, 3: Level 3, 4: Level 4, 5: Level 5, 6: Level 6+

# Wave 4 (Age 17)
full_frame <- full_frame %>% mutate(
  educaim17 = as.numeric(w4saim) %>% {
    res <- rep(NA, length(.))
    res[ . %in% c(9, 10, 11) ] <- 1
    res[ . %in% c(5, 6, 7, 8) ] <- 2
    res[ . %in% c(1, 2, 3, 4) ] <- 3
    res[ . == 12 ] <- 3 # Other
    res[ . == 13 ] <- -3 # No detail
    res[ . == 14 ] <- -1 # Not studying
    res
  }
)

# Wave 6 (Age 19)
full_frame <- full_frame %>% mutate(
  educaim19 = as.numeric(W6Saim) %>% {
    res <- rep(NA, length(.))
    res[ . %in% c(12, 13) ] <- 1
    res[ . %in% c(9, 10, 11) ] <- 2
    res[ . %in% c(5, 6, 7, 8) ] <- 3
    res[ . == 3 ] <- 4
    res[ . %in% c(1, 2, 4) ] <- 5
    res[ . == 14 ] <- 3 # Other
    res[ . == 15 ] <- -3 # No detail
    res[ . == 16 ] <- -1 # Not studying
    res
  }
)

# Wave 7 (Age 20)
full_frame <- full_frame %>% mutate(
  educaim20 = as.numeric(W7SAim) %>% {
    res <- rep(NA, length(.))
    res[ . %in% c(1, 2) ] <- 1
    res[ . %in% c(3, 4, 5) ] <- 2
    res[ . %in% c(6, 7, 8, 9) ] <- 3
    res[ . == 10 ] <- 4
    res[ . %in% c(11, 12, 13) ] <- 5
    res[ . == 14 ] <- 3 # Other
    res[ . == -91 ] <- -1 # Not studying
    res[ . == -94 ] <- -8 # Insufficient
    res
  }
)

# Wave 8 (Age 25)
# Logic: highest qualification being studied
full_frame <- full_frame %>% mutate(
  educaim25 = case_when(
    W8ACTIVITY05 == '0' ~ -1, # Not in education
    W8ACQUC0A == '1' ~ 6,
    W8ACQUC0B == '1' ~ 5,
    W8ACQUC0C == '1' ~ 4,
    W8ACQUC0D == '1' | W8ACQUC0E == '1' ~ 4,
    W8ACQUC0F == '1' | W8ACQUC0G == '1' | W8ACQUC0H == '1' | W8ACQUC0I == '1' | W8ACQUC0J == '1' | W8ACQUC0K == '1' ~ 3,
    W8VCQUC0J == '1' | W8VCQUC0K == '1' ~ 4,
    W8ACQUC0L == '1' | W8ACQUC0M == '1' | W8ACQUC0N == '1' ~ 2,
    W8VCQUC0A == '1' | W8VCQUC0B == '1' | W8VCQUC0C == '1' | W8VCQUC0D == '1' ~ 1,
    TRUE ~ -3
  )
)

# Wave 9 (Age 32)
full_frame <- full_frame %>% mutate(
  educaim32 = case_when(
    W9ECONACT2 %in% c('1','2','3','4','5','8','9','10','11','13','14') ~ -1,
    W9ACQUC0A == '1' ~ 6,
    W9ACQUC0B == '1' ~ 5,
    W9ACQUC0C == '1' ~ 4,
    W9ACQUC0D == '1' | W9ACQUC0E == '1' ~ 4,
    W9ACQUC0F == '1' | W9ACQUC0G == '1' | W9ACQUC0J == '1' | W9ACQUC0K == '1' ~ 3,
    W9ACQUC0H == '1' | W9ACQUC0I == '1' | W9ACQUC0L == '1' | W9ACQUC0M == '1' | W9ACQUC0N == '1' ~ 2,
    W9ACQUC0O == '1' | W9ACQUC0P == '1' | W9ACQUC0Q == '1' ~ 1,
    W9VCQUC0A == '1' ~ 6,
    W9VCQUC0B == '1' ~ 4,
    W9VCQUC0C == '1' ~ 4,
    W9VCQUC0D == '1' | W9VCQUC0I == '1' | W9VCQUC0O == '1' ~ 3,
    W9VCQUC0E == '1' | W9VCQUC0J == '1' | W9VCQUC0P == '1' ~ 2,
    W9VCQUC0F == '1' | W9VCQUC0Q == '1' ~ 1,
    TRUE ~ -3
  )
)

# Final Clean and Missing Values
final_vars <- c('NSID', 'educaim17', 'educaim19', 'educaim20', 'educaim25', 'educaim32')
output_df <- full_frame %>% select(all_of(final_vars))

# Replace NA with -3
output_df <- output_df %>% mutate(across(where(is.numeric), ~replace_na(., -3)))

# Apply factor labels (Common 6-category scheme)
level_labels <- c("Level 1" = 1, "Level 2" = 2, "Level 3" = 3, "Level 4" = 4, "Level 5" = 5, "Level 6+" = 6, "Not studying" = -1, "No detail" = -3, "Insufficient" = -8)

# We keep them as numeric for the output CSV as per typical requirements, but ensure missing codes are correct

write_csv(output_df, 'data/output/cleaned_data.csv')

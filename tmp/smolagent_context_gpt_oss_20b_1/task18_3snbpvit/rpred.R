library(readr)
library(dplyr)

load_tab <- function(file){
  read_delim(paste0('data/input/',file), delim='\t', col_types=cols(.default='c'))
}

# load files
wave_one <- load_tab('wave_one_lsype_young_person_2020.tab')
wave_two <- load_tab('wave_two_lsype_young_person_2020.tab')
wave_four <- load_tab('wave_four_lsype_young_person_2020.tab')
ns8_self <- load_tab('ns8_2015_self_completion.tab')
ns8_derived <- load_tab('ns8_2015_derived.tab')
ns9_main <- load_tab('ns9_2022_main_interview.tab')
ns9_derived <- load_tab('ns9_2022_derived_variables.tab')

# merge
all_data <- wave_one %>%
  full_join(wave_two, by='NSID') %>%
  full_join(wave_four, by='NSID') %>%
  full_join(ns8_self, by='NSID') %>%
  full_join(ns8_derived, by='NSID') %>%
  full_join(ns9_main, by='NSID') %>%
  full_join(ns9_derived, by='NSID')

# convert to numeric
numeric_cols <- setdiff(names(all_data), 'NSID')
all_data <- all_data %>% mutate(across(all_of(numeric_cols), ~ as.numeric(.)))

# harmonise pre‑derived
harmonise_pre <- function(vec, wave){
  vec <- ifelse(is.na(vec), NA, vec)
  if(wave %in% c('W2','W4')) vec <- ifelse(vec %in% c(-97,-92), -9, vec)
  vec <- case_when(
    vec %in% c(-99) ~ -3,
    vec %in% c(-98) ~ -2,
    vec %in% c(-96) ~ -8,
    vec %in% c(-91) ~ -1,
    TRUE ~ vec
  )
  vec
}

# compute item‑summed GHQ12
compute_ghqtl <- function(df, pref){
  names <- paste0(pref,'_',1:12)
  existing <- names[names %in% names(df)]
  if(length(existing) < 12) return(rep(-3, nrow(df)))
  items <- df %>% select(all_of(existing))
  apply(items, 1, function(row){
    if(all(is.na(row))) return(-3)
    if(any(row < 0, na.rm=TRUE)) return(-8)
    sum(row-1, na.rm=TRUE)
  })
}

# variables
ghqtl15 <- compute_ghqtl(all_data,'W2GHQ12')
ghq15 <- harmonise_pre(all_data$W2ghq12scr,'W2')
ghqtl17 <- compute_ghqtl(all_data,'W4GHQ12')
ghq17 <- harmonise_pre(all_data$W4ghq12scr,'W4')
ghqtl25 <- compute_ghqtl(all_data,'W8GHQ12')
ghq25 <- harmonise_pre(all_data$W8DGHQSC,'W8')
ghqtl32 <- compute_ghqtl(all_data,'W9GHQ12')
ghq32 <- harmonise_pre(all_data$W9DGHQSC,'W9')

final_df <- data.frame(NSID=all_data$NSID, ghqtl15=ghqtl15, ghq15=ghq15, ghqtl17=ghqtl17, ghq17=ghq17, ghqtl25=ghqtl25, ghq25=ghq25, ghqtl32=ghqtl32, ghq32=ghq32)
write_csv(final_df, 'data/output/cleaned_data.csv')
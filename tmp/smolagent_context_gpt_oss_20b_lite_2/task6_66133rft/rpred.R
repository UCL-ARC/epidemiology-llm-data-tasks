library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

recode_missing <- function(x) {
  if (is.null(x)) return(x)
  x_num <- as.numeric(x)
  x_num[x_num == -94] <- -8
  standard_missing <- c(-9, -8, -7, -3, -2, -1)
  x_num[x_num < 0 & !(x_num %in% standard_missing)] <- -2
  return(x_num)
}

files <- list(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab',
  'ns9_2022_main_interview.tab'
)

datasets <- lapply(files, function(f) {
  path <- file.path('data', 'input', f)
  df <- read_delim(path, delim = '\t', col_types = cols(.default = col_guess()))
  if (grepl('wave_two', f)) {
    df <- df %>% rename(urbind15 = urbind, gor15 = gor)
  }
  if (grepl('wave_three', f)) {
    df <- df %>% rename(urbind16 = urbind, gor16 = gor)
  }
  if (grepl('ns8_2015', f)) {
    df <- df %>% rename(regor25_source = W8DGOR)
  }
  if (grepl('ns9_2022_derived', f)) {
    df <- df %>% rename(regor32_source = W9DRGN)
  }
  if (grepl('ns9_2022_main', f)) {
    df <- df %>% rename(nation_res_source = W9NATIONRES)
  }
  return(df)
})
names(datasets) <- files

merged <- reduce(datasets, function(x, y) full_join(x, y, by = 'NSID'))

merged <- merged %>%
  mutate(
    regub15 = recode_missing(urbind15),
    regub16 = recode_missing(urbind16),
    regov15 = recode_missing(gor15),
    regov16 = recode_missing(gor16),
    regor25 = recode_missing(regor25_source),
    regor32 = recode_missing(regor32_source),
    nation_res = recode_missing(nation_res_source),
    regint32 = case_when(
      nation_res %in% c(1, 2, 3, 4) ~ 1L,
      nation_res == 5 ~ 2L,
      nation_res %in% c(-9, -8, -7, -3, -2, -1) ~ nation_res,
      TRUE ~ NA_integer_
    )
  )

final_df <- merged %>% select(
  NSID,
  regub15,
  regub16,
  regov15,
  regov16,
  regor25,
  regor32,
  regint32
)

output_path <- file.path('data', 'output', 'cleaned_data.csv')
write_csv(final_df, output_path)

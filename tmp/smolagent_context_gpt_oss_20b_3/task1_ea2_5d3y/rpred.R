library(readr)
library(dplyr)
library(tidyr)
library(purrr)

clean_sex <- function(x) {
  case_when(
    x %in% c(1,2) ~ as.integer(x),
    TRUE ~ NA_integer_
  )
}

read_wave <- function(file, var, suffix) {
  df <- read_delim(file, delim = "\t", col_types = cols(NSID = col_character()))
  raw_name <- paste0("sex_", suffix, "_raw")
  clean_name <- paste0("sex_", suffix, "_clean")
  df <- df %>% select(NSID, !!sym(var)) %>% rename(!!raw_name := !!sym(var))
  df <- df %>% mutate(!!clean_name := clean_sex(!!sym(raw_name)))
  df %>% select(NSID, !!clean_name)
}

wave1 <- read_wave("data/input/wave_one_lsype_young_person_2020.tab", "W1sexYP", "1")
wave2 <- read_wave("data/input/wave_two_lsype_young_person_2020.tab", "W2SexYP", "2")
wave3 <- read_wave("data/input/wave_three_lsype_young_person_2020.tab", "W3sexYP", "3")
wave4 <- read_wave("data/input/wave_four_lsype_young_person_2020.tab", "W4SexYP", "4")
wave5 <- read_wave("data/input/wave_five_lsype_young_person_2020.tab", "W5SexYP", "5")
wave6 <- read_wave("data/input/wave_six_lsype_young_person_2020.tab", "W6Sex", "6")
wave7 <- read_wave("data/input/wave_seven_lsype_young_person_2020.tab", "W7Sex", "7")
wave8 <- read_wave("data/input/ns8_2015_main_interview.tab", "W8CMSEX", "8")
wave9 <- read_wave("data/input/ns9_2022_main_interview.tab", "W9DSEX", "9")

all_waves <- list(wave1, wave2, wave3, wave4, wave5, wave6, wave7, wave8, wave9)
merged <- reduce(all_waves, full_join, by = "NSID")

merged <- merged %>%
  mutate(sex = coalesce(sex_9_clean, sex_1_clean, sex_2_clean, sex_3_clean, sex_4_clean,
                       sex_5_clean, sex_6_clean, sex_7_clean, sex_8_clean),
         sex = replace_na(sex, -3))

final_df <- merged %>% select(NSID, sex)
write_csv(final_df, "data/output/cleaned_data.csv")
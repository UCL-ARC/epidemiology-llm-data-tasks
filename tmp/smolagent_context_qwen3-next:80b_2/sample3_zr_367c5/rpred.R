library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

wave1 <- wave1 %>% rename(lang_S1 = W1englangYP)
wave2 <- wave2 %>% rename(lang_S2 = W2EnglangYP)
wave3 <- wave3 %>% rename(hh_lang_S3 = W3englangHH)
wave4 <- wave4 %>% rename(hh_lang_S4 = W4EngLangHH)

merged_data <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

merged_data$lang_S1 <- case_when(
  is.na(merged_data$lang_S1) ~ -3,
  merged_data$lang_S1 == -99 ~ -3,
  merged_data$lang_S1 == -92 ~ -9,
  merged_data$lang_S1 == -91 ~ -1,
  merged_data$lang_S1 == -1 ~ -8,
  TRUE ~ merged_data$lang_S1
)

merged_data$lang_S2 <- case_when(
  is.na(merged_data$lang_S2) ~ -3,
  merged_data$lang_S2 == -998 ~ -2,
  merged_data$lang_S2 == -997 ~ -2,
  merged_data$lang_S2 == -995 ~ -2,
  merged_data$lang_S2 == -99 ~ -3,
  merged_data$lang_S2 == -92 ~ -9,
  merged_data$lang_S2 == -91 ~ -1,
  merged_data$lang_S2 == -1 ~ -8,
  TRUE ~ merged_data$lang_S2
)

merged_data$hh_lang_S3 <- case_when(
  is.na(merged_data$hh_lang_S3) ~ -3,
  merged_data$hh_lang_S3 == -999 ~ -2,
  merged_data$hh_lang_S3 == -997 ~ -2,
  merged_data$hh_lang_S3 == -99 ~ -3,
  merged_data$hh_lang_S3 == -92 ~ -9,
  merged_data$hh_lang_S3 == -91 ~ -1,
  merged_data$hh_lang_S3 == -1 ~ -8,
  TRUE ~ merged_data$hh_lang_S3
)

merged_data$hh_lang_S4 <- case_when(
  is.na(merged_data$hh_lang_S4) ~ -3,
  merged_data$hh_lang_S4 == -999 ~ -2,
  merged_data$hh_lang_S4 == -997 ~ -2,
  merged_data$hh_lang_S4 == -92 ~ -9,
  merged_data$hh_lang_S4 == -91 ~ -1,
  merged_data$hh_lang_S4 == -1 ~ -8,
  TRUE ~ merged_data$hh_lang_S4
)

merged_data$lang <- case_when(
  !is.na(merged_data$lang_S1) & merged_data$lang_S1 > 0 ~ merged_data$lang_S1,
  !is.na(merged_data$lang_S2) & merged_data$lang_S2 > 0 ~ merged_data$lang_S2,
  !is.na(merged_data$lang_S1) ~ merged_data$lang_S1,
  !is.na(merged_data$lang_S2) ~ merged_data$lang_S2,
  TRUE ~ -3
)

merged_data$hh_lang <- case_when(
  !is.na(merged_data$hh_lang_S3) & merged_data$hh_lang_S3 > 0 ~ merged_data$hh_lang_S3,
  !is.na(merged_data$hh_lang_S4) & merged_data$hh_lang_S4 > 0 ~ merged_data$hh_lang_S4,
  !is.na(merged_data$hh_lang_S3) ~ merged_data$hh_lang_S3,
  !is.na(merged_data$hh_lang_S4) ~ merged_data$hh_lang_S4,
  TRUE ~ -3
)

cleaned_data <- merged_data %>% select(NSID, lang, hh_lang)

write_csv(cleaned_data, "data/output/cleaned_data.csv")
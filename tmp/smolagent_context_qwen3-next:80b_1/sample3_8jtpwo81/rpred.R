library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

df1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
df2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
df3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
df4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

df1 <- df1 %>% select(NSID, W1englangYP) %>% rename(lang_S1 = W1englangYP)
df2 <- df2 %>% select(NSID, W2EnglangYP) %>% rename(lang_S2 = W2EnglangYP)
df3 <- df3 %>% select(NSID, W3englangHH) %>% rename(lang_S3 = W3englangHH)
df4 <- df4 %>% select(NSID, W4EngLangHH) %>% rename(lang_S4 = W4EngLangHH)

df1$lang_S1 <- case_when(
  df1$lang_S1 == -99 ~ -3,
  df1$lang_S1 == -92 ~ -9,
  df1$lang_S1 == -91 ~ -1,
  df1$lang_S1 == -1 ~ -8,
  TRUE ~ df1$lang_S1
)

df2$lang_S2 <- case_when(
  df2$lang_S2 %in% c(-998, -997, -995) ~ -2,
  df2$lang_S2 == -99 ~ -3,
  df2$lang_S2 == -92 ~ -9,
  df2$lang_S2 == -91 ~ -1,
  df2$lang_S2 == -1 ~ -8,
  TRUE ~ df2$lang_S2
)

df3$lang_S3 <- case_when(
  df3$lang_S3 %in% c(-999, -997) ~ -2,
  df3$lang_S3 == -99 ~ -3,
  df3$lang_S3 == -92 ~ -9,
  df3$lang_S3 == -91 ~ -1,
  df3$lang_S3 == -1 ~ -8,
  TRUE ~ df3$lang_S3
)

df4$lang_S4 <- case_when(
  df4$lang_S4 %in% c(-999, -997) ~ -2,
  df4$lang_S4 == -92 ~ -9,
  df4$lang_S4 == -91 ~ -1,
  df4$lang_S4 == -1 ~ -8,
  TRUE ~ df4$lang_S4
)

df_merged <- full_join(df1, df2, by = "NSID") %>%
  full_join(df3, by = "NSID") %>%
  full_join(df4, by = "NSID")

df_merged <- df_merged %>%
  mutate(lang = case_when(
    !is.na(lang_S1) & lang_S1 > 0 ~ lang_S1,
    !is.na(lang_S2) & lang_S2 > 0 ~ lang_S2,
    !is.na(lang_S3) & lang_S3 > 0 ~ lang_S3,
    !is.na(lang_S4) & lang_S4 > 0 ~ lang_S4,
    !is.na(lang_S1) ~ lang_S1,
    !is.na(lang_S2) ~ lang_S2,
    !is.na(lang_S3) ~ lang_S3,
    !is.na(lang_S4) ~ lang_S4,
    TRUE ~ -3
  ))

df_cleaned <- df_merged %>% select(NSID, lang)

write_csv(df_cleaned, "data/output/cleaned_data.csv")
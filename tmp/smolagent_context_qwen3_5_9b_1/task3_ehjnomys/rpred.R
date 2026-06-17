library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
W1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
W2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
W3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
W4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Clean missing values for each variable
clean_W1 <- function() {
  W1 %>%
    mutate(
      W1englangYP_clean = case_when(
        is.na(W1englangYP) ~ -3,
        W1englangYP == -92 ~ -9,
        W1englangYP == -91 ~ -1,
        W1englangYP == -99 ~ -3,
        W1englangYP == -94 ~ -2,
        W1englangYP == -999 ~ -2,
        W1englangYP == -998 ~ -2,
        W1englangYP == -997 ~ -2,
        W1englangYP == -995 ~ -2,
        W1englangYP == -1 ~ -8,
        TRUE ~ W1englangYP
      )
    )
}

clean_W2 <- function() {
  W2 %>%
    mutate(
      W2EnglangYP_clean = case_when(
        is.na(W2EnglangYP) ~ -3,
        W2EnglangYP == -92 ~ -9,
        W2EnglangYP == -91 ~ -1,
        W2EnglangYP == -99 ~ -3,
        W2EnglangYP == -94 ~ -2,
        W2EnglangYP == -999 ~ -2,
        W2EnglangYP == -998 ~ -2,
        W2EnglangYP == -997 ~ -2,
        W2EnglangYP == -995 ~ -2,
        W2EnglangYP == -1 ~ -8,
        TRUE ~ W2EnglangYP
      )
    )
}

clean_W3 <- function() {
  W3 %>%
    mutate(
      W3englangHH_clean = case_when(
        is.na(W3englangHH) ~ -3,
        W3englangHH == -92 ~ -9,
        W3englangHH == -91 ~ -1,
        W3englangHH == -99 ~ -3,
        W3englangHH == -94 ~ -2,
        W3englangHH == -999 ~ -2,
        W3englangHH == -997 ~ -2,
        W3englangHH == -1 ~ -8,
        TRUE ~ W3englangHH
      )
    )
}

clean_W4 <- function() {
  W4 %>%
    mutate(
      W4EngLangHH_clean = case_when(
        is.na(W4EngLangHH) ~ -3,
        W4EngLangHH == -92 ~ -9,
        W4EngLangHH == -91 ~ -1,
        W4EngLangHH == -99 ~ -3,
        W4EngLangHH == -94 ~ -2,
        W4EngLangHH == -999 ~ -2,
        W4EngLangHH == -997 ~ -2,
        W4EngLangHH == -1 ~ -8,
        TRUE ~ W4EngLangHH
      )
    )
}

W1_clean <- clean_W1()
W2_clean <- clean_W2()
W3_clean <- clean_W3()
W4_clean <- clean_W4()

# Merge all datasets
combined <- full_join(W1_clean, W2_clean, by = "NSID")
combined <- full_join(combined, W3_clean, by = "NSID")
combined <- full_join(combined, W4_clean, by = "NSID")

# Create the consolidated lang variable
combined <- combined %>%
  mutate(
    lang = case_when(
      !is.na(W1englangYP_clean) & W1englangYP_clean >= 1 & W1englangYP_clean <= 4 ~ W1englangYP_clean,
      !is.na(W2EnglangYP_clean) & W2EnglangYP_clean >= 1 & W2EnglangYP_clean <= 4 ~ W2EnglangYP_clean,
      !is.na(W3englangHH_clean) & W3englangHH_clean >= 1 & W3englangHH_clean <= 4 ~ W3englangHH_clean,
      !is.na(W4EngLangHH_clean) & W4EngLangHH_clean >= 1 & W4EngLangHH_clean <= 4 ~ W4EngLangHH_clean,
      TRUE ~ -3
    )
  )

# Create value labels
combined <- combined %>%
  mutate(
    lang_label = case_when(
      lang == 1 ~ "Yes - English only",
      lang == 2 ~ "Yes - English first/ main and speaks other languages",
      lang == 3 ~ "No, another language is respondent's first or main language",
      lang == 4 ~ "Respondent is bilingual",
      lang == -9 ~ "Refusal",
      lang == -8 ~ "Don't know / insufficient information",
      lang == -3 ~ "Not asked at the fieldwork stage / not interviewed",
      lang == -2 ~ "Schedule not applicable / script error / information lost",
      lang == -1 ~ "Item not applicable",
      TRUE ~ NA_character_
    )
  )

# Write output
write_csv(combined, "data/output/cleaned_data.csv")

cat("Output written successfully. First 5 rows:\n")
print(head(combined, 5))
cat("\nTotal observations:", nrow(combined), "\n")
cat("\nLang variable distribution:\n")
table(combined$lang)
cat("\nLang variable (showing NA as <NA>):\n")
print(unique(combined$lang))

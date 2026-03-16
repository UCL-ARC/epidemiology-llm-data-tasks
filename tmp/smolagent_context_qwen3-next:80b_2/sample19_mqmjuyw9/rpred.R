library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

merged_data <- full_join(wave1, wave4, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

selected <- merged_data %>% select(NSID, W8DBMI, W9DBMI)

clean_bmi <- function(x) {
  x <- ifelse(is.na(x), -3, x)
  x <- ifelse(x < 0 & !x %in% c(-9, -8, -1, -2, -3), -3, x)
  labels <- c(
    "Refusal" = -9,
    "Don't know/insufficient information" = -8,
    "Not applicable" = -1,
    "Script error/lost" = -2,
    "Not asked/interviewed" = -3
  )
  labelled::labelled(x, labels = labels)
}

cleaned_data <- selected %>%
  mutate(bmi25 = clean_bmi(W8DBMI)) %>%
  mutate(bmi32 = clean_bmi(W9DBMI)) %>%
  select(NSID, bmi25, bmi32)

write_csv(cleaned_data, "data/output/cleaned_data.csv")
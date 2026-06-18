library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)
wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Check unique values in regub15 and regov15
test_data <- full_join(wave1, wave2, by = "NSID") %>%
  mutate(
    urbind_final = case_when(
      urbind == 13 ~ -2,
      TRUE ~ as.numeric(urbind)
    ),
    gor_final = case_when(
      gor == 13 ~ -2,
      TRUE ~ as.numeric(gor)
    )
  ) %>%
  mutate(
    regub15 = urbind_final,
    regub16 = urbind_final,
    regov15 = gor_final,
    regov16 = gor_final
  )

cat("regub15 unique values:\n")
print(table(test_data$regub15))

cat("\nregov15 unique values:\n")
print(table(test_data$regov15))
cat("\nregor25 unique values:\n")
print(table(wave8$W8DGOR))
cat("\nregor32 unique values:\n")
print(table(wave9_derived$W9DRGN))
cat("\nregint32 unique values:\n")
print(table(wave9_main$W9NATIONRES))
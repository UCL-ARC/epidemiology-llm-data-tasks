library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = readr::cols(.default = "c"))
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols(.default = "c"))

cohort <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

harmonize_bmi <- function(x) {
  val <- as.numeric(x)
  res <- rep(NA, length(val))
  
  is_valid <- !is.na(val) & val > 0
  is_refusal <- !is.na(val) & val == -9
  is_dk <- !is.na(val) & val == -8
  is_na_code <- !is.na(val) & val == -1
  
  res[is_valid] <- val[is_valid]
  res[is_refusal] <- -9
  res[is_dk] <- -8
  res[is_na_code] <- -1
  res[is.na(res)] <- -3
  
  return(res)
}

cleaned_data <- cohort %>%
  mutate(
    bmi25 = harmonize_bmi(W8DBMI),
    bmi32 = harmonize_bmi(W9DBMI)
  ) %>%
  select(NSID, bmi25, bmi32)

# 5. Variable Type and Labels
# To avoid the casting error with labelled::labelled,
# we define a named vector where names are labels and values are the numeric codes.
# This is the expected format for haven-style labelled vectors.
correct_labels <- c(
  "Refusal" = -9,
  "Don't know/insufficient information" = -8,
  "Item not applicable" = -1,
  "Not asked/interviewed" = -3,
  "Script error/lost" = -2
)

cleaned_data$bmi25 <- labelled::labelled(cleaned_data$bmi25, labels = correct_labels)
cleaned_data$bmi32 <- labelled::labelled(cleaned_data$bmi32, labels = correct_labels)

# 8. Output Requirements
write_csv(cleaned_data, "data/output/cleaned_data.csv")
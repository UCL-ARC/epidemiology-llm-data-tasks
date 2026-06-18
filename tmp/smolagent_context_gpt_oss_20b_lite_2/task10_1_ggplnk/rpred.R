library(readr)
library(dplyr)
library(tidyr)
library(labelled)
# Helper to standardise missing codes and convert NA to -3
standardise_missing <- function(x, miss_map) {
  for (k in names(miss_map)) {
    x[x == as.numeric(k)] <- miss_map[[k]]
  }
  x[is.na(x)] <- -3
  return(x)
}
# Read files
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim="\t", col_types = cols())
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim="\t", col_types = cols())
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim="\t", col_types = cols())
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim="\t", col_types = cols())
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim="\t", col_types = cols())
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim="\t", col_types = cols())
# Standardise missing codes per variable
miss_map_w4 <- c("-999" = -2, "-94" = -8, "-92" = -9, "-91" = -1)
miss_map_w5 <- c("-999" = -2, "-94" = -8)
miss_map_w6 <- c("-999" = -2, "-91" = -1)
miss_map_w7 <- c("-999" = -2, "-91" = -1)
miss_map_w8 <- c("-9" = -9, "-8" = -8, "-1" = -1)
miss_map_w9 <- c("-9" = -9, "-8" = -8, "-1" = -1)
wave4 <- wave4 %>% mutate(W4empsYP = standardise_missing(as.numeric(W4empsYP), miss_map_w4))
wave5 <- wave5 %>% mutate(W5mainactYP = standardise_missing(as.numeric(W5mainactYP), miss_map_w5))
wave6 <- wave6 %>% mutate(W6TCurrentAct = standardise_missing(as.numeric(W6TCurrentAct), miss_map_w6))
wave7 <- wave7 %>% mutate(W7TCurrentAct = standardise_missing(as.numeric(W7TCurrentAct), miss_map_w7))
wave8 <- wave8 %>% mutate(W8DACTIVITYC = standardise_missing(as.numeric(W8DACTIVITYC), miss_map_w8))
wave9 <- wave9 %>% mutate(W9DACTIVITYC = standardise_missing(as.numeric(W9DACTIVITYC), miss_map_w9))
# Merge all datasets by NSID
all_data <- full_join(wave4, wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")
# Collapse functions
collapse_w4 <- function(x) {
  case_when(
    x %in% c(1,2) ~ 1,
    x == 3 ~ 3,
    x == 4 ~ 5,
    x == 5 ~ 4,
    TRUE ~ 6
  )
}
collapse_w5 <- function(x) {
  case_when(
    x == 3 ~ 1,
    x == 4 ~ 4,
    x %in% c(1,2,5,6,10,11,9) ~ 5,
    x == 7 ~ 3,
    TRUE ~ 6
  )
}
collapse_w6 <- function(x) {
  case_when(
    x == 3 ~ 1,
    x %in% c(1,2) ~ 4,
    x %in% c(4,5,10,11) ~ 5,
    x == 8 ~ 3,
    TRUE ~ 6
  )
}
collapse_w7 <- function(x) {
  case_when(
    x == 3 ~ 1,
    x %in% c(1,2) ~ 4,
    x %in% c(4,5,9) ~ 5,
    x == 8 ~ 3,
    TRUE ~ 6
  )
}
# Detailed collapse for age 25 and 32: keep numeric
collapse_w8detailed <- function(x) { as.numeric(x) }
# Apply collapsed variables
all_data <- all_data %>%
  mutate(ecoact17 = collapse_w4(W4empsYP),
         ecoact18 = collapse_w5(W5mainactYP),
         ecoact19 = collapse_w6(W6TCurrentAct),
         ecoact20 = collapse_w7(W7TCurrentAct),
         ecoact25 = collapse_w8detailed(W8DACTIVITYC),
         ecoact32 = collapse_w8detailed(W9DACTIVITYC),
         ecoactadu25 = W8DACTIVITYC,
         ecoactadu32 = W9DACTIVITYC)
# Collapse ecoact25 and ecoact32 to 6 categories
collapse_w8 <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 4 ~ 3,
    x == 5 ~ 4,
    x %in% c(6,7) ~ 5,
    TRUE ~ 6
  )
}
all_data <- all_data %>%
  mutate(ecoact25 = collapse_w8(ecoact25),
         ecoact32 = collapse_w8(ecoact32))
# Define labels for collapsed categories
labels_collapsed <- c("1"="Paid work", "2"="Self-employed", "3"="Unemployed/looking", "4"="Education", "5"="Training/apprenticeship", "6"="Other")
# Apply factor labels to collapsed variables
all_data <- all_data %>%
  mutate(across(c(ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32), ~factor(., levels = 1:6, labels = labels_collapsed)))
# Final output variables
final_vars <- all_data %>% select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)
write_csv(final_vars, "data/output/cleaned_data.csv")

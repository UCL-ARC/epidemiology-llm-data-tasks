library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
load_tab_fixed <- function(fname) {
  df <- readr::read_delim(paste0('data/input/', fname), delim = "\t", col_types = readr::cols(NSID = readr::col_character(), .default = "double"), guess_max = 10000)
  return(df)
}

wave1 <- load_tab_fixed('wave_one_lsype_young_person_2020.tab')
wave4 <- load_tab_fixed('wave_four_lsype_young_person_2020.tab')
wave5 <- load_tab_fixed('wave_five_lsype_young_person_2020.tab')
wave6 <- load_tab_fixed('wave_six_lsype_young_person_2020.tab')
wave7 <- load_tab_fixed('wave_seven_lsype_young_person_2020.tab')
wave8 <- load_tab_fixed('ns8_2015_derived.tab')
wave9 <- load_tab_fixed('ns9_2022_derived_variables.tab')

full_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

map_missing <- function(val, labels) {
  if (is.na(val)) return(-3)
  lbl <- labels[as.character(val)]
  if (is.na(lbl)) return(-3)
  if (grepl("Refused", lbl, ignore.case = TRUE)) return(-9)
  if (grepl("Insufficient information", lbl, ignore.case = TRUE)) return(-8)
  if (grepl("Prefer not to say", lbl, ignore.case = TRUE)) return(-7)
  if (grepl("Not asked", lbl, ignore.case = TRUE)) return(-3)
  if (grepl("lost|script error|schedule not applicable", lbl, ignore.case = TRUE)) return(-2)
  if (grepl("Not applicable", lbl, ignore.case = TRUE)) return(-1)
  return(val)
}

map_w4 <- function(x) {
  if (is.na(x)) return(-3)
  if (x == 1.0 || x == 2.0) return(1)
  if (x == 4.0) return(2)
  if (x == 5.0) return(3)
  if (x == 3.0) return(4)
  if (x == 6.0) return(5)
  if (x == 7.0 || x == 8.0 || x == 9.0) return(6)
  lbls <- c("-999.0" = "Missing household information - lost", "-94.0" = "Insufficient information", "-92.0" = "Refused", "-91.0" = "Not applicable - still at school")
  map_missing(x, lbls)
}

map_w5 <- function(x) {
  if (is.na(x)) return(-3)
  if (x == 3.0) return(1)
  if (x == 1.0 || x == 5.0 || x == 6.0) return(2)
  if (x == 4.0) return(3)
  if (x == 7.0) return(4)
  if (x == 8.0) return(5)
  if (x == 2.0 || x == 9.0 || x == 10.0 || x == 11.0) return(6)
  lbls <- c("-94.0" = "Insufficient information")
  map_missing(x, lbls)
}

map_w6 <- function(x) {
  if (is.na(x)) return(-3)
  if (x == 3.0) return(1)
  if (x == 4.0 || x == 5.0) return(2)
  if (x == 1.0 || x == 2.0) return(3)
  if (x == 8.0) return(4)
  if (x == 7.0) return(5)
  if (x == 6.0 || x == 9.0 || x == 10.0 || x == 11.0) return(6)
  lbls <- c("-91.0" = "Unable to classify")
  map_missing(x, lbls)
}

map_w7 <- function(x) {
  if (is.na(x)) return(-3)
  if (x == 3.0) return(1)
  if (x == 4.0 || x == 5.0 || x == 11.0) return(2)
  if (x == 1.0 || x == 2.0) return(3)
  if (x == 8.0) return(4)
  if (x == 7.0) return(5)
  if (x == 6.0 || x == 9.0 || x == 10.0 || x == 12.0 || x == 13.0 || x == 14.0 || x == 15.0) return(6)
  lbls <- c("-91.0" = "Not applicable")
  map_missing(x, lbls)
}

map_w8 <- function(x) {
  if (is.na(x)) return(-3)
  if (x == 1.0 || x == 2.0) return(1)
  if (x == 6.0 || x == 7.0) return(2)
  if (x == 5.0) return(3)
  if (x == 4.0) return(4)
  if (x == 9.0) return(5)
  if (x == 3.0 || x == 8.0 || x == 10.0) return(6)
  lbls <- c("-9.0" = "Refused", "-8.0" = "Insufficient information", "-1.0" = "Not applicable")
  map_missing(x, lbls)
}

map_w9 <- function(x) {
  if (is.na(x)) return(-3)
  if (x == 1.0 || x == 2.0) return(1)
  if (x == 6.0 || x == 7.0) return(2)
  if (x == 5.0) return(3)
  if (x == 4.0) return(4)
  if (x == 9.0) return(5)
  if (x == 3.0 || x == 8.0 || x == 10.0) return(6)
  lbls <- c("-9.0" = "Refused", "-8.0" = "Insufficient information", "-1.0" = "Not applicable")
  map_missing(x, lbls)
}

map_detailed <- function(x) {
  if (is.na(x)) return(-3)
  if (x >= 1 && x <= 10) return(x)
  lbls <- c("-9.0" = "Refused", "-8.0" = "Insufficient information", "-1.0" = "Not applicable")
  map_missing(x, lbls)
}

final_df <- full_data %>%
  mutate(
    ecoact17 = sapply(W4empsYP, map_w4),
    ecoact18 = sapply(W5mainactYP, map_w5),
    ecoact19 = sapply(W6TCurrentAct, map_w6),
    ecoact20 = sapply(W7TCurrentAct, map_w7),
    ecoact25 = sapply(W8DACTIVITYC, map_w8),
    ecoact32 = sapply(W9DACTIVITYC, map_w9),
    ecoactadu25 = sapply(W8DACTIVITYC, map_detailed),
    ecoactadu32 = sapply(W9DACTIVITYC, map_detailed)
  )

collapsed_labels <- c(
  "1" = "In paid work", "2" = "Apprenticeship / government training scheme / training", "3" = "Education", "4" = "Unemployed", "5" = "Looking after home / family", "6" = "Other",
  "-9" = "Refusal", "-8" = "Don't know / insufficient information", "-7" = "Prefer not to say", "-3" = "Not asked at the fieldwork stage / not interviewed", "-2" = "Schedule not applicable / script error / information lost", "-1" = "Item not applicable"
)

detailed_labels <- c(
  "1" = "Employee - in paid work", "2" = "Self employed", "3" = "In unpaid/voluntary work", "4" = "Unemployed", "5" = "Education: School/college/university", "6" = "Apprenticeship", "7" = "On gov't scheme for employment training", "8" = "Sick or disabled", "9" = "Looking after home or family", "10" = "Something else",
  "-9" = "Refusal", "-8" = "Don't know / insufficient information", "-7" = "Prefer not to say", "-3" = "Not asked at the fieldwork stage / not interviewed", "-2" = "Schedule not applicable / script error / information lost", "-1" = "Item not applicable"
)

apply_labels <- function(vec, label_map) {
  levels_order <- c(names(label_map)[!grepl("^-", names(label_map))], names(label_map)[grepl("^-", names(label_map))])
  factor(vec, levels = as.numeric(levels_order), labels = label_map[levels_order])
}

final_df <- final_df %>%
  mutate(across(c(ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32), ~apply_labels(.x, collapsed_labels))) %>%
  mutate(across(c(ecoactadu25, ecoactadu32), ~apply_labels(.x, detailed_labels)))

final_output <- final_df %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

readr::write_csv(final_output, "data/output/cleaned_data.csv")
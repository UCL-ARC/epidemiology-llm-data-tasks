library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Helper function to map missing values to the standard codes
map_missing <- function(x, codes) {
  out <- x
  for (orig in names(codes)) {
    out[as.numeric(x) == as.numeric(orig)] <- as.numeric(codes[orig])
  }
  out
}

# File paths
file_paths <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_five = "data/input/wave_five_lsype_young_person_2020.tab",
  wave_six  = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "data/input/wave_seven_lsype_young_person_2020.tab",
  ns8 = "data/input/ns8_2015_derived.tab",
  ns9 = "data/input/ns9_2022_derived_variables.tab"
)

# Read data frames
read_tab <- function(path) {
  read_delim(path, delim = "\t", col_types = cols(), show_col_types = FALSE)
}

wave_one_df   <- read_tab(file_paths$wave_one)
wave_four_df  <- read_tab(file_paths$wave_four)
wave_five_df  <- read_tab(file_paths$wave_five)
wave_six_df   <- read_tab(file_paths$wave_six)
wave_seven_df <- read_tab(file_paths$wave_seven)
ns8_df        <- read_tab(file_paths$ns8)
ns9_df        <- read_tab(file_paths$ns9)

# Merge all by NSID
merged_df <- reduce(
  list(wave_one_df, wave_four_df, wave_five_df, wave_six_df,
       wave_seven_df, ns8_df, ns9_df),
  full_join, by = "NSID"
)

# ---------------- Collapsing functions ------------------------------------
collaps_w4 <- function(x) {
  x_clean <- map_missing(x, c("-999" = "-2", "-94" = "-8", "-92" = "-9", "-91" = "-1"))
  case_when(
    x_clean %in% c(1,2)  ~ 1,
    x_clean == 3        ~ 2,
    x_clean == 5        ~ 3,
    x_clean == 4        ~ 4,
    x_clean == 6        ~ 5,
    x_clean %in% c(7,8,9) ~ 6,
    TRUE                 ~ x_clean
  )
}

collaps_w5 <- function(x) {
  x_clean <- map_missing(x, c("-94" = "-8", "-1" = "-1"))
  case_when(
    x_clean %in% c(3,2)     ~ 1,
    x_clean == 7            ~ 2,
    x_clean == 4            ~ 3,
    x_clean %in% c(1,5,6)   ~ 4,
    x_clean == 8            ~ 5,
    x_clean %in% c(9,10,11) ~ 6,
    TRUE                    ~ x_clean
  )
}

collaps_w6 <- function(x) {
  x_clean <- map_missing(x, c("-999" = "-2", "-94" = "-8", "-92" = "-9", "-91" = "-1"))
  case_when(
    x_clean %in% c(3,10)    ~ 1,
    x_clean == 8            ~ 2,
    x_clean %in% c(1,2)    ~ 3,
    x_clean %in% c(4,5)    ~ 4,
    x_clean == 7            ~ 5,
    x_clean %in% c(6,9,11) ~ 6,
    TRUE                    ~ x_clean
  )
}

collaps_w7 <- function(x) {
  x_clean <- map_missing(x, c("-999" = "-2", "-94" = "-8", "-92" = "-9", "-91" = "-1"))
  case_when(
    x_clean %in% c(3,9)      ~ 1,
    x_clean == 8            ~ 2,
    x_clean %in% c(1,2)      ~ 3,
    x_clean %in% c(4,5)      ~ 4,
    x_clean == 7            ~ 5,
    x_clean %in% c(6,10,11,12,13,14,15) ~ 6,
    TRUE                    ~ x_clean
  )
}

collaps_w8 <- function(x) {
  x_clean <- map_missing(x, c("-9" = "-9", "-8" = "-8", "-1" = "-1"))
  case_when(
    x_clean %in% c(1,2)  ~ 1,
    x_clean == 4        ~ 2,
    x_clean == 5        ~ 3,
    x_clean %in% c(6,7) ~ 4,
    x_clean == 9        ~ 5,
    x_clean %in% c(3,8,10) ~ 6,
    TRUE                ~ x_clean
  )
}

collaps_w9 <- function(x) {
  x_clean <- map_missing(x, c("-9" = "-9", "-8" = "-8", "-1" = "-1"))
  case_when(
    x_clean %in% c(1,2)  ~ 1,
    x_clean == 4        ~ 2,
    x_clean == 5        ~ 3,
    x_clean %in% c(6,7) ~ 4,
    x_clean == 9        ~ 5,
    x_clean %in% c(3,8,10) ~ 6,
    TRUE                ~ x_clean
  )
}

# ---------------- Apply collapsing and create detailed variables ----------------
merged_df <- merged_df %>%
  mutate(
    ecoact17  = collaps_w4(W4empsYP),
    ecoact18  = collaps_w5(W5mainactYP),
    ecoact19  = collaps_w6(W6TCurrentAct),
    ecoact20  = collaps_w7(W7TCurrentAct),
    ecoact25  = collaps_w8(W8DACTIVITYC),
    ecoact32  = collaps_w9(W9DACTIVITYC),
    ecoactadu25 = map_missing(W8DACTIVITYC, c("-9" = "-9", "-8" = "-8", "-1" = "-1")),
    ecoactadu32 = map_missing(W9DACTIVITYC, c("-9" = "-9", "-8" = "-8", "-1" = "-1"))
  )

# ---------------- Labels -----------------------------------------
labels_collapsed <- c(
  "Paid work" = 1,
  "Unemployed/looking" = 2,
  "Education" = 3,
  "Training" = 4,
  "Family/voluntary" = 5,
  "Other" = 6,
  "Not applicable" = -1,
  "Schedule not applicable" = -2,
  "Not asked at fieldwork stage" = -3,
  "Insufficient information" = -8,
  "Refusal" = -9
)

labels_w8 <- c(
  "Refused" = -9,
  "Insufficient information" = -8,
  "Not applicable" = -1,
  "Employee - in paid work" = 1,
  "Self employed" = 2,
  "In unpaid/voluntary work" = 3,
  "Unemployed" = 4,
  "Education: School/college/university" = 5,
  "Apprenticeship" = 6,
  "On gov't scheme for employment training" = 7,
  "Sick or disabled" = 8,
  "Looking after home or family" = 9,
  "Something else" = 10
)

# Apply labels
merged_df <- merged_df %>%
  mutate(
    ecoact17   = labelled(ecoact17,   labels_collapsed),
    ecoact18   = labelled(ecoact18,   labels_collapsed),
    ecoact19   = labelled(ecoact19,   labels_collapsed),
    ecoact20   = labelled(ecoact20,   labels_collapsed),
    ecoact25   = labelled(ecoact25,   labels_collapsed),
    ecoact32   = labelled(ecoact32,   labels_collapsed),
    ecoactadu25 = labelled(ecoactadu25, labels_w8),
    ecoactadu32 = labelled(ecoactadu32, labels_w8)
  )

# ---------------- Final dataset -----------------------------------------
final_df <- merged_df %>%
  select(NSID,
         ecoact17, ecoact18, ecoact19, ecoact20,
         ecoact25, ecoact32,
         ecoactadu25, ecoactadu32)

# ---------------- Write output -----------------------------------------
write_csv(final_df, "data/output/cleaned_data.csv")
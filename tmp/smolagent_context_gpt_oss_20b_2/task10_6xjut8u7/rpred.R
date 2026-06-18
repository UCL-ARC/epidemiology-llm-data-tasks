library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# Helper: replace missing codes
replace_missing <- function(x, mapping){
  for(code in names(mapping)){
    x <- ifelse(x == as.numeric(code), mapping[code], x)
  }
  x
}

# Common missing map
common_map <- c(`-999`=-2, `-94`=-8, `-92`=-9, `-91`=-1, `-8`=-8, `-9`=-9, `-1`=-1)

# Load files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim="\t", col_types = cols(), na = c(""))
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim="\t", col_types = cols(), na = c(""))
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim="\t", col_types = cols(), na = c(""))
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim="\t", col_types = cols(), na = c(""))
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim="\t", col_types = cols(), na = c(""))
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim="\t", col_types = cols(), na = c(""))
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim="\t", col_types = cols(), na = c(""))

# Replace missing codes
wave4$W4empsYP <- replace_missing(wave4$W4empsYP, common_map)
wave5$W5mainactYP <- replace_missing(wave5$W5mainactYP, common_map)
wave6$W6TCurrentAct <- replace_missing(wave6$W6TCurrentAct, common_map)
wave7$W7TCurrentAct <- replace_missing(wave7$W7TCurrentAct, common_map)
wave8$W8DACTIVITYC <- replace_missing(wave8$W8DACTIVITYC, c(`-9`=-9, `-8`=-8, `-1`=-1))
wave9$W9DACTIVITYC <- replace_missing(wave9$W9DACTIVITYC, c(`-9`=-9, `-8`=-8, `-1`=-1))

# Merge by NSID
full_df <- wave1 %>%
  full_join(wave4, by="NSID") %>%
  full_join(wave5, by="NSID") %>%
  full_join(wave6, by="NSID") %>%
  full_join(wave7, by="NSID") %>%
  full_join(wave8, by="NSID") %>%
  full_join(wave9, by="NSID")

# Mapping functions for collapsed 6 categories
map_ecoact17 <- function(x){
  case_when(
    x %in% c(1,2) ~ 1,
    x %in% c(4) ~ 2,
    x %in% c(5) ~ 3,
    x %in% c(3) ~ 4,
    x %in% c(6) ~ 5,
    x %in% c(7,8,9) ~ 6,
    TRUE ~ NA_real_
  )
}
map_ecoact18 <- function(x){
  case_when(
    x %in% c(2,3) ~ 1,
    x %in% c(1) ~ 2,
    x %in% c(5,6) ~ 2,
    x %in% c(4) ~ 3,
    x %in% c(7) ~ 4,
    x %in% c(8,9,10,11) ~ 5,
    TRUE ~ NA_real_
  )
}
map_ecoact19 <- function(x){
  case_when(
    x %in% c(3) ~ 1,
    x %in% c(4,5,10) ~ 2,
    x %in% c(1,2) ~ 3,
    x %in% c(8) ~ 4,
    x %in% c(6,7,9) ~ 5,
    x %in% c(11) ~ 6,
    TRUE ~ NA_real_
  )
}
map_ecoact20 <- function(x){
  case_when(
    x %in% c(3,9) ~ 1,
    x %in% c(4,5,11) ~ 2,
    x %in% c(1,2) ~ 3,
    x %in% c(8) ~ 4,
    x %in% c(6,7,13) ~ 5,
    x %in% c(10,12,14) ~ 6,
    TRUE ~ NA_real_
  )
}
map_ecoact25 <- function(x){
  case_when(
    x %in% c(1,2) ~ 1,
    x %in% c(6,7) ~ 2,
    x %in% c(5) ~ 3,
    x %in% c(4) ~ 4,
    x %in% c(9) ~ 5,
    x %in% c(3,8,10) ~ 6,
    TRUE ~ NA_real_
  )
}
map_ecoact32 <- map_ecoact25

# Create collapsed variables
full_df <- full_df %>%
  mutate(
    ecoact17 = map_ecoact17(W4empsYP),
    ecoact18 = map_ecoact18(W5mainactYP),
    ecoact19 = map_ecoact19(W6TCurrentAct),
    ecoact20 = map_ecoact20(W7TCurrentAct),
    ecoact25 = map_ecoact25(W8DACTIVITYC),
    ecoact32 = map_ecoact32(W9DACTIVITYC)
  )

# Replace NA with -3 for collapsed variables
full_df <- full_df %>% mutate(across(c(ecoact17:ecoact32), ~ ifelse(is.na(.), -3, .)))

# Detailed variables (direct from source)
full_df <- full_df %>%
  mutate(
    ecoactadu25 = W8DACTIVITYC,
    ecoactadu32 = W9DACTIVITYC
  ) %>%
  mutate(across(c(ecoactadu25, ecoactadu32), ~ ifelse(is.na(.), -3, .)))

# Convert detailed variables to factor with labels for readability
label_vec <- c("Employee - in paid work", "Self employed", "In unpaid/voluntary work", "Unemployed", "Education: School/college/university", "Apprenticeship", "On gov' t scheme for employment training", "Sick or disabled", "Looking after home or family", "Something else")
full_df <- full_df %>%
  mutate(
    ecoactadu25 = factor(ecoactadu25, levels = 1:10, labels = label_vec, exclude = -3),
    ecoactadu32 = factor(ecoactadu32, levels = 1:10, labels = label_vec, exclude = -3)
  )

# Final selection
final_df <- full_df %>% select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Write CSV
write_csv(final_df, "data/output/cleaned_data.csv", na = "")

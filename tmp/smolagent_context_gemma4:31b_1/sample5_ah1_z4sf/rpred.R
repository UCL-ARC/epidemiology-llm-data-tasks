library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# To avoid memory issues, we will read only the necessary columns from each file
# and handle the joins more carefully. 

# Wave 6: NSID and W6MarStatYP
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", col_select = c("NSID", "W6MarStatYP"), col_types = cols(NSID = "c", W6MarStatYP = "d"))
# Wave 8: NSID and W8DMARSTAT
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_select = c("NSID", "W8DMARSTAT"), col_types = cols(NSID = "c", W8DMARSTAT = "d"))
# Wave 9: NSID and W9DMARSTAT
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_select = c("NSID", "W9DMARSTAT"), col_types = cols(NSID = "c", W9DMARSTAT = "d"))
# Wave 1: NSID only (to keep the full cohort list)
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_select = c("NSID"), col_types = cols(NSID = "c"))
# Wave 4: NSID only
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_select = c("NSID"), col_types = cols(NSID = "c"))

# Ensure NSIDs are unique per file to prevent many-to-many join explosion
wave1 <- wave1 %>% distinct(NSID, .keep_all = TRUE)
wave4 <- wave4 %>% distinct(NSID, .keep_all = TRUE)
wave6 <- wave6 %>% distinct(NSID, .keep_all = TRUE)
wave8 <- wave8 %>% distinct(NSID, .keep_all = TRUE)
wave9 <- wave9 %>% distinct(NSID, .keep_all = TRUE)

# Merge datasets
full_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Processing functions as defined previously
process_w6_mar <- function(x) {
  res <- x
  res[x == -997] <- -2
  res[x == -97] <- -7
  res[x == -92] <- -9
  res[x == -91] <- -1
  res[x == -1] <- -8
  res[is.na(res)] <- -3
  return(res)
}

process_w8_mar <- function(x) {
  res <- x
  res[x == -9] <- -9
  res[x == -8] <- -8
  res[x == -1] <- -1
  res[is.na(res)] <- -3
  return(res)
}

collapse_w8_mar <- function(x) {
  res <- x
  res[x == 1 | x == 6] <- 1
  res[x == 3 | x == 7] <- 3
  res[x == 4 | x == 8] <- 4
  res[x == 5 | x == 9] <- 5
  return(res)
}

process_w9_mar <- function(x) {
  res <- x
  res[x == -9] <- -9
  res[x == -8] <- -8
  res[is.na(res)] <- -3
  return(res)
}

collapse_w9_mar <- function(x) {
  res <- x
  res[x == 1 | x == 6] <- 1
  res[x == 4] <- 3
  res[x == 3 | x == 7] <- 4
  res[x == 5 | x == 8] <- 5
  return(res)
}

# Apply transformations
full_data <- full_data %>%
  mutate(
    partnr19 = process_w6_mar(W6MarStatYP),
    partnr19_coll = process_w6_mar(W6MarStatYP),
    partnradu25 = process_w8_mar(W8DMARSTAT),
    partnr25_coll = collapse_w8_mar(process_w8_mar(W8DMARSTAT)),
    partnradu32 = process_w9_mar(W9DMARSTAT),
    partnr32_coll = collapse_w9_mar(process_w9_mar(W9DMARSTAT))
  )

# Select derived variables
final_output <- full_data %>%
  select(NSID, partnr19, partnr19_coll, partnr25_coll, partnradu25, partnr32_coll, partnradu32)

write_csv(final_output, "data/output/cleaned_data.csv")
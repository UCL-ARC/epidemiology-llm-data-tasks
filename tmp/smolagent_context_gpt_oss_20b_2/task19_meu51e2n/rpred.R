library(readr)
library(dplyr)

map_missing <- function(x){
  res <- x
  res[is.na(res)] <- -3
  res[res < 0 & !(res %in% c(-9,-8,-1))] <- -3
  return(res)
}

base_path <- "data/input/"

wave1 <- read_delim(file.path(base_path, "wave_one_lsype_young_person_2020.tab"),
                    delim = "\t", col_types = cols(.default = col_guess()), show_col_types = FALSE) %>%
  mutate(NSID = as.character(NSID))

wave4 <- read_delim(file.path(base_path, "wave_four_lsype_young_person_2020.tab"),
                    delim = "\t", col_types = cols(.default = col_guess()), show_col_types = FALSE) %>%
  mutate(NSID = as.character(NSID))

ns8 <- read_delim(file.path(base_path, "ns8_2015_derived.tab"),
                  delim = "\t", col_types = cols(.default = col_guess()), show_col_types = FALSE) %>%
  mutate(NSID = as.character(NSID))

ns9 <- read_delim(file.path(base_path, "ns9_2022_derived_variables.tab"),
                  delim = "\t", col_types = cols(.default = col_guess()), show_col_types = FALSE) %>%
  mutate(NSID = as.character(NSID))

merged <- full_join(wave1, wave4, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

merged <- merged %>%
  mutate(
    bmi25 = map_missing(W8DBMI),
    bmi32 = map_missing(W9DBMI)
  )

final_data <- merged %>%
  select(NSID, bmi25, bmi32)

write_csv(final_data, "data/output/cleaned_data.csv")
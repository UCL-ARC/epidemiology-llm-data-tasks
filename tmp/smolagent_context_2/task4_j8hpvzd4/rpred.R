library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- list(
  wave_one = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  wave_six = 'data/input/wave_six_lsype_young_person_2020.tab',
  wave_seven = 'data/input/wave_seven_lsype_young_person_2020.tab',
  wave_eight = 'data/input/ns8_2015_self_completion.tab',
  wave_nine = 'data/input/ns9_2022_main_interview.tab'
)

load_data <- function(path) {
  read_delim(path, delim = "\t", col_types = cols(.default = "c"))
}

data_list <- map(files, load_data)

# Merge datasets using full_join by NSID to preserve the full cohort frame
all_ids <- map(data_list, ~ .x$NSID) %>% flatten_chr() %>% unique()
cohort_frame <- data.frame(NSID = all_ids, stringsAsFactors = FALSE)

cohort_frame <- cohort_frame %>%
  full_join(data_list[[3]] %>% select(NSID, W6SexualityYP), by = "NSID") %>%
  full_join(data_list[[4]] %>% select(NSID, W7SexualityYP), by = "NSID") %>%
  full_join(data_list[[5]] %>% select(NSID, W8SEXUALITY), by = "NSID") %>%
  full_join(data_list[[6]] %>% select(NSID, W9SORI), by = "NSID")

# Helper function for missing value mapping
map_missing <- function(val, wave) {
  if (is.na(val)) return(-3)
  val <- as.numeric(val)
  
  if (wave == "W6") {
    if (val == -97) return(-9)
    if (val == -92) return(-9)
    if (val == -91) return(-1)
    if (val == -1) return(-8)
  } else if (wave == "W7") {
    if (val == -100) return(-9)
    if (val == -97) return(-9)
    if (val == -92) return(-9)
    if (val == -91) return(-1)
    if (val == -1) return(-8)
  } else if (wave == "W8") {
    if (val == -9) return(-9)
    if (val == -8) return(-8)
    if (val == -1) return(-1)
  } else if (wave == "W9") {
    if (val == -9) return(-9)
    if (val == -8) return(-8)
    if (val == -3) return(-3)
    if (val == -1) return(-1)
    if (val == 5) return(-7)
  }
  
  if (val < 0) return(-2)
  return(val)
}

# Process variables
cohort_frame <- cohort_frame %>%
  mutate(
    sexuality19 = map_dbl(W6SexualityYP, ~map_missing(.x, "W6")),
    sexuality20 = map_dbl(W7SexualityYP, ~map_missing(.x, "W7")),
    sexuality_w8 = map_dbl(W8SEXUALITY, ~map_missing(.x, "W8")),
    sexuality32 = map_dbl(W9SORI, ~map_missing(.x, "W9"))
  )

final_df <- cohort_frame %>%
  select(NSID, sexuality19, sexuality20, sexuality_w8, sexuality32)

# In haven/labelled, value labels must be a named vector where names are the labels
# and the vector values are the numeric codes.
val_labs_vec <- c(
  "Heterosexual / Straight" = 1,
  "Gay / Lesbian" = 2,
  "Bisexual" = 3,
  "Other" = 4,
  "Refusal" = -9,
  "Don't know / insufficient information" = -8,
  "Prefer not to say" = -7,
  "Not asked at the fieldwork stage / not interviewed" = -3,
  "Schedule not applicable / script error / information lost" = -2,
  "Item not applicable" = -1
)

vars_to_label <- c("sexuality19", "sexuality20", "sexuality_w8", "sexuality32")

for (var in vars_to_label) {
  final_df[[var]] <- set_value_labels(final_df[[var]], val_labs_vec)
}

write_csv(final_df, "data/output/cleaned_data.csv")
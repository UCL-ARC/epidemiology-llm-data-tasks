library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Helper: read wave file and keep NSID plus variable of interest
read_wave_var <- function(file, var) {
  df <- read_delim(file.path("data/input", file),
                    delim = "\t",
                    locale = locale(encoding = "UTF-8"))
  df %>% select(NSID, !!sym(var))
}

# Waves information: file, variable name, age
waves <- list(
  list(file = "wave_one_lsype_young_person_2020.tab",  var = "W1alceverYP",  age = 14),
  list(file = "wave_two_lsype_young_person_2020.tab",  var = "W2alceverYP",  age = 15),
  list(file = "wave_three_lsype_young_person_2020.tab", var = "W3alceverYP",  age = 16),
  list(file = "wave_four_lsype_young_person_2020.tab",  var = "W4AlcEverYP",  age = 17),
  list(file = "wave_six_lsype_young_person_2020.tab",  var = "W6AlcEverYP",  age = 19),
  list(file = "wave_seven_lsype_young_person_2020.tab", var = "W7AlcEverYP",  age = 20),
  list(file = "ns8_2015_self_completion.tab",        var = "W8AUDIT1",   age = 25),
  list(file = "ns9_2022_main_interview.tab",         var = "W9AUDIT1",   age = 32)
)

# Read all waves and rename variable to a common prefix with age
list_dfs <- lapply(waves, function(w) {
  df <- read_wave_var(w$file, w$var)
  new_name <- paste0("ever", w$age)
  df %>% rename(!!new_name := !!sym(w$var))
})

# Merge all by NSID
merged_df <- reduce(list_dfs, full_join, by = "NSID")

# Function to convert responses: 1=yes, 0=no, NA for missing
convert_ever <- function(x, age) {
  if (age %in% c(25, 32)) {
    # W8/W9 AUDIT1: 1=Never, 2-5 = some consumption
    case_when(
      x > 1 ~ 1,
      x == 1 ~ 0,
      TRUE ~ NA_real_
    )
  } else {
    # other waves: 1=Yes, 2=No
    case_when(
      x == 1 ~ 1,
      x == 2 ~ 0,
      TRUE ~ NA_real_
    )
  }
}

# Apply conversion to all ever* columns
converted_df <- merged_df %>%
  mutate(across(starts_with("ever"), ~ convert_ever(.x, as.numeric(sub("ever", "", cur_column())))))

# Determine earliest age of consumption
final_df <- converted_df %>%
  rowwise() %>%
  mutate(
    alcfst = case_when(
      !is.na(ever14) & ever14 == 1 ~ 14,
      !is.na(ever15) & ever15 == 1 ~ 15,
      !is.na(ever16) & ever16 == 1 ~ 16,
      !is.na(ever17) & ever17 == 1 ~ 17,
      !is.na(ever19) & ever19 == 1 ~ 19,
      !is.na(ever20) & ever20 == 1 ~ 20,
      !is.na(ever25) & ever25 == 1 ~ 25,
      !is.na(ever32) & ever32 == 1 ~ 32,
      TRUE ~ 99
    )
  ) %>%
  ungroup() %>%
  select(NSID, alcfst)

# Ensure output directory exists
if (!dir.exists("data/output")) dir.create("data/output", recursive = TRUE)

# Write cleaned data
write_csv(final_df, "data/output/cleaned_data.csv")

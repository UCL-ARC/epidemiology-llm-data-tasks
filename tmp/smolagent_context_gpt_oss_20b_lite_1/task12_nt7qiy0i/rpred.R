library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Missing value mapping
standard_missing_code <- function(x) {
  if (is.na(x)) return(NA_real_)
  if (x >= 0) return(x)
  switch(as.character(x),
         "-9" = -9,
         "-8" = -8,
         "-7" = -7,
         "-6" = -8,
         "-5" = -8,
         "-4" = -8,
         "-3" = -3,
         "-2" = -2,
         "-1" = -1,
         "-999" = -9,
         "-998" = -9,
         "-997" = -9,
         "-995" = -9,
         "-94"  = -8,
         "-92"  = -9,
         "-91"  = -1,
         "-99"  = -3,
         "-100" = -9,
         "-97"  = -9,
         -8)
}

# Map to major NS-SEC categories 1-7
major_nssec_map <- function(x) {
  if (is.na(x)) return(NA_real_)
  if (x < 0) return(x)
  if (x %in% 1:7) return(x)
  int_part <- floor(x)
  if (int_part %in% 1:7) return(int_part)
  return(NA_real_)
}

transform_nssec <- function(x) {
  if (is.null(x)) return(NULL)
  vapply(x, function(v){
    if (is.na(v)) return(NA_real_)
    if (v < 0) return(standard_missing_code(v))
    return(major_nssec_map(v))
  }, FUN.VALUE = numeric(1))
}

read_tab <- function(file) {
  path <- file.path("data/input", file)
  if (!file.exists(path) || file.info(path)$size == 0) {
    message("File missing or empty: ", path)
    return(NULL)
  }
  read_delim(path, delim = "\t", col_types = cols(), trim_ws = TRUE, progress = FALSE)
}

files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_five_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_main_interview.tab"
)

list_df <- list()
for (f in files) {
  df <- read_tab(f)
  if (!is.null(df) && "NSID" %in% names(df)) {
    list_df[[f]] <- df
  }
}

merged_df <- reduce(list_df, full_join, by = "NSID")

merged_df <- merged_df %>% mutate(
  nssec17 = if ("W4nsseccatYP" %in% names(.)) transform_nssec(as.numeric(.[["W4nsseccatYP"]])) else NA_real_,
  nssec18 = if ("W5nsseccatYP" %in% names(.)) transform_nssec(as.numeric(.[["W5nsseccatYP"]])) else NA_real_,
  nssec19 = if ("w6nsseccatYP" %in% names(.)) transform_nssec(as.numeric(.[["w6nsseccatYP"]])) else NA_real_,
  nssec20 = if ("W7NSSECCat" %in% names(.)) transform_nssec(as.numeric(.[["W7NSSECCat"]])) else NA_real_,
  nssec25 = if ("W8DNSSEC17" %in% names(.)) transform_nssec(as.numeric(.[["W8DNSSEC17"]])) else NA_real_,
  nssec32 = if ("W9NSSEC" %in% names(.)) transform_nssec(as.numeric(.[["W9NSSEC"]])) else NA_real_
)

final_df <- merged_df %>% select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

write_csv(final_df, "data/output/cleaned_data.csv")

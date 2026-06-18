library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Custom loading function
load_tab <- function(f) {
  path <- paste0('data/input/', f)
  if (!file.exists(path)) return(data.frame(NSID = character()))
  df <- readr::read_delim(path, delim = '\t', show_col_types = FALSE)
  if (nrow(df) == 0) return(data.frame(NSID = character()))
  if (!'NSID' %in% colnames(df)) return(data.frame(NSID = character()))
  df <- df %>% mutate(NSID = as.character(NSID))
  return(df)
}

# Load datasets
data_w1 <- load_tab('wave_one_lsype_young_person_2020.tab')
data_w4 <- load_tab('wave_four_lsype_young_person_2020.tab')
data_w5 <- load_tab('wave_five_lsype_young_person_2020.tab')
data_w6 <- load_tab('wave_six_lsype_young_person_2020.tab')
data_w7 <- load_tab('wave_seven_lsype_young_person_2020.tab')
data_w8 <- load_tab('ns8_2015_derived.tab')
data_w9 <- load_tab('ns9_2022_main_interview.tab')

# Merge datasets
full_df <- data_w1 %>%
  full_join(data_w4, by = 'NSID') %>%
  full_join(data_w5, by = 'NSID') %>%
  full_join(data_w6, by = 'NSID') %>%
  full_join(data_w7, by = 'NSID') %>%
  full_join(data_w8, by = 'NSID') %>%
  full_join(data_w9, by = 'NSID')

clean_nssec <- function(var) {
  if (missing(var) || is.null(var) || all(is.na(var))) return(rep(-3, 1))
  val_int <- floor(var)
  res <- case_when(
    var == -91 ~ -1,
    var == -99 ~ -3,
    var < -98 ~ -2,
    val_int >= 1 & val_int <= 17 ~ val_int,
    TRUE ~ -3
  )
  return(res)
}

safe_mutate_nssec <- function(df, target_name, source_name) {
  if (source_name %in% colnames(df)) {
    df <- df %>% mutate(!!target_name := clean_nssec(!!sym(source_name)))
  } else {
    df <- df %>% mutate(!!target_name := -3)
  }
  return(df)
}

full_df <- full_df %>%
  safe_mutate_nssec('nssec17', 'W4nsseccatYP') %>%
  safe_mutate_nssec('nssec18', 'W5nsseccatYP') %>%
  safe_mutate_nssec('nssec19', 'w6nsseccatYP') %>%
  safe_mutate_nssec('nssec20', 'W7NSSECCat')

if ('W8DNSSEC17' %in% colnames(full_df)) {
  full_df <- full_df %>% mutate(nssec25_raw = clean_nssec(W8DNSSEC17))
} else {
  full_df <- full_df %>% mutate(nssec25_raw = -3)
}

if ('W8DACTIVITYC' %in% colnames(full_df)) {
  full_df <- full_df %>% mutate(nssec25 = case_when(
    W8DACTIVITYC == 5 ~ 15,
    nssec25_raw != -3 ~ nssec25_raw,
    TRUE ~ -3
  ))
} else {
  full_df <- full_df %>% mutate(nssec25 = case_when(nssec25_raw != -3 ~ nssec25_raw, TRUE ~ -3))
}

if ('W9NSSEC' %in% colnames(full_df)) {
  full_df <- full_df %>% mutate(nssec32 = case_when(
    W9NSSEC == -1.0 ~ -1,
    W9NSSEC >= 1 & W9NSSEC <= 17 ~ floor(W9NSSEC),
    TRUE ~ -3
  ))
} else {
  full_df <- full_df %>% mutate(nssec32 = -3)
}

final_vars <- full_df %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Define value labels as a named numeric vector for labels
# The labelled package expects the names to be the labels and the values to be the numeric codes
nssec_vals <- c(-3, -2, -1, 1:17)
nssec_labs <- c(
  "Not asked", "Schedule not applicable", "Not applicable",
  "Employers in large organisations", "Higher managerial and administrative occupations",
  "Higher professional occupations", "Lower professional and higher technical occupations",
  "Lower managerial and administrative occupations", "Higher supervisory occupations",
  "Intermediate occupations", "Employers in small establishments", "Own account workers",
  "Lower supervisory occupations", "Lower technical occupations", "Semi-routine occupations",
  "Routine occupations", "Never worked and Long-term unemployed", "Full-time students",
  "Occupations not stated or inadequately described", "Not classifiable for other reasons"
)

# Create a named vector where names = codes, values = labels
# Actually, labelled::set_value_labels takes a named vector where names are the labels and values are the codes
# or a named vector where names are the codes and values are the labels depending on the version/method.
# Let's use the most robust way: a named vector where names are labels and values are codes.
val_labels_vec <- nssec_vals
names(val_labels_vec) <- nssec_labs

for (col in colnames(final_vars)) {
  if (col != 'NSID') {
    # Ensure column is numeric
    final_vars[[col]] <- as.numeric(final_vars[[col]])
    final_vars[[col]] <- set_value_labels(final_vars[[col]], val_labels_vec)
  }
}

readr::write_csv(final_vars, 'data/output/cleaned_data.csv')

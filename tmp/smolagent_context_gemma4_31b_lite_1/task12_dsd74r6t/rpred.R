library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# List of files from metadata
file_list <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_five_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_main_interview.tab'
)

load_tab <- function(filename) {
  df <- read_delim(paste0('data/input/', filename), delim = '\t', col_types = cols(.default = 'c'))
  nsid_col <- grep('^NSID$', colnames(df), ignore.case = TRUE, value = TRUE)
  if (length(nsid_col) > 0) {
    df <- df %>% rename(NSID = !!sym(nsid_col[1]))
    df$NSID <- trimws(df$NSID)
  } else {
    df$NSID <- NA_character_
  }
  return(df)
}

data_list <- map(file_list, load_tab)

full_frame <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_frame <- full_join(full_frame, data_list[[i]], by = 'NSID')
}

harmonise_nssec <- function(x) {
  if (is.null(x)) return(rep(NA_real_, 0)) # Handle missing columns
  val <- as.numeric(x)
  res <- case_when(
    val == -91 ~ -1, 
    val == -99 ~ -3, 
    val < -1  ~ -3, 
    TRUE ~ val
  )
  res_collapsed <- floor(res)
  final <- ifelse(res >= 1, res_collapsed, res)
  return(final)
}

# Use a safer way to assign columns to avoid "Unknown or uninitialised column" errors
# and ensure they are created even if the source column is missing

# Define mapping of target variable to source variable
mapping <- c(
  nssec17 = 'W4nsseccatYP',
  nssec18 = 'W5nsseccatYP',
  nssec19 = 'w6nsseccatYP',
  nssec20 = 'W7NSSECCat',
  nssec25 = 'W8DNSSEC17',
  nssec32 = 'W9NSSEC'
)

for (target in names(mapping)) {
  source_col <- mapping[target]
  if (source_col %in% colnames(full_frame)) {
    full_frame[[target]] <- harmonise_nssec(full_frame[[source_col]])
  } else {
    full_frame[[target]] <- NA_real_
  }
}

nssec_labels <- c(
  "1" = "Employers in large organisations",
  "2" = "Higher managerial and administrative occupations",
  "3" = "Higher professional occupations",
  "4" = "Lower professional and higher technical occupations",
  "5" = "Lower managerial and administrative occupations",
  "6" = "Higher supervisory occupations",
  "7" = "Intermediate occupations",
  "8" = "Employers in small establishments",
  "9" = "Own account workers",
  "10" = "Lower supervisory occupations",
  "11" = "Lower technical occupations",
  "12" = "Semi-routine occupations",
  "13" = "Routine occupations",
  "14" = "Never worked and Long-term unemployed",
  "15" = "Full-time students",
  "16" = "Occupations not stated or inadequately described",
  "17" = "Not classifiable for other reasons",
  "-1" = "Not applicable",
  "-2" = "Schedule not applicable / script error / information lost",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-7" = "Prefer not to say",
  "-8" = "Don't know / insufficient information",
  "-9" = "Refusal"
)

apply_nssec_labels <- function(x) {
  char_x <- as.character(x)
  f <- factor(char_x, levels = names(nssec_labels), labels = nssec_labels)
  return(f)
}

vars_to_label <- names(mapping)
full_frame <- full_frame %>% 
  mutate(across(all_of(vars_to_label), apply_nssec_labels))

final_data <- full_frame %>% 
  select(NSID, all_of(vars_to_label))

write_csv(final_data, 'data/output/cleaned_data.csv')
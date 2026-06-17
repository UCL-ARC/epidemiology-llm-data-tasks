library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Load only necessary columns to save memory
files_vars <- list(
  'wave_one_lsype_young_person_2020.tab' = c('NSID', 'W1ethnic2YP'),
  'wave_two_lsype_young_person_2020.tab' = c('NSID', 'W2ethnicYP'),
  'wave_four_lsype_young_person_2020.tab' = c('NSID', 'w4ethnic2YP'),
  'ns8_2015_derived.tab' = c('NSID', 'W8DETHN15'),
  'ns9_2022_derived_variables.tab' = c('NSID', 'W9DETHN15')
)

load_subset <- function(filename, vars) {
  # Read only the columns we need
  df <- read_delim(paste0('data/input/', filename), delim = '\t', col_types = readr::cols(.default = 'double'))
  df <- df %>% select(all_of(vars))
  df$NSID <- as.character(df$NSID)
  return(df)
}

data_list <- lapply(names(files_vars), function(f) {
  load_subset(f, files_vars[[f]])
})

# Merge datasets
# The error indicated a many-to-many relationship, likely due to duplicate NSIDs in some files.
# We should keep only unique NSIDs per file before joining.

clean_list <- lapply(data_list, function(df) {
  df %>% distinct(NSID, .keep_all = TRUE)
})

merged_data <- clean_list[[1]]
for(i in 2:length(clean_list)) {
  merged_data <- full_join(merged_data, clean_list[[i]], by = 'NSID')
}

# 2. Define harmonisation mapping for ethnicity
harmonise_missing <- function(x, wave_name) {
  res <- x
  if (wave_name == 'W1') {
    res[x == -999.0] <- -2
    res[x == -94.0] <- -8
    res[x == -92.0] <- -9
    res[x == -91.0] <- -1
    res[x == -1.0] <- -8
  } else if (wave_name == 'W2') {
    res[x == -998.0] <- -2
    res[x == -997.0] <- -2
    res[x == -995.0] <- -2
    res[x == -99.0] <- -3
    res[x == -92.0] <- -9
    res[x == -91.0] <- -1
    res[x == -1.0] <- -8
  } else if (wave_name == 'W4') {
    res[x == -94.0] <- -8
    res[x == -1.0] <- -8
  } else if (wave_name == 'W8') {
    res[x == -9.0] <- -9
    res[x == -8.0] <- -8
    res[x == -1.0] <- -1
  } else if (wave_name == 'W9') {
    res[x == -8.0] <- -8
  }
  res[is.na(res)] <- -3
  return(res)
}

merged_data <- merged_data %>%
  mutate(
    eth_w1 = harmonise_missing(W1ethnic2YP, 'W1'),
    eth_w2 = harmonise_missing(W2ethnicYP, 'W2'),
    eth_w4 = harmonise_missing(w4ethnic2YP, 'W4'),
    eth_w8 = harmonise_missing(W8DETHN15, 'W8'),
    eth_w9 = harmonise_missing(W9DETHN15, 'W9')
  )

# 3. Consolidation (Earliest-valid-first)
merged_data$eth <- apply(merged_data[, c('eth_w1', 'eth_w2', 'eth_w4', 'eth_w8', 'eth_w9')], 1, function(row) {
  for(v in row) {
    if(!is.na(v) && v >= 1 && v <= 16) return(v)
  }
  for(v in row) {
    if(!is.na(v)) return(v)
  }
  return(-3)
})

eth_labels <- c(
  "1" = "White - British",
  "2" = "White - Irish",
  "3" = "Any other White background",
  "4" = "Mixed - White and Black Caribbean",
  "5" = "Mixed - White and Black African",
  "6" = "Mixed - White and Asian",
  "7" = "Any other mixed background",
  "8" = "Indian",
  "9" = "Pakistani",
  "10" = "Bangladeshi",
  "11" = "Any other Asian background",
  "12" = "Black Caribbean",
  "13" = "Black African",
  "14" = "Any other Black background",
  "15" = "Chinese",
  "16" = "Any other ethnic background",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

merged_data$eth <- factor(merged_data$eth, levels = names(eth_labels), labels = eth_labels)

final_df <- merged_data %>%
  select(NSID, eth)

write_csv(final_df, 'data/output/cleaned_data.csv')

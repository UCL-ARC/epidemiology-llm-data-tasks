library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Memory optimization: Load only necessary columns
files_info <- list(
  'wave_one_lsype_young_person_2020.tab' = c('NSID', 'W1ethnic2YP'),
  'wave_two_lsype_young_person_2020.tab' = c('NSID', 'W2ethnicYP'),
  'wave_four_lsype_young_person_2020.tab' = c('NSID', 'w4ethnic2YP'),
  'ns8_2015_derived.tab' = c('NSID', 'W8DETHN15'),
  'ns9_2022_derived_variables.tab' = c('NSID', 'W9DETHN15')
)

load_subset <- function(filename, cols) {
  path <- paste0('data/input/', filename)
  df <- read_delim(path, delim = '\t', col_types = cols(NSID = col_character(), .default = col_double())) %>%
    select(all_of(cols)) %>%
    distinct(NSID, .keep_all = TRUE)
  return(df)
}

data_list <- map2(names(files_info), files_info, load_subset)
merged_df <- reduce(data_list, full_join, by = 'NSID')

harmonize_missing <- function(x) {
  x <- as.numeric(x)
  res <- x
  res[x == -999 | x == -99 | x == -998 | x == -997 | x == -995] <- -3
  res[x == -94] <- -8
  res[x == -92] <- -9
  res[x == -91] <- -1
  res[is.na(res)] <- -3
  return(res)
}

merged_df <- merged_df %>%
  mutate(
    eth14 = harmonize_missing(W1ethnic2YP),
    eth15 = harmonize_missing(W2ethnicYP),
    eth17 = harmonize_missing(w4ethnic2YP),
    eth_w8 = harmonize_missing(W8DETHN15),
    eth_w9 = harmonize_missing(W9DETHN15)
  )

consolidate_stable <- function(df, vars) {
  # Simple row-wise approach to avoid max.col argument issues
  res <- apply(df[, vars], 1, function(row) {
    vals <- as.numeric(row)
    pos_vals <- vals[vals > 0]
    if (length(pos_vals) > 0) {
      return(pos_vals[1])
    } else {
      last_val <- tail(vals, 1)
      return(if(is.na(last_val)) -3 else last_val)
    }
  })
  return(as.numeric(res))
}

ethnic_ordered <- c('eth14', 'eth15', 'eth17', 'eth_w8', 'eth_w9')
merged_df$eth <- consolidate_stable(merged_df, ethnic_ordered)

eth_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-1" = "Item not applicable",
  "-3" = "Not asked/interviewed",
  "-2" = "Schedule not applicable",
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
  "16" = "Any other ethnic background"
)

merged_df$eth <- factor(merged_df$eth, levels = as.numeric(names(eth_labels)), labels = eth_labels)

final_df <- merged_df %>%
  select(NSID, eth)

write_csv(final_df, 'data/output/cleaned_data.csv')

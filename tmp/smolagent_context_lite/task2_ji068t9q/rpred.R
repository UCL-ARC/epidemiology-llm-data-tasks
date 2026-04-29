library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

# Load all files and store in a list
data_list <- map(files, ~ read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = 'c')))
names(data_list) <- files

# Merge datasets using full_join by NSID
# Start with the first file as the base frame
full_df <- data_list[[1]]

for (i in 2:length(data_list)) {
  full_df <- full_join(full_df, data_list[[i]], by = 'NSID')
}

# Helper for missing value mapping
map_missing <- function(val, labels_map) {
  if (is.na(val)) return(-3)
  val_num <- as.numeric(val)
  if (is.na(val_num)) return(-3)
  
  label <- labels_map[[as.character(val_num)]]
  if (is.null(label)) return(-3)
  
  lbl <- tolower(label)
  if (grepl('refused', lbl)) return(-9)
  if (grepl('insufficient', lbl) || grepl('dont know', lbl) || grepl('don\'t know', lbl)) return(-8)
  if (grepl('prefer not to say', lbl)) return(-7)
  if (grepl('not interviewed', lbl) || grepl('not asked', lbl)) return(-3)
  if (grepl('not applicable', lbl)) return(-1)
  if (grepl('lost', lbl) || grepl('error', lbl) || grepl('script', lbl)) return(-2)
  
  return(val_num)
}

# Labels
w1_labels <- list('-999.0' = 'Missing - household data lost', '-94.0' = 'Insufficient information', '-92.0' = 'Refused', '-91.0' = 'Not applicable', '-1.0' = "Don't know", '1.0' = 'White - British', '2.0' = 'White - Irish', '3.0' = 'Any other White background', '4.0' = 'Mixed - White and Black Caribbean', '5.0' = 'Mixed - White and Black African', '6.0' = 'Mixed - White and Asian', '7.0' = 'Any other mixed background', '8.0' = 'Indian', '9.0' = 'Pakistani', '10.0' = 'Bangladeshi', '11.0' = 'Any other Asian background', '12.0' = 'Black Caribbean', '13.0' = 'Black African', '14.0' = 'Any other Black background', '15.0' = 'Chinese', '16.0' = 'Any other ethnic background')
w2_labels <- list('-998.0' = 'Interviewer missed question', '-997.0' = 'Script error', '-995.0' = 'Missing history section data - unexplained', '-99.0' = 'YP not interviewed', '-92.0' = 'Refused', '-91.0' = 'Not applicable', '-1.0' = "Don't Know", '1.0' = 'White - British', '2.0' = 'White - Irish', '3.0' = 'Any other White background', '4.0' = 'White and Black Caribbean', '5.0' = 'White and Black African', '6.0' = 'White and Asian', '7.0' = 'Any other mixed background', '8.0' = 'Indian', '9.0' = 'Pakistani', '10.0' = 'Bangladeshi', '11.0' = 'Any other Asian background', '12.0' = 'Caribbean', '13.0' = 'African', '14.0' = 'Any other Black background', '15.0' = 'Chinese', '16.0' = 'Any other')
w4_labels <- list('-94.0' = 'Insufficient information', '-1.0' = "Don't know", '1.0' = 'White - British', '2.0' = 'White - Irish', '3.0' = 'Any other White background', '4.0' = 'Mixed - White and Black Caribbean', '5.0' = 'Mixed - White and Black African', '6.0' = 'Mixed - White and Asian', '7.0' = 'Any other mixed background', '8.0' = 'Indian', '9.0' = 'Pakistani', '10.0' = 'Bangladeshi', '11.0' = 'Any other Asian background', '12.0' = 'Black Caribbean', '13.0' = 'Black African', '14.0' = 'Any other Black background', '15.0' = 'Chinese', '16.0' = 'Any other ethnic background')
w8_labels <- list('-9.0' = 'Refused', '-8.0' = 'Insufficient information', '-1.0' = 'Not applicable', '1.0' = 'White - British', '2.0' = 'White - Irish', '3.0' = 'Any other White background', '4.0' = 'Mixed - White and Black Caribbean', '5.0' = 'Mixed - White and Black African', '6.0' = 'Mixed - White and Asian', '7.0' = 'Any other mixed background', '8.0' = 'Asian/Asian British - Indian', '9.0' = 'Asian/Asian British - Pakistani', '10.0' = 'Asian/Asian British - Bangladeshi', '11.0' = 'Other other Asian background', '12.0' = 'Black/Black British - Caribbean', '13.0' = 'Black/Black British - African', '14.0' = 'Any other Black background', '15.0' = 'Chinese', '16.0' = 'Any other background')
w9_labels <- list('-8.0' = 'Insufficient information', '1.0' = 'White - British', '2.0' = 'White - Irish', '3.0' = 'Any other White background', '4.0' = 'Mixed - White and Black Caribbean', '5.0' = 'Mixed - White and Black African', '6.0' = 'Mixed - White and Asian', '7.0' = 'Any other Mixed background', '8.0' = 'Asian/Asian British - Indian', '9.0' = 'Asian/Asian British - Pakistani', '10.0' = 'Asian/Asian British - Bangladeshi', '11.0' = 'Any other Asian background', '12.0' = 'Black/Black British - Caribbean', '13.0' = 'Black/Black British - African', '14.0' = 'Any other Black background', '15.0' = 'Chinese', '16.0' = 'Any other background')

# Clean each wave variable using map_dbl on the vector directly
full_df <- full_df %>%
  mutate(
    eth_w1 = map_dbl(W1ethnic2YP, ~map_missing(.x, w1_labels)),
    eth_w2 = map_dbl(W2ethnicYP, ~map_missing(.x, w2_labels)),
    eth_w4 = map_dbl(w4ethnic2YP, ~map_missing(.x, w4_labels)),
    eth_w8 = map_dbl(W8DETHN15, ~map_missing(.x, w8_labels)),
    eth_w9 = map_dbl(W9DETHN15, ~map_missing(.x, w9_labels))
  )

# Consolidation: earliest-valid-first
full_df <- full_df %>%
  mutate(
    eth = case_when(
      eth_w1 >= 1 & eth_w1 <= 16 ~ eth_w1,
      eth_w2 >= 1 & eth_w2 <= 16 ~ eth_w2,
      eth_w4 >= 1 & eth_w4 <= 16 ~ eth_w4,
      eth_w8 >= 1 & eth_w8 <= 16 ~ eth_w8,
      eth_w9 >= 1 & eth_w9 <= 16 ~ eth_w9,
      eth_w1 < 1 ~ eth_w1,
      eth_w2 < 1 ~ eth_w2,
      eth_w4 < 1 ~ eth_w4,
      eth_w8 < 1 ~ eth_w8,
      eth_w9 < 1 ~ eth_w9,
      TRUE ~ -3
    )
  )

eth_labels <- c(
  "1" = "White - British", "2" = "White - Irish", "3" = "Any other White background",
  "4" = "Mixed - White and Black Caribbean", "5" = "Mixed - White and Black African", "6" = "Mixed - White and Asian",
  "7" = "Any other mixed background", "8" = "Indian", "9" = "Pakistani", "10" = "Bangladeshi",
  "11" = "Any other Asian background", "12" = "Black Caribbean", "13" = "Black African",
  "14" = "Any other Black background", "15" = "Chinese", "16" = "Any other ethnic background",
  "-9" = "Refusal", "-8" = "Don't know/insufficient", "-7" = "Prefer not to say",
  "-3" = "Not asked/NA", "-2" = "Schedule not applicable/lost", "-1" = "Not applicable"
)

full_df$eth <- factor(full_df$eth, levels = as.numeric(names(eth_labels)), labels = eth_labels)

final_df <- full_df %>%
  select(NSID, eth)

write_csv(final_df, 'data/output/cleaned_data.csv')

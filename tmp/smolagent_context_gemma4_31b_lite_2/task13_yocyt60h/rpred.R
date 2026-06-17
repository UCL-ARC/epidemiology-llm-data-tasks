library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab',
  'wave_five_lsype_family_background_2020.tab'
)

all_data <- files %>% 
  map(~ read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = "numeric", NSID = col_character()))) %>% 
  set_names(files)

# Merge datasets
merged_df <- all_data[[1]]
for (i in 2:length(all_data)) {
  merged_df <- full_join(merged_df, all_data[[i]], by = "NSID")
}

# 2. NS-SEC Major Category Mapping Function
# Major categories typically are:
# 1: 1.0, 2.0
# 2: 3.x, 4.x
# 3: 5.0, 6.0
# 4: 7.x, 8.x, 9.x
# 5: 10.0, 11.x
# 6: 12.x, 13.x
# 7: 14.x
# 8: 15.0
# 9: 16.0, 17.0

collapse_nssec <- function(x) {
  res <- rep(NA, length(x))
  
  # Substantive categories
  res[x >= 1 & x < 3] <- 1 # Employers large, Higher managerial
  res[x >= 3 & x < 5] <- 2 # Higher professional, Lower professional
  res[x >= 5 & x < 7] <- 3 # Lower managerial, Higher supervisory
  res[x >= 7 & x < 10] <- 4 # Intermediate, Small org employers, Own account
  res[x >= 10 & x < 12] <- 5 # Lower supervisory, Lower technical craft
  res[x >= 12 & x < 14] <- 6 # Semi routine, Routine
  res[x >= 14 & x < 15] <- 7 # Never worked, Long-term unemployed, Not currently working
  res[x == 15] <- 8 # Full-time students
  res[x >= 16] <- 9 # Not classified
  
  # Missing values mapping based on labels
  # -999: Missing household data lost / Missing household info -> -2
  # -99: Mother/Father not interviewed -> -3
  # -98: Mother/Father not present -> -1
  # -94: Insufficient information -> -8
  # R NA -> -3
  
  # Since we are applying this to columns, we need to handle the specific raw codes
  # The logic above handles substantive. Now handle missing specifically.
  # Let's refine this into a mapping based on the provided labels.
  return(res)
}

# Refined mapping function that handles specific raw codes
process_nssec <- function(var_name, df) {
  vals <- df[[var_name]]
  out <- rep(NA, length(vals))
  
  # Substantive
  out[vals >= 1 & vals < 3] <- 1
  out[vals >= 3 & vals < 5] <- 2
  out[vals >= 5 & vals < 7] <- 3
  out[vals >= 7 & vals < 10] <- 4
  out[vals >= 10 & vals < 12] <- 5
  out[vals >= 12 & vals < 14] <- 6
  out[vals >= 14 & vals < 15] <- 7
  out[vals == 15] <- 8
  out[vals >= 16] <- 9
  
  # Missing labels from metadata
  # -999: Missing - household data lost -> -2
  out[vals == -999] <- -2
  # -99: Not interviewed -> -3
  out[vals == -99] <- -3
  # -98: Not present -> -1
  out[vals == -98] <- -1
  # -94: Insufficient information -> -8
  out[vals == -94] <- -8
  
  # R NA -> -3
  out[is.na(out)] <- -3
  
  return(out)
}

# Target variables and source mappings
# Age mapping: wave1->14, wave2->15, wave3->16, wave4->17, wave5->18
mappings <- list(
  nssecma14 = "W1nsseccatmum",
  nssecpa14 = "W1nsseccatdad",
  nssecma15 = "W2nsseccatmum",
  nssecpa15 = "W2nsseccatdad",
  nssecma16 = "W3cnsseccatmum",
  nssecpa16 = "W3cnsseccatdad",
  nssecma17 = "w4cnsseccatmum",
  nssecpa17 = "w4cnsseccatdad",
  nssecma18 = "w5Cnsseccatmum",
  nssecpa18 = "w5Cnsseccatdad"
)

final_df <- data.frame(NSID = merged_df$NSID)

for (target in names(mappings)) {
  source <- mappings[[target]]
  final_df[[target]] <- process_nssec(source, merged_df)
}

# Apply labels to categorical variables
nssec_labels <- c(
  "1" = "Employers/Higher Managerial",
  "2" = "Higher/Lower Professional",
  "3" = "Lower Managerial/Higher Supervisory",
  "4" = "Intermediate/Small Employers/Own Account",
  "5" = "Lower Supervisory/Lower Tech Craft",
  "6" = "Semi-routine/Routine",
  "7" = "Unemployed/Never worked",
  "8" = "Full-time students",
  "9" = "Not classified",
  "-1" = "Not applicable",
  "-2" = "Schedule not applicable/information lost",
  "-3" = "Not asked/not interviewed",
  "-8" = "Don't know/insufficient information"
)

for (target in names(mappings)) {
  final_df[[target]] <- factor(final_df[[target]], levels = as.numeric(names(nssec_labels)), labels = nssec_labels)
}

# Write output
readr::write_csv(final_df, 'data/output/cleaned_data.csv')
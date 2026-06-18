library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_three_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_five_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns9_2022_main_interview.tab'
)

load_data <- function(file) {
  read_delim(paste0('data/input/', file), delim = '\t', col_types = cols(.default = "c"))
}

data_list <- map(files, load_data)

# Merge datasets
merged_data <- data_list %>% reduce(full_join, by = 'NSID')

# Define a function to clean sex variables based on labels
# Standard Missing-Value Codes:
# -9 = Refusal, -8 = Don't know, -7 = Prefer not to say, -3 = Not asked, -2 = Schedule not applicable/script error, -1 = Item not applicable

clean_sex <- function(var_name, labels) {
  # Convert to numeric first
  vec <- as.numeric(merged_data[[var_name]])
  
  # Map based on labels provided in metadata
  # We use a temporary vector to avoid overwriting while processing
  cleaned <- rep(NA, length(vec))
  
  # Valid values (Male=1, Female=2)
  cleaned[vec == 1] <- 1
  cleaned[vec == 2] <- 2
  
  # Missing values based on labels
  for (val in names(labels)) {
    val_num <- as.numeric(val)
    label_text <- labels[[val]]
    
    if (grepl("Refused", label_text, ignore.case = TRUE)) {
      cleaned[vec == val_num] <- -9
    } else if (grepl("Don't know", label_text, ignore.case = TRUE) || grepl("insufficient information", label_text, ignore.case = TRUE)) {
      cleaned[vec == val_num] <- -8
    } else if (grepl("Prefer not to say", label_text, ignore.case = TRUE)) {
      cleaned[vec == val_num] <- -7
    } else if (grepl("Not applicable", label_text, ignore.case = TRUE)) {
      cleaned[vec == val_num] <- -1
    } else if (grepl("YP not interviewed", label_text, ignore.case = TRUE)) {
      cleaned[vec == val_num] <- -3
    } else if (grepl("Script error", label_text, ignore.case = TRUE) || grepl("interviewer missed", label_text, ignore.case = TRUE) || grepl("unexplained", label_text, ignore.case = TRUE)) {
      cleaned[vec == val_num] <- -2
    }
  }
  
  # Convert remaining NA to -3 (Not asked)
  cleaned[is.na(cleaned)] <- -3
  return(cleaned)
}

# Process each wave
# Wave 1: W1sexYP
sex1 <- clean_sex('W1sexYP', list('-99.0' = 'YP not interviewed', '-92.0' = 'Refused', '-91.0' = 'Not applicable', '1.0' = 'Male', '2.0' = 'Female'))
# Wave 2: W2SexYP
sex2 <- clean_sex('W2SexYP', list('-998.0' = 'Interviewer missed question', '-997.0' = 'Script error', '-995.0' = 'Missing history section data - unexplained', '-99.0' = 'YP not interviewed', '-92.0' = 'Refused', '-91.0' = 'Not applicable', '-1.0' = "Don't Know", '1.0' = 'Male', '2.0' = 'Female'))
# Wave 3: W3sexYP
sex3 <- clean_sex('W3sexYP', list('-99.0' = 'YP not interviewed', '-92.0' = 'Refused', '-91.0' = 'Not applicable', '1.0' = 'Male', '2.0' = 'Female'))
# Wave 4: W4SexYP
sex4 <- clean_sex('W4SexYP', list('-99.0' = 'YP not interviewed', '-92.0' = 'Refused', '-91.0' = 'Not applicable', '-1.0' = "Don't know", '1.0' = 'Male', '2.0' = 'Female'))
# Wave 5: W5SexYP
sex5 <- clean_sex('W5SexYP', list('-1.0' = "Don't know", '1.0' = 'Male', '2.0' = 'Female'))
# Wave 6: W6Sex
sex6 <- clean_sex('W6Sex', list('-92.0' = 'Refused', '-91.0' = 'Not applicable', '1.0' = 'Male', '2.0' = 'Female'))
# Wave 7: W7Sex
sex7 <- clean_sex('W7Sex', list('-91.0' = 'Not applicable', '1.0' = 'Male', '2.0' = 'Female'))
# Wave 8: W8CMSEX
sex8 <- clean_sex('W8CMSEX', list('-9.0' = 'Refused', '-8.0' = "Don't know", '-1.0' = 'Not applicable', '1.0' = 'Male', '2.0' = 'Female'))
# Wave 9: W9DSEX
sex9 <- clean_sex('W9DSEX', list('1.0' = 'Male', '2.0' = 'Female'))

# Consolidation for Sex: most-recent-valid-first
# Order: W9, W8, W7, W6, W5, W4, W3, W2, W1
sex_matrix <- cbind(sex9, sex8, sex7, sex6, sex5, sex4, sex3, sex2, sex1)

# Find most recent valid (substantive) response
# Substantive values are 1 and 2
final_sex <- apply(sex_matrix, 1, function(row) {
  for (val in row) {
    if (val == 1 || val == 2) return(val)
  }
  # If no substantive value, find the most recent missing code
  for (val in row) {
    if (!is.na(val)) return(val)
  }
  return(-3)
})

# Create final dataframe
final_df <- data.frame(NSID = merged_data$NSID, sex = final_sex)

# Create factor with labels
final_df$sex <- factor(final_df$sex, 
                       levels = c(1, 2, -9, -8, -7, -3, -2, -1),
                       labels = c('Male', 'Female', 'Refusal', 'Don\'t know / insufficient information', 'Prefer not to say', 'Not asked at the fieldwork stage / not interviewed', 'Schedule not applicable / script error / information lost', 'Item not applicable'))

# Write output
write_csv(final_df, 'data/output/cleaned_data.csv')

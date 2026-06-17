library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
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

data_list <- map(files, ~ read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = col_double(), NSID = col_character()))) 

# Merge all datasets by NSID
full_df <- data_list %>% reduce(full_join, by = 'NSID')

# Define mapping for missing values based on general guidance and metadata
# Standard: -9=Refusal, -8=DK, -7=Prefer not to say, -3=Not asked, -2=NA/Script error, -1=Not applicable

clean_sex <- function(val, wave_labels) {
  if (is.na(val)) return(-3)
  
  # Check if it's a valid substantive response first
  if (val == 1) return(1)
  if (val == 2) return(2)
  
  # Map negative codes based on labels
  # We look for the label associated with the value in the metadata
  # Since we are in a function, we'll handle common patterns mentioned in guidance
  # and specific metadata labels provided in the prompt.
  
  # Most wave-specific variables follow: -99=not interviewed (-3), -92=refused (-9), -91=not applicable (-1)
  # Wave 8: -9=refused (-9), -8=DK (-8), -1=not applicable (-1)
  
  if (val == -92 || val == -9) return(-9)
  if (val == -8 || val == -1.0) {
    # This is tricky because -1 is often 'Not applicable' in metadata but 'DK' in some
    # For Wave 2, 4, 5: -1.0 is "Don't Know" -> -8
    # For Wave 1, 3, 6, 7: -91 is "Not applicable" -> -1
    # Let's refine this inside the main loop per variable.
    return(-8)
  }
  if (val == -91) return(-1)
  if (val == -99) return(-3)
  
  return(-3) # Default for other negatives/NAs
}

# To be precise with the labels provided in metadata for each variable:
# W1sexYP: -99:YP not int (-3), -92:Ref (-9), -91:NA (-1)
# W2SexYP: -998, -997, -995: (-2), -99:YP not int (-3), -92:Ref (-9), -91:NA (-1), -1:DK (-8)
# W3sexYP: -99:YP not int (-3), -92:Ref (-9), -91:NA (-1)
# W4SexYP: -99:YP not int (-3), -92:Ref (-9), -91:NA (-1), -1:DK (-8)
# W5SexYP: -1:DK (-8)
# W6Sex: -92:Ref (-9), -91:NA (-1)
# W7Sex: -91:NA (-1)
# W8CMSEX: -9:Ref (-9), -8:DK (-8), -1:NA (-1)
# W9DSEX: No missing codes listed in value_labels, but general guidance says NA -> -3

process_var <- function(x, mapping) {
  res <- rep(NA, length(x))
  for (i in seq_along(x)) {
    val <- x[i]
    if (is.na(val)) {
      res[i] <- -3
    } else if (val == 1) {
      res[i] <- 1
    } else if (val == 2) {
      res[i] <- 2
    } else {
      # Map based on the provided mapping list
      # mapping is a named vector where names are raw values and values are target codes
      match <- as.character(val)
      if (match %in% names(mapping)) {
        res[i] <- as.numeric(mapping[match])
      } else {
        res[i] <- -3 # Default missing
      }
    }
  }
  return(res)
}

# Define mappings for each variable
map1 <- c("-99" = "-3", "-92" = "-9", "-91" = "-1")
map2 <- c("-998" = "-2", "-997" = "-2", "-995" = "-2", "-99" = "-3", "-92" = "-9", "-91" = "-1", "-1" = "-8")
map3 <- c("-99" = "-3", "-92" = "-9", "-91" = "-1")
map4 <- c("-99" = "-3", "-92" = "-9", "-91" = "-1", "-1" = "-8")
map5 <- c("-1" = "-8")
map6 <- c("-92" = "-9", "-91" = "-1")
map7 <- c("-91" = "-1")
map8 <- c("-9" = "-9", "-8" = "-8", "-1" = "-1")
map9 <- c()

full_df <- full_df %>%
  mutate(
    s1 = process_var(W1sexYP, map1),
    s2 = process_var(W2SexYP, map2),
    s3 = process_var(W3sexYP, map3),
    s4 = process_var(W4SexYP, map4),
    s5 = process_var(W5SexYP, map5),
    s6 = process_var(W6Sex, map6),
    s7 = process_var(W7Sex, map7),
    s8 = process_var(W8CMSEX, map8),
    s9 = process_var(W9DSEX, map9)
  )

# Consolidation logic: Most recent valid first (W9DSEX), then earliest to most recent (W1...W8)
# Valid response is 1 or 2.

get_sex <- function(row) {
  # Most recent first
  if (!is.na(row['s9']) && row['s9'] %in% c(1, 2)) return(row['s9'])
  
  # Then earliest to most recent (W1 to W8)
  waves <- c('s1', 's2', 's3', 's4', 's5', 's6', 's7', 's8')
  for (w in waves) {
    if (!is.na(row[w]) && row[w] %in% c(1, 2)) return(row[w])
  }
  
  # If no valid response, fall back to the most recent available missing code
  # The logic says "fall back to an appropriate harmonised missing code"
  # We'll take the value from s9, then s8...s1
  all_waves_rev <- c('s9', 's8', 's7', 's6', 's5', 's4', 's3', 's2', 's1')
  for (w in all_waves_rev) {
    if (!is.na(row[w])) return(row[w])
  }
  
  return(-3)
}

full_df$sex <- apply(full_df[, c('s1', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9')], 1, get_sex)

# Create labelled factor
full_df$sex <- factor(full_df$sex, levels = c(1, 2, -9, -8, -7, -3, -2, -1), 
                     labels = c("Male", "Female", "Refusal", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Not applicable"))

# Final selection
final_data <- full_df %>% select(NSID, sex)

write_csv(final_data, 'data/output/cleaned_data.csv')

library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- list(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

load_file <- function(filename) {
  read_delim(paste0('data/input/', filename), delim = "\t", col_types = cols(.default = "c"))
}

data_list <- map(files, load_file)
names(data_list) <- files

# Convert numeric columns to numeric
# In this specific task, we know the ethnicity variables we need
# W1ethnic2YP, W2ethnicYP, w4ethnic2YP, W8DETHN15, W9DETHN15

# Merge datasets
full_frame <- data_list[[1]] %>% 
  full_join(data_list[[2]], by = "NSID") %>% 
  full_join(data_list[[3]], by = "NSID") %>% 
  full_join(data_list[[4]], by = "NSID") %>% 
  full_join(data_list[[5]], by = "NSID")

# Helper function to clean ethnicity variables
clean_ethnicity <- function(var_name, labels_map) {
  # Ensure numeric
  vec <- as.numeric(full_frame[[var_name]])
  
  # Mapping based on metadata labels
  # Standard Missing-Value Codes:
  # -9 = Refusal, -8 = Don't know/insufficient, -7 = Prefer not to say, 
  # -3 = Not asked, -2 = Schedule not applicable, -1 = Item not applicable
  
  res <- rep(NA, length(vec))
  
  for (i in seq_along(vec)) {
    val <- vec[i]
    if (is.na(val)) {
      res[i] <- -3
    } else {
      # Check if val is in the labels_map
      val_str <- as.character(val)
      if (val_str %in% names(labels_map)) {
        label <- labels_map[[val_str]]
        
        if (grepl("Refused", label, ignore.case = TRUE)) res[i] <- -9
        else if (grepl("Insufficient information", label, ignore.case = TRUE) || 
                 grepl("Don't know", label, ignore.case = TRUE)) res[i] <- -8
        else if (grepl("Prefer not to say", label, ignore.case = TRUE)) res[i] <- -7
        else if (grepl("Not interviewed", label, ignore.case = TRUE) || 
                 grepl("Not asked", label, ignore.case = TRUE)) res[i] <- -3
        else if (grepl("Schedule not applicable", label, ignore.case = TRUE) || 
                 grepl("Script error", label, ignore.case = TRUE) || 
                 grepl("data lost", label, ignore.case = TRUE) || 
                 grepl("unexplained", label, ignore.case = TRUE)) res[i] <- -2
        else if (grepl("Not applicable", label, ignore.case = TRUE)) res[i] <- -1
        else {
          # Substantive value: keep the original code (1-16)
          res[i] <- val
        }
      }
    }
  }
  return(res)
}

# Define label maps from metadata
map1 <- list("-999.0"="Missing - household data lost", "-94.0"="Insufficient information", "-92.0"="Refused", "-91.0"="Not applicable", "-1.0"="Don't know", "1.0"="White - British", "2.0"="White - Irish", "3.0"="Any other White background", "4.0"="Mixed - White and Black Caribbean", "5.0"="Mixed - White and Black African", "6.0"="Mixed - White and Asian", "7.0"="Any other mixed background", "8.0"="Indian", "9.0"="Pakistani", "10.0"="Bangladeshi", "11.0"="Any other Asian background", "12.0"="Black Caribbean", "13.0"="Black African", "14.0"="Any other Black background", "15.0"="Chinese", "16.0"="Any other ethnic background")
map2 <- list("-998.0"="Interviewer missed question", "-997.0"="Script error", "-995.0"="Missing history section data - unexplained", "-99.0"="YP not interviewed", "-92.0"="Refused", "-91.0"="Not applicable", "-1.0"="Don't Know", "1.0"="White - British", "2.0"="White - Irish", "3.0"="Any other White background", "4.0"="White and Black Caribbean", "5.0"="White and Black African", "6.0"="White and Asian", "7.0"="Any other mixed background", "8.0"="Indian", "9.0"="Pakistani", "10.0"="Bangladeshi", "11.0"="Any other Asian background", "12.0"="Caribbean", "13.0"="African", "14.0"="Any other Black background", "15.0"="Chinese", "16.0"="Any other")
map4 <- list("-94.0"="Insufficient information", "-1.0"="Don't know", "1.0"="White - British", "2.0"="White - Irish", "3.0"="Any other White background", "4.0"="Mixed - White and Black Caribbean", "5.0"="Mixed - White and Black African", "6.0"="Mixed - White and Asian", "7.0"="Any other mixed background", "8.0"="Indian", "9.0"="Pakistani", "10.0"="Bangladeshi", "11.0"="Any other Asian background", "12.0"="Black Caribbean", "13.0"="Black African", "14.0"="Any other Black background", "15.0"="Chinese", "16.0"="Any other ethnic background")
map8 <- list("-9.0"="Refused", "-8.0"="Insufficient information", "-1.0"="Not applicable", "1.0"="White - British", "2.0"="White - Irish", "3.0"="Any other White background", "4.0"="Mixed - White and Black Caribbean", "5.0"="Mixed - White and Black African", "6.0"="Mixed - White and Asian", "7.0"="Any other mixed background", "8.0"="Asian/Asian British - Indian", "9.0"="Asian/Asian British - Pakistani", "10.0"="Asian/Asian British - Bangladeshi", "11.0"="Other other Asian background", "12.0"="Black/Black British - Caribbean", "13.0"="Black/Black British - African", "14.0"="Any other Black background", "15.0"="Chinese", "16.0"="Any other background")
map9 <- list("-8.0"="Insufficient information", "1.0"="White - British", "2.0"="White - Irish", "3.0"="Any other White background", "4.0"="Mixed - White and Black Caribbean", "5.0"="Mixed - White and Black African", "6.0"="Mixed - White and Asian", "7.0"="Any other Mixed background", "8.0"="Asian/Asian British - Indian", "9.0"="Asian/Asian British - Pakistani", "10.0"="Asian/Asian British - Bangladeshi", "11.0"="Any other Asian background", "12.0"="Black/Black British - Caribbean", "13.0"="Black/Black British - African", "14.0"="Any other Black background", "15.0"="Chinese", "16.0"="Any other background")

# Create cleaned vectors
eth1 <- clean_ethnicity("W1ethnic2YP", map1)
eth2 <- clean_ethnicity("W2ethnicYP", map2)
eth4 <- clean_ethnicity("w4ethnic2YP", map4)
eth8 <- clean_ethnicity("W8DETHN15", map8)
eth9 <- clean_ethnicity("W9DETHN15", map9)

# Consolidate ethnicity (Earliest valid positive response)
# Positive response is >= 1
consolidated_ethnicity <- eth1
for (val in list(eth2, eth4, eth8, eth9)) {
  consolidated_ethnicity <- ifelse(consolidated_ethnicity < 1 & val >= 1, val, consolidated_ethnicity)
}

# Final fallback for missing codes if all are < 1
# In case the first wave was NA (-3) and later waves have a more specific missing code, 
# usually we'd follow a hierarchy, but the requirement is simply "earliest valid positive response".
# If no positive response, keep the earliest available code.

# Handle missing value labels for the final factor
ethnicity_labels <- c(
  "1" = "White - British", "2" = "White - Irish", "3" = "Any other White background",
  "4" = "Mixed - White and Black Caribbean", "5" = "Mixed - White and Black African",
  "6" = "Mixed - White and Asian", "7" = "Any other mixed background",
  "8" = "Indian", "9" = "Pakistani", "10" = "Bangladeshi",
  "11" = "Any other Asian background", "12" = "Black Caribbean",
  "13" = "Black African", "14" = "Any other Black background",
  "15" = "Chinese", "16" = "Any other ethnic background",
  "-9" = "Refusal", "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say", "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost", "-1" = "Item not applicable"
)

# Create final dataframe
final_df <- data.frame(NSID = full_frame$NSID, ethnicity = consolidated_ethnicity)

# Convert to factor with labels
final_df$ethnicity <- factor(final_df$ethnicity, levels = names(ethnicity_labels), labels = ethnicity_labels)

# Save output
write_csv(final_df, "data/output/cleaned_data.csv")

library(dplyr)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE)

# Load all files from data/input/ as listed in metadata
# Files: wave_one_lsype_young_person_2020.tab, wave_four_lsype_young_person_2020.tab,
#        ns8_2015_derived.tab, ns9_2022_derived_variables.tab

file_list <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_derived_variables.tab"
)

# Load each file
files <- purrr::map(file_list, ~ read_delim(paste0("data/input/", .x), delim = "\t", show_col_types = FALSE))
names(files) <- file_list

# Merge all files by NSID using full_join
cleaned <- files[[1]]
for (i in 2:length(files)) {
  cleaned <- full_join(cleaned, files[[i]], by = "NSID")
}

# Function to handle BMI missing values
code_bmi <- function(x) {
  # Replace NA with -3 (Not asked / not interviewed)
  x[is.na(x)] <- -3
  
  # Check if there are any positive values (valid BMI)
  # If there are valid positive values, we keep them
  # Negative values should already be preserved from source
  # But we need to ensure standard codes are applied
  
  # The source data already has negative codes for missing values
  # We just need to ensure NA is converted to -3
  return(x)
}

# Create bmi25 from W8DBMI (Wave 8, Age 25)
# W8DBMI has user_missing_values: -9 thru -8 and -1
# Labels: -9 = Refused, -8 = Insufficient information, -1 = Not applicable
# Map NA to -3
cleaned <- cleaned %>%
  mutate(bmi25 = case_when(
    is.na(W8DBMI) ~ -3,
    TRUE ~ as.numeric(W8DBMI)
  ))

# Create bmi32 from W9DBMI (Wave 9, Age 32)
# W9DBMI has user_missing_values: -1 thru -8 and -9
# Labels: -9 = Refused, -8 = Insufficient information, -1 = Not applicable
# Map NA to -3
cleaned <- cleaned %>%
  mutate(bmi32 = case_when(
    is.na(W9DBMI) ~ -3,
    TRUE ~ as.numeric(W9DBMI)
  ))

# Keep only NSID and the two BMI variables
output <- cleaned %>%
  select(NSID, bmi25, bmi32)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

# Print summary
print(paste("Output dimensions:", nrow(output), "rows,", ncol(output), "columns"))
print(head(output))
print(summary(output$bmi25))
print(summary(output$bmi32))

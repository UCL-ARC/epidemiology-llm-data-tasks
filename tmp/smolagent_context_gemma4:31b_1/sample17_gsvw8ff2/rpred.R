library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
file1 <- "data/input/wave_one_lsype_young_person_2020.tab"
file2 <- "data/input/wave_two_lsype_family_background_2020.tab"
file3 <- "data/input/wave_three_lsype_family_background_2020.tab"
file4 <- "data/input/wave_four_lsype_young_person_2020.tab"
file5 <- "data/input/ns9_2022_derived_variables.tab"

data1 <- read_delim(file1, delim = "\t", col_types = cols(.default = "c"))
data2 <- read_delim(file2, delim = "\t", col_types = cols(.default = "c"))
data3 <- read_delim(file3, delim = "\t", col_types = cols(.default = "c"))
data4 <- read_delim(file4, delim = "\t", col_types = cols(.default = "c"))
data5 <- read_delim(file5, delim = "\t", col_types = cols(.default = "c"))

# 2, 4. Variable Selection and Renaming
df1 <- data1 %>% select(NSID)
df2 <- data2 %>% select(NSID, imd15 = IMDRSCORE)
df3 <- data3 %>% select(NSID, imd16 = IMDRSCORE)
df4 <- data4 %>% select(NSID)
df5 <- data5 %>% select(NSID, imd32 = W9DIMDD)

merged_data <- df1 %>%
  full_join(df2, by = "NSID") %>%
  full_join(df3, by = "NSID") %>%
  full_join(df4, by = "NSID") %>%
  full_join(df5, by = "NSID")

merged_data <- merged_data %>%
  mutate(across(starts_with("imd"), as.numeric))

# 5. Missing Value Code Harmonisation
harmonise_missing <- function(x) {
  x <- ifelse(!is.na(x) & x == -94, -8, x)
  x <- ifelse(is.na(x), -3, x)
  return(x)
}

merged_data <- merged_data %>%
  mutate(across(starts_with("imd"), harmonise_missing))

# 8. Variable Labelling
# The error was caused by passing character names in the named vector to set_value_labels.
# We must ensure the names of the vector are converted to the numeric type of the variable.
val_labels <- c(
  "Refusal" = -9,
  "Don’t know / insufficient information" = -8,
  "Not asked at the fieldwork stage / not interviewed" = -3,
  "Script error / information lost" = -2,
  "Not applicable" = -1
)

# set_value_labels expects a named vector where names are the labels and values are the codes
merged_data <- merged_data %>%
  mutate(imd15 = set_value_labels(imd15, val_labels), 
         imd16 = set_value_labels(imd16, val_labels), 
         imd32 = set_value_labels(imd32, val_labels))

# 11. Final variable selection
final_data <- merged_data %>%
  select(NSID, imd15, imd16, imd32)

# 10. Output Requirements
write_csv(final_data, "data/output/cleaned_data.csv")

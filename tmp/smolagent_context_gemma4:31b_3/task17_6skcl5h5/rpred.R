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

# Variable selection and renaming
df15 <- data2 %>%
  select(NSID, imd15 = IMDRSCORE) %>%
  mutate(imd15 = as.numeric(imd15))

df16 <- data3 %>%
  select(NSID, imd16 = IMDRSCORE) %>%
  mutate(imd16 = as.numeric(imd16))

df32 <- data5 %>%
  select(NSID, imd32 = W9DIMDD) %>%
  mutate(imd32 = as.numeric(imd32))

final_df <- df15 %>%
  full_join(df16, by = "NSID") %>%
  full_join(df32, by = "NSID")

# 5. Missing Value Code Harmonisation
harmonise_missing <- function(x) {
  x <- ifelse(x == -94, -8, x)
  x[is.na(x)] <- -3
  return(x)
}

final_df <- final_df %>%
  mutate(across(starts_with("imd"), harmonise_missing))

# 8. Variable Labelling
# To avoid the vec_cast_named error, we create a named vector where the values
# are explicitly numeric. The labels should be the names of the vector.

# Standard missing-value codes mapping
missing_codes <- c(-9, -8, -3, -2, -1)
missing_labels_vals <- c(
  "Refusal",
  "Don’t know / insufficient information",
  "Not asked at the fieldwork stage / not interviewed",
  "Script error / information lost",
  "Not applicable"
)
names(missing_labels_vals) <- as.character(missing_codes)

# Use a loop or map to apply labels to avoid across() issues with labelled vectors
# We must use the labels in a way that haven/labelled expects: 
# a named vector where names are the labels and values are the codes, 
# OR use set_value_labels correctly.

# Correct way for set_value_labels is to provide a named vector where names are labels
# and values are the codes.
val_labels <- c(
  "Refusal" = -9,
  "Don’t know / insufficient information" = -8,
  "Not asked at the fieldwork stage / not interviewed" = -3,
  "Script error / information lost" = -2,
  "Not applicable" = -1
)

for (col in names(final_df)) {
  if (grepl("^imd", col)) {
    final_df[[col]] <- set_value_labels(final_df[[col]], val_labels)
  }
}

# 11. Final Variable Selection
final_df <- final_df %>%
  select(NSID, imd15, imd16, imd32)

# 10. Output Requirements
write_csv(final_df, "data/output/cleaned_data.csv")
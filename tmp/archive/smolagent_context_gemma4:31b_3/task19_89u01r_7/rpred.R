library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
file1 <- "data/input/wave_one_lsype_young_person_2020.tab"
file4 <- "data/input/wave_four_lsype_young_person_2020.tab"
file8 <- "data/input/ns8_2015_derived.tab"
file9 <- "data/input/ns9_2022_derived_variables.tab"

data1 <- read_delim(file1, delim = "\t", col_types = readr::cols(.default = "c"))
data4 <- read_delim(file4, delim = "\t", col_types = readr::cols(.default = "c"))
data8 <- read_delim(file8, delim = "\t", col_types = readr::cols(.default = "c"))
data9 <- read_delim(file9, delim = "\t", col_types = readr::cols(.default = "c"))

data1 <- data1 %>% mutate(NSID = as.character(NSID))
data4 <- data4 %>% mutate(NSID = as.character(NSID))
data8 <- data8 %>% mutate(NSID = as.character(NSID))
data9 <- data9 %>% mutate(NSID = as.character(NSID))

full_cohort <- data1 %>%
  full_join(data4, by = "NSID") %>%
  full_join(data8, by = "NSID") %>%
  full_join(data9, by = "NSID")

# 2. Variable Harmonization Function
harmonize_bmi <- function(x) {
  val <- as.numeric(x)
  res <- case_when(
    !is.na(val) & val > 0 ~ val,
    !is.na(val) & val == -9 ~ -9,
    !is.na(val) & val == -8 ~ -8,
    !is.na(val) & val == -1 ~ -1,
    TRUE ~ -3
  )
  return(res)
}

# 3. Process BMI variables
cleaned_data <- full_cohort %>%
  mutate(
    bmi25 = harmonize_bmi(W8DBMI),
    bmi32 = harmonize_bmi(W9DBMI)
  ) %>%
  select(NSID, bmi25, bmi32)

# 4. Apply Labels
# The previous error occurred because labelled() expects the names of the label vector 
# to be the same type as the data (numeric), not characters.
missing_labels <- c(
  "Refusal" = -9,
  "Don't know/insufficient information" = -8,
  "Item not applicable" = -1,
  "Not asked/interviewed" = -3,
  "Script error/lost" = -2
)

# Note: labelled() takes a named vector where names are labels and values are the codes.
# However, the standard way in the labelled package is to provide labels as names 
# and the values as the vector elements, but the mapping must match types.
# Let's use the correct named vector format for labelled(): 
# The values in the vector are the codes, and the names are the labels.

cleaned_data$bmi25 <- labelled::labelled(cleaned_data$bmi25, missing_labels)
cleaned_data$bmi32 <- labelled::labelled(cleaned_data$bmi32, missing_labels)

# Apply variable labels
var_label(cleaned_data$bmi25) <- "BMI at age 25"
var_label(cleaned_data$bmi32) <- "BMI at age 32"

# 5. Output
write_csv(cleaned_data, "data/output/cleaned_data.csv")
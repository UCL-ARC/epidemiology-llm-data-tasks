library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
path <- "data/input/"

# Using a list to load files to avoid repetitive code and potential syntax errors in large blobs
files_to_load <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_derived_variables.tab"
)

# Read all files as strings first to be safe, then convert specific columns
data_list <- map(files_to_load, ~read_delim(paste0(path, .x), delim = "\t", col_types = cols(.default = "c")))
names(data_list) <- files_to_load

# Merge all datasets using full_join by NSID
cohort_frame <- data_list[[1]]
for (i in 2:length(data_list)) {
  cohort_frame <- full_join(cohort_frame, data_list[[i]], by = "NSID")
}

# 2. Target Variable Identification and Harmonisation
# Convert income variables to numeric for processing
cohort_frame <- cohort_frame %>%
  mutate(
    W8DINCB = as.numeric(W8DINCB),
    W9DINCB = as.numeric(W9DINCB)
  )

# Define missing value mapping function
process_income <- function(x) {
  # NA -> -3 (Not asked)
  # -1.0 (from metadata) -> -1 (Not applicable)
  res <- ifelse(is.na(x), -3, x)
  return(res)
}

# Create inc25 and inc32
cohort_frame <- cohort_frame %>%
  mutate(
    inc25 = process_income(W8DINCB),
    inc32 = process_income(W9DINCB)
  )

# 3. Labeling
# Ordered based on metadata
income_labels <- c(
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-1" = "Not applicable",
  "1" = "less than 25",
  "2" = "25 to 50",
  "3" = "50 to 90",
  "4" = "90 to 140",
  "5" = "140 to 240",
  "6" = "240 to 300",
  "7" = "300 to 350",
  "8" = "350 to 400",
  "9" = "400 to 500",
  "10" = "500 to 600",
  "11" = "600 to 700",
  "12" = "700 to 800",
  "13" = "800 to 900",
  "14" = "900 to 1200",
  "15" = "1200 to 1400",
  "16" = "more than 1400"
)

# Convert to factors with explicit levels and labels
levs <- as.numeric(names(income_labels))
cohort_frame$inc25 <- factor(cohort_frame$inc25, levels = levs, labels = income_labels)
cohort_frame$inc32 <- factor(cohort_frame$inc32, levels = levs, labels = income_labels)

# 4. Final selection and Output
final_data <- cohort_frame %>%
  select(NSID, inc25, inc32)

readr::write_csv(final_data, "data/output/cleaned_data.csv")
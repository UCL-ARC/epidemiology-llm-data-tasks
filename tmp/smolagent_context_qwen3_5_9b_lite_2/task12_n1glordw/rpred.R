library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

setwd("/home/jovyan/rdss-volume/tmp/smolagent_context_qwen3_5_9b_1/task12_n1glordw")

files <- list(
  wave1 = "data/input/wave_one_lsype_young_person_2020.tab",
  wave4 = "data/input/wave_four_lsype_young_person_2020.tab",
  wave5 = "data/input/wave_five_lsype_young_person_2020.tab",
  wave6 = "data/input/wave_six_lsype_young_person_2020.tab",
  wave7 = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave8 = "data/input/ns8_2015_derived.tab",
  wave9 = "data/input/ns9_2022_main_interview.tab"
)

wave1_data <- read_delim(files$wave1, delim = "\t")
wave4_data <- read_delim(files$wave4, delim = "\t")
wave5_data <- read_delim(files$wave5, delim = "\t")
wave6_data <- read_delim(files$wave6, delim = "\t")
wave7_data <- read_delim(files$wave7, delim = "\t")
wave8_data <- read_delim(files$wave8, delim = "\t")
wave9_data <- read_delim(files$wave9, delim = "\t")

all_data <- wave1_data
all_data <- full_join(all_data, wave4_data, by = "NSID")
all_data <- full_join(all_data, wave5_data, by = "NSID")
all_data <- full_join(all_data, wave6_data, by = "NSID")
all_data <- full_join(all_data, wave7_data, by = "NSID")
all_data <- full_join(all_data, wave8_data, by = "NSID")
all_data <- full_join(all_data, wave9_data, by = "NSID")

nssec_clean_and_collapse <- function(x) {
  # First convert missing values
  x <- x
  x[x == -9 | x == -99 | x == -91 | x == -991] <- -3
  x[x == -8 | x == -998] <- -8
  x[x == -1 | x == -991 | x == -91] <- -1
  
  # Collapse to major categories
  # For decimal categories like 3.1, 3.2, etc., extract the integer part
  x <- as.numeric(x)
  x[!is.na(x) & x >= 1 & x <= 17] <- floor(x[!is.na(x) & x >= 1 & x <= 17])
  
  return(x)
}

wave_vars <- list(
  nssec17 = "W4nsseccatYP",
  nssec18 = "W5nsseccatYP",
  nssec19 = "w6nsseccatYP",
  nssec20 = "W7NSSECCat",
  nssec25 = "W8DNSSEC17",
  nssec32 = "W9NSSEC"
)

# Define labels for major NS-SEC categories
nssec_labels <- c(
  -3 = "Refused",
  -8 = "Insufficient information",
  -1 = "Not applicable",
  1 = "Employers in large organisations",
  2 = "Higher managerial occupations",
  3 = "Higher professional occupations",
  4 = "Lower professional occupations",
  5 = "Lower managerial occupations",
  6 = "Higher supervisory occupations",
  7 = "Intermediate occupations",
  8 = "Employers in small establishments",
  9 = "Own account workers",
  10 = "Lower supervisory occupations",
  11 = "Lower technical occupations",
  12 = "Semi-routine occupations",
  13 = "Routine occupations",
  14 = "Never worked and Long-term unemployed",
  15 = "Full-time students",
  16 = "Occupations not stated or inadequately described",
  17 = "Not classifiable for other reasons"
)

for (var_name in names(wave_vars)) {
  source_var <- wave_vars[[var_name]]
  all_data[[var_name]] <- as.numeric(all_data[[source_var]])
  all_data[[var_name]] <- nssec_clean_and_collapse(all_data[[var_name]])
  
  # Create labelled factor
  all_data[[var_name]] <- factor(all_data[[var_name]], levels = sort(c(unique(all_data[[var_name]][!is.na(all_data[[var_name]])), -3, -8, -1)]), labels = names(nssec_labels))
}

source_var_list <- c("W4nsseccatYP", "W5nsseccatYP", "w6nsseccatYP", "W7NSSECCat", "W8DNSSEC17", "W9NSSEC")
all_data <- all_data %>% select(!all_of(source_var_list))

dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(all_data, "data/output/cleaned_data.csv")

cat("Done\n")
print(paste("Rows:", nrow(all_data)))
print(paste("Columns:", ncol(all_data)))
print(paste("Variables:", paste(names(all_data), collapse = ", ")))
}
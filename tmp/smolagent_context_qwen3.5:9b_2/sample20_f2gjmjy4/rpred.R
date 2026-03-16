library(haven)
library(dplyr)
library(readr)

# Define file paths
files <- list(
  "wave_one_lsype_young_person_2020.tab" = "data/input/wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab" = "data/input/wave_two_lsype_young_person_2020.tab",
  "wave_three_lsype_young_person_2020.tab" = "data/input/wave_three_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab" = "data/input/wave_four_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab" = "data/input/wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab" = "data/input/wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_self_completion.tab" = "data/input/ns8_2015_self_completion.tab",
  "ns9_2022_main_interview.tab" = "data/input/ns9_2022_main_interview.tab"
)

# Load each file as tab-delimited
w1 <- read_delim(files[["wave_one_lsype_young_person_2020.tab"]], delim = "\\t", col_types = cols(.default = "c"))
w2 <- read_delim(files[["wave_two_lsype_young_person_2020.tab"]], delim = "\\t", col_types = cols(.default = "c"))
w3 <- read_delim(files[["wave_three_lsype_young_person_2020.tab"]], delim = "\\t", col_types = cols(.default = "c"))
w4 <- read_delim(files[["wave_four_lsype_young_person_2020.tab"]], delim = "\\t", col_types = cols(.default = "c"))
w6 <- read_delim(files[["wave_six_lsype_young_person_2020.tab"]], delim = "\\t", col_types = cols(.default = "c"))
w7 <- read_delim(files[["wave_seven_lsype_young_person_2020.tab"]], delim = "\\t", col_types = cols(.default = "c"))
w8 <- read_delim(files[["ns8_2015_self_completion.tab"]], delim = "\\t", col_types = cols(.default = "c"))
w9 <- read_delim(files[["ns9_2022_main_interview.tab"]], delim = "\\t", col_types = cols(.default = "c"))

# Check column names in each file
cat("Column names in w1:\n")
print(names(w1)[1:5])
cat("Column names in w2:\n")
print(names(w2)[1:5])
cat("Column names in w3:\n")
print(names(w3)[1:5])
cat("Column names in w4:\n")
print(names(w4)[1:5])
cat("Column names in w6:\n")
print(names(w6)[1:5])
cat("Column names in w7:\n")
print(names(w7)[1:5])
cat("Column names in w8:\n")
print(names(w8)[1:5])
cat("Column names in w9:\n")
print(names(w9)[1:5])

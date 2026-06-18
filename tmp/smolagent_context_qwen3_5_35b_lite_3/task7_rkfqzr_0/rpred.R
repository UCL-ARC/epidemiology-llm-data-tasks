# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_main_interview.tab",
  "ns9_2022_main_interview.tab"
)

# Load all files
data_files <- files %>% 
  map(~read_delim(paste0("data/input/", .x), delim = "\t", show_col_types = FALSE))

names(data_files) <- files

# Merge all datasets by NSID
df <- reduce(data_files, full_join, by = "NSID")

cat("Merged dataset dimensions:", dim(df), "\n")

# Create function to recode educational aims to 5-level NVQ scheme
recodes_educational_aims <- function(var) {
  case_when(
    var %in% c(1, 5, 10, 13) ~ 5,  # NVQ 5, First/Other Degree, Other HE, NVQ 4
    var %in% c(3, 6, 7, 8) ~ 3,    # NVQ 3, AVCE, A/AS, Other level 3
    var %in% c(5, 9, 7) ~ 2,       # NVQ 2, Intermediate GNVQ, Other level 2
    var %in% c(9, 10, 11) ~ 1,     # NVQ 1, Foundation, Other level 1
    var == 8 ~ 0,                  # GCSE
    var %in% c(12, 14, 15, 16) ~ -1,  # Not studying, Other, No detail, Other (level unknown)
    TRUE ~ -1
  )
}

# Process Wave 4 (Age 17) - w4saim
df$educaim17 <- recodes_educational_aims(df$w4saim)

# Process Wave 6 (Age 19) - W6Saim
df$educaim19 <- recodes_educational_aims(df$W6Saim)

# Process Wave 7 (Age 20) - W7SAim
df$educaim20 <- recodes_educational_aims(df$W7SAim)

cat("educaim17 range:", range(df$educaim17, na.rm=TRUE), "\n")
cat("educaim19 range:", range(df$educaim19, na.rm=TRUE), "\n")
cat("educaim20 range:", range(df$educaim20, na.rm=TRUE), "\n")

# Derive educaim25 from Wave 8 qualification data
df$educaim25 <- case_when(
  # Check if studying for higher qualifications (NVQ 5)
  !is.na(df$W8ACTIVITY05) & df$W8ACTIVITY05 == 1 & 
    (df$W8ACQUC0A == 1 | df$W8ACQUC0B == 1 | df$W8VCQUC0J == 1 | df$W8VCQUC0K == 1) ~ 5,
  # NVQ 3 level (A Level, HND/HNC, NVQ 3-5)
  !is.na(df$W8ACTIVITY05) & df$W8ACTIVITY05 == 1 & 
    (df$W8ACQUC0F == 1 | df$W8VCQUC0J == 1 | df$W8VCQUC0K == 1) ~ 3,
  # NVQ 2 level
  !is.na(df$W8ACTIVITY05) & df$W8ACTIVITY05 == 1 & 
    (df$W8ACQUC0G == 1 | df$W8ACQUC0H == 1 | df$W8ACQUC0I == 1 | df$W8VCQUC0E == 1) ~ 2,
  # NVQ 1 level
  !is.na(df$W8ACTIVITY05) & df$W8ACTIVITY05 == 1 & 
    (df$W8ACQUC0L == 1 | df$W8ACQUC0M == 1 | df$W8VCQUC0A == 1 | df$W8VCQUC0B == 1) ~ 1,
  # GCSE level
  !is.na(df$W8ACTIVITY05) & df$W8ACTIVITY05 == 1 & 
    df$W8ACQUC0L == 1 ~ 0,
  # Not in education or not applicable
  TRUE ~ -1
)

# Derive educaim32 from Wave 9 qualification data
df$educaim32 <- case_when(
  # Check if in education
  !is.na(df$W9ECONACT2) & df$W9ECONACT2 %in% c(6, 7, 12) &
    # NVQ 5 (Doctorate, Masters, Undergraduate, HE Diplomas)
    (df$W9ACQUC0A == 1 | df$W9ACQUC0B == 1 | df$W9ACQUC0C == 1 | df$W9ACQUC0E == 1 | 
     df$W9VCQUC0A == 1 | df$W9VCQUC0C == 1) ~ 5,
  # NVQ 3 level (A/AS Level, Level 3 vocational)
  !is.na(df$W9ECONACT2) & df$W9ECONACT2 %in% c(6, 7, 12) &
    (df$W9ACQUC0G == 1 | df$W9VCQUC0D == 1 | df$W9VCQUC0G == 1 | df$W9VCQUC0O == 1) ~ 3,
  # NVQ 2 level
  !is.na(df$W9ECONACT2) & df$W9ECONACT2 %in% c(6, 7, 12) &
    (df$W9ACQUC0H == 1 | df$W9VCQUC0E == 1 | df$W9VCQUC0H == 1 | df$W9VCQUC0J == 1) ~ 2,
  # NVQ 1 level
  !is.na(df$W9ECONACT2) & df$W9ECONACT2 %in% c(6, 7, 12) &
    (df$W9ACQUC0I == 1 | df$W9VCQUC0F == 1 | df$W9VCQUC0K == 1 | df$W9VCQUC0L == 1) ~ 1,
  # GCSE
  !is.na(df$W9ECONACT2) & df$W9ECONACT2 %in% c(6, 7, 12) &
    df$W9ACQUC0H == 1 ~ 0,
  # Not in education or not applicable
  TRUE ~ -1
)

cat("educaim25 range:", range(df$educaim25, na.rm=TRUE), "\n")
cat("educaim32 range:", range(df$educaim32, na.rm=TRUE), "\n")

# Create labelled vectors using labelled::labelled() with numeric labels
label_values <- labelled::labelled(
  c("Not asked at fieldwork stage" = -3, "Not applicable" = -1, "GCSE" = 0, "NVQ 1" = 1, "NVQ 2" = 2, "NVQ 3" = 3, "NVQ 5" = 5)
)

# Apply labels to all educaim variables
for (var in c("educaim17", "educaim19", "educaim20", "educaim25", "educaim32")) {
  df[[var]] <- labelled::labelled(df[[var]], label_values)
}

# Select only ID and final derived variables
output_df <- df %>% select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

cat("\nFinal dataset dimensions:", dim(output_df), "\n")
cat("Variables:", names(output_df), "\n")

# Write to CSV
write_csv(output_df, "data/output/cleaned_data.csv")

cat("\nOutput written to data/output/cleaned_data.csv\n")

# Summary
output_df %>% 
  select(-NSID) %>% 
  summarise_all(list(~sum(!is.na(.)), ~mean(. == -1, na.rm=TRUE), ~mean(. >= 0, na.rm=TRUE))) %>%
  print()

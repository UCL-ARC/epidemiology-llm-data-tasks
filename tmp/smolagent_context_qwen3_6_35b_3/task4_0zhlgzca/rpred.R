library(dplyr)
library(readr)
library(haven)
library(labelled)
library(tidyr)
library(purrr)

# Load all files from metadata
files_to_load <- c(
  "data/input/wave_one_lsype_young_person_2020.tab",
  "data/input/wave_four_lsype_young_person_2020.tab",
  "data/input/wave_six_lsype_young_person_2020.tab",
  "data/input/wave_seven_lsype_young_person_2020.tab",
  "data/input/ns8_2015_self_completion.tab",
  "data/input/ns9_2022_main_interview.tab"
)

# Load each file
df_w1 <- read_delim(files_to_load[1], delim = "\t", show_col_types = FALSE)
df_w4 <- read_delim(files_to_load[2], delim = "\t", show_col_types = FALSE)
df_w6 <- read_delim(files_to_load[3], delim = "\t", show_col_types = FALSE)
df_w7 <- read_delim(files_to_load[4], delim = "\t", show_col_types = FALSE)
df_w8 <- read_delim(files_to_load[5], delim = "\t", show_col_types = FALSE)
df_w9 <- read_delim(files_to_load[6], delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID using full_join
df_merged <- df_w1 %>%
  full_join(df_w4, by = "NSID") %>%
  full_join(df_w6, by = "NSID") %>%
  full_join(df_w7, by = "NSID") %>%
  full_join(df_w8, by = "NSID") %>%
  full_join(df_w9, by = "NSID")

cat("Merged dataset dimensions:", dim(df_merged), "\n")

# Derive sori19 from W6SexualityYP (wave6, age 19)
df_merged <- df_merged %>%
  mutate(sori19 = case_when(
    W6SexualityYP == -97 ~ -9,
    W6SexualityYP == -92 ~ -9,
    W6SexualityYP == -91 ~ -1,
    W6SexualityYP == -1 ~ -8,
    W6SexualityYP %in% c(1, 2, 3, 4) ~ as.numeric(W6SexualityYP),
    is.na(W6SexualityYP) ~ -3,
    TRUE ~ -3
  ))

# Derive sori20 from W7SexualityYP (wave7, age 20)
df_merged <- df_merged %>%
  mutate(sori20 = case_when(
    W7SexualityYP == -100 ~ -9,
    W7SexualityYP == -97 ~ -9,
    W7SexualityYP == -92 ~ -9,
    W7SexualityYP == -91 ~ -1,
    W7SexualityYP == -1 ~ -8,
    W7SexualityYP %in% c(1, 2, 3, 4) ~ as.numeric(W7SexualityYP),
    is.na(W7SexualityYP) ~ -3,
    TRUE ~ -3
  ))

# Derive sori25 from W8SEXUALITY (wave8, age 25)
df_merged <- df_merged %>%
  mutate(sori25 = case_when(
    W8SEXUALITY == -9 ~ -9,
    W8SEXUALITY == -8 ~ -8,
    W8SEXUALITY == -1 ~ -1,
    W8SEXUALITY %in% c(1, 2, 3, 4) ~ as.numeric(W8SEXUALITY),
    is.na(W8SEXUALITY) ~ -3,
    TRUE ~ -3
  ))

# Derive sori32 from W9SORI (wave9, age 32)
df_merged <- df_merged %>%
  mutate(sori32 = case_when(
    W9SORI == -9 ~ -9,
    W9SORI == -8 ~ -8,
    W9SORI == -3 ~ -3,
    W9SORI == -1 ~ -1,
    W9SORI == 5 ~ -7,
    W9SORI %in% c(1, 2, 3, 4) ~ as.numeric(W9SORI),
    is.na(W9SORI) ~ -3,
    TRUE ~ -3
  ))

# Keep only NSID and the derived variables
df_output <- df_merged %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Set labels using haven functions
for (var in c("sori19", "sori20", "sori25", "sori32")) {
  attr(df_output[[var]], "label") <- paste("Sexual orientation at age", gsub("sori", "", var))
  value_labels <- c(`Heterosexual/straight` = 1, `Gay/lesbian` = 2, `Bisexual` = 3, `Other` = 4, 
                    `Item not applicable` = -1, `Not asked at fieldwork stage` = -3, 
                    `Prefer not to say` = -7, `Don't know` = -8, `Refusal` = -9)
  attr(df_output[[var]], "labels") <- value_labels
  class(df_output[[var]]) <- c("haven_labelled", "vctrs_vctr", "double")
}

# Write output
dir.create("data/output", showWarnings = FALSE)
write_csv(df_output, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Output dimensions:", dim(df_output), "\n")
cat("\nSample of output:\n")
print(head(df_output))

# Summary of each variable
cat("\nSummary of sori19:\n")
print(table(df_output$sori19, useNA = "ifany"))
cat("\nSummary of sori20:\n")
print(table(df_output$sori20, useNA = "ifany"))
cat("\nSummary of sori25:\n")
print(table(df_output$sori25, useNA = "ifany"))
cat("\nSummary of sori32:\n")
print(table(df_output$sori32, useNA = "ifany"))

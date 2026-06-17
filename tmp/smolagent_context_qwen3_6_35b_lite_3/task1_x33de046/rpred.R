library(dplyr)
library(readr)
library(tidyr)
library(labelled)
library(haven)

# Define file paths
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab",
  "wave_three_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_five_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_main_interview.tab",
  "ns9_2022_main_interview.tab"
)

# Load all files
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
w9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Extract NSID and sex variable from each file, then merge
df1 <- w1 %>% select(NSID, W1sexYP)
df2 <- w2 %>% select(NSID, W2SexYP)
df3 <- w3 %>% select(NSID, W3sexYP)
df4 <- w4 %>% select(NSID, W4SexYP)
df5 <- w5 %>% select(NSID, W5SexYP)
df6 <- w6 %>% select(NSID, W6Sex)
df7 <- w7 %>% select(NSID, W7Sex)
df8 <- w8 %>% select(NSID, W8CMSEX)
df9 <- w9 %>% select(NSID, W9DSEX)

# Merge all datasets
df <- df1
df <- full_join(df, df2, by = "NSID")
df <- full_join(df, df3, by = "NSID")
df <- full_join(df, df4, by = "NSID")
df <- full_join(df, df5, by = "NSID")
df <- full_join(df, df6, by = "NSID")
df <- full_join(df, df7, by = "NSID")
df <- full_join(df, df8, by = "NSID")
df <- full_join(df, df9, by = "NSID")

# Function to harmonize sex variables
harmonize_sex <- function(x) {
  recode(x,
    "-999" = -2,
    "-998" = -2,
    "-997" = -2,
    "-995" = -2,
    "-99" = -3,
    "-92" = -9,
    "-91" = -1,
    "-10" = -1,
    "-9" = -9,
    "-8" = -8,
    "-1" = -8,
    .default = x
  )
}

# Harmonize each wave's sex variable
df <- df %>%
  mutate(
    W1_sex = harmonize_sex(W1sexYP),
    W2_sex = harmonize_sex(W2SexYP),
    W3_sex = harmonize_sex(W3sexYP),
    W4_sex = harmonize_sex(W4SexYP),
    W5_sex = harmonize_sex(W5SexYP),
    W6_sex = harmonize_sex(W6Sex),
    W7_sex = harmonize_sex(W7Sex),
    W8_sex = harmonize_sex(W8CMSEX),
    W9_sex = harmonize_sex(W9DSEX)
  )

# Consolidate sex variable using most-recent-valid-first
df <- df %>%
  mutate(
    sex = case_when(
      !is.na(W9_sex) & W9_sex %in% c(1, 2) ~ W9_sex,
      !is.na(W8_sex) & W8_sex %in% c(1, 2) ~ W8_sex,
      !is.na(W7_sex) & W7_sex %in% c(1, 2) ~ W7_sex,
      !is.na(W6_sex) & W6_sex %in% c(1, 2) ~ W6_sex,
      !is.na(W5_sex) & W5_sex %in% c(1, 2) ~ W5_sex,
      !is.na(W4_sex) & W4_sex %in% c(1, 2) ~ W4_sex,
      !is.na(W3_sex) & W3_sex %in% c(1, 2) ~ W3_sex,
      !is.na(W2_sex) & W2_sex %in% c(1, 2) ~ W2_sex,
      !is.na(W1_sex) & W1_sex %in% c(1, 2) ~ W1_sex,
      TRUE ~ NA_real_
    )
  )

# If no valid sex was found, assign -3
df <- df %>%
  mutate(
    sex = ifelse(is.na(sex), -3, sex)
  )

# Create labelled vector (numeric, not factor)
sex_labelled <- labelled::labelled(df$sex, c(
  "Male" = 1,
  "Female" = 2,
  "Not asked at fieldwork / not interviewed" = -3,
  "Don't know / insufficient information" = -8,
  "Refusal" = -9,
  "Item not applicable" = -1
))
df$sex <- sex_labelled

# Keep only NSID and sex
df <- df %>%
  select(NSID, sex)

# Write output
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(df, "data/output/cleaned_data.csv")

cat("Done. Output written to data/output/cleaned_data.csv\n")
print(head(df))
print(summary(df$sex))
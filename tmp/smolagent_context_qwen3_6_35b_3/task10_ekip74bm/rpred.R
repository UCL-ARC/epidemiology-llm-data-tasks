library(dplyr)
library(readr)
library(haven)
library(labelled)

# Load all files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all by NSID
df <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Mapping functions for collapsed 6-category scheme
code_w4 <- function(x) {
  x[is.na(x)] <- NA_real_
  x[x == -999] <- -2
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x %in% c(1, 2)] <- 1
  x[x == 3] <- 4
  x[x == 4] <- 2
  x[x == 5] <- 3
  x[x == 6] <- 5
  x[x %in% c(7, 8, 9)] <- 6
  return(x)
}

code_w5 <- function(x) {
  x[is.na(x)] <- NA_real_
  x[x == -94] <- -8
  x[x %in% c(1, 2, 5, 6)] <- 2
  x[x == 3] <- 1
  x[x == 4] <- 3
  x[x == 7] <- 4
  x[x == 8] <- 5
  x[x %in% c(9, 10, 11)] <- 6
  return(x)
}

code_w6 <- function(x) {
  x[is.na(x)] <- NA_real_
  x[x == -91] <- -1
  x[x %in% c(1, 2)] <- 3
  x[x == 3] <- 1
  x[x %in% c(4, 5, 10)] <- 2
  x[x == 6] <- 6
  x[x == 7] <- 5
  x[x == 8] <- 4
  x[x == 9] <- 6
  x[x == 11] <- 6
  return(x)
}

code_w7 <- function(x) {
  x[is.na(x)] <- NA_real_
  x[x == -91] <- -1
  x[x %in% c(1, 2)] <- 3
  x[x == 3] <- 1
  x[x %in% c(4, 5, 9, 11)] <- 2
  x[x == 6] <- 6
  x[x == 7] <- 5
  x[x == 8] <- 4
  x[x %in% c(10, 12, 13, 14, 15)] <- 6
  return(x)
}

code_w8 <- function(x) {
  x[is.na(x)] <- NA_real_
  x[x == -9] <- -9
  x[x == -8] <- -8
  x[x == -1] <- -1
  x[x %in% c(1, 2)] <- 1
  x[x %in% c(6, 7)] <- 2
  x[x == 3] <- 6
  x[x == 4] <- 4
  x[x == 5] <- 3
  x[x == 8] <- 6
  x[x == 9] <- 5
  x[x == 10] <- 6
  return(x)
}

code_w9 <- function(x) {
  x[is.na(x)] <- NA_real_
  x[x == -9] <- -9
  x[x == -8] <- -8
  x[x == -1] <- -1
  x[x %in% c(1, 2)] <- 1
  x[x %in% c(6, 7)] <- 2
  x[x == 3] <- 6
  x[x == 4] <- 4
  x[x == 5] <- 3
  x[x == 8] <- 6
  x[x == 9] <- 5
  x[x == 10] <- 6
  return(x)
}

# Create collapsed variables
df <- df %>%
  mutate(
    ecoact17 = code_w4(W4empsYP),
    ecoact18 = code_w5(W5mainactYP),
    ecoact19 = code_w6(W6TCurrentAct),
    ecoact20 = code_w7(W7TCurrentAct),
    ecoact25 = code_w8(W8DACTIVITYC),
    ecoact32 = code_w9(W9DACTIVITYC)
  )

# Create detailed variables for age 25 and 32
df <- df %>%
  mutate(
    ecoactadu25 = W8DACTIVITYC,
    ecoactadu32 = W9DACTIVITYC
  )

# Define labels for collapsed variables - character names with numeric values (as required by haven::labelled)
collapse_labels <- c(
  `In paid work` = 1,
  `Apprenticeship / government training scheme / training` = 2,
  `Education` = 3,
  `Unemployed` = 4,
  `Looking after home / family` = 5,
  `Other (including voluntary work, sick/disabled, waiting for course, travelling, and other residual categories)` = 6,
  `Not applicable` = -1,
  `Schedule not applicable / script error / information lost` = -2,
  `Not asked at the fieldwork stage / not interviewed` = -3,
  `Prefer not to say` = -7,
  `Don\'t know / insufficient information` = -8,
  `Refusal` = -9
)

# Define labels for detailed variables
detail_labels <- c(
  `Employee - in paid work` = 1,
  `Self employed` = 2,
  `In unpaid/voluntary work` = 3,
  `Unemployed` = 4,
  `Education: School/college/university` = 5,
  `Apprenticeship` = 6,
  `On gov\'t scheme for employment training` = 7,
  `Sick or disabled` = 8,
  `Looking after home or family` = 9,
  `Something else` = 10,
  `Not applicable` = -1,
  `Insufficient information` = -8,
  `Refused` = -9
)

# Apply labels using labelled package
for (var in c("ecoact17", "ecoact18", "ecoact19", "ecoact20", "ecoact25", "ecoact32")) {
  df[[var]] <- labelled::set_value_labels(df[[var]], collapse_labels)
  df[[var]] <- haven::as_factor(df[[var]])
}

for (var in c("ecoactadu25", "ecoactadu32")) {
  df[[var]] <- labelled::set_value_labels(df[[var]], detail_labels)
  df[[var]] <- haven::as_factor(df[[var]])
}

# Keep only NSID and derived variables
df_out <- df %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Ensure output directory exists
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Write output
write_csv(df_out, "data/output/cleaned_data.csv")

cat("Done! Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(df_out), "\n")
cat("Columns:", paste(names(df_out), collapse = ", "), "\n")

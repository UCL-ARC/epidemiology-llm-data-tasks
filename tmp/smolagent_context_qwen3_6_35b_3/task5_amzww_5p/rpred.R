library(dplyr)
library(readr)
library(labelled)
library(tidyr)
library(purrr)

# Load all files from metadata
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all files by NSID using full_join
cleaned <- full_join(wave1, wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

cat("Merged dataset dimensions:", dim(cleaned), "\n")

# ============================================================
# Helper function to convert missing codes to standard scheme
# ============================================================
convert_missing <- function(x) {
  # Replace -999, -998, -997, -995 with -2 (schedule not applicable / script error / info lost)
  x[x == -999 | x == -998 | x == -997 | x == -995] <- -2
  # Replace -92 with -9 (refused)
  x[x == -92] <- -9
  # Replace -91 with -1 (not applicable)
  x[x == -91] <- -1
  # Replace -99 with -3 (not asked)
  x[x == -99] <- -3
  # Replace -97 (respondent declined) with -7 (prefer not to say)
  x[x == -97] <- -7
  # Replace -1 (don't know) with -8
  x[x == -1] <- -8
  # Replace any remaining NAs with -3 (not asked / not interviewed)
  x[is.na(x)] <- -3
  return(x)
}

# ============================================================
# 1. partnr19 from W6MarStatYP (Wave 6, Age 19)
# ============================================================
partnr19_df <- wave6 %>%
  select(NSID, W6MarStatYP) %>%
  mutate(partnr19 = convert_missing(W6MarStatYP)) %>%
  mutate(partnr19 = factor(partnr19, 
                           levels = c(1, 2, 3, 4, 5, -1, -2, -3, -7, -8, -9),
                           labels = c("Single (never married)",
                                      "Married",
                                      "Separated",
                                      "Divorced",
                                      "Widowed",
                                      "Not applicable",
                                      "Script error / schedule not applicable",
                                      "Not asked",
                                      "Prefer not to say",
                                      "Don't know",
                                      "Refused"))) %>%
  select(NSID, partnr19)

# ============================================================
# 2. partnradu25 from W8DMARSTAT (Wave 8, Age 25) - detailed
# ============================================================
partnradu25_df <- wave8 %>%
  select(NSID, W8DMARSTAT) %>%
  mutate(partnradu25 = convert_missing(W8DMARSTAT)) %>%
  mutate(partnradu25 = factor(partnradu25,
                               levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1, -8, -9),
                               labels = c("Single (never married or in CP)",
                                          "Married",
                                          "Separated (still legally married)",
                                          "Divorced",
                                          "Widowed",
                                          "Civil Partner",
                                          "Separated (still legally in CP)",
                                          "Former Civil Partner",
                                          "Surviving Civil Partner",
                                          "Not applicable",
                                          "Insufficient information",
                                          "Refused"))) %>%
  select(NSID, partnradu25)

# ============================================================
# 3. partnradu32 from W9DMARSTAT (Wave 9, Age 32) - detailed
# ============================================================
partnradu32_df <- wave9 %>%
  select(NSID, W9DMARSTAT) %>%
  mutate(partnradu32 = convert_missing(W9DMARSTAT)) %>%
  mutate(partnradu32 = factor(partnradu32,
                               levels = c(1, 2, 3, 4, 5, 6, 7, 8, -8, -9),
                               labels = c("Single (never married or in CP)",
                                          "Married",
                                          "Divorced",
                                          "Legally separated",
                                          "Widowed",
                                          "Civil Partner",
                                          "Former Civil Partner",
                                          "Surviving Civil Partner",
                                          "Insufficient information",
                                          "Refused"))) %>%
  select(NSID, partnradu32)

# ============================================================
# 4. partnr25 from W8DMARSTAT (Wave 8, Age 25) - collapsed
# ============================================================
partnr25_df <- wave8 %>%
  select(NSID, W8DMARSTAT) %>%
  mutate(partnradu25_raw = convert_missing(W8DMARSTAT)) %>%
  mutate(partnr25 = partnradu25_raw) %>%
  mutate(partnr25 = case_when(
    partnradu25_raw == 1 ~ 1,
    partnradu25_raw == 2 ~ 2,
    partnradu25_raw == 3 ~ 3,
    partnradu25_raw == 4 ~ 4,
    partnradu25_raw == 5 ~ 5,
    partnradu25_raw == 6 ~ 2,  # Civil Partner -> Married
    partnradu25_raw == 7 ~ 3,  # Separated in CP -> Separated
    partnradu25_raw == 8 ~ 4,  # Former Civil Partner -> Divorced
    partnradu25_raw == 9 ~ 5,  # Surviving Civil Partner -> Widowed
    TRUE ~ partnradu25_raw  # keep missing codes as-is
  )) %>%
  mutate(partnr25 = factor(partnr25,
                           levels = c(1, 2, 3, 4, 5, -1, -2, -3, -7, -8, -9),
                           labels = c("Single (never married)",
                                      "Married",
                                      "Separated",
                                      "Divorced",
                                      "Widowed",
                                      "Not applicable",
                                      "Script error / schedule not applicable",
                                      "Not asked",
                                      "Prefer not to say",
                                      "Insufficient information",
                                      "Refused"))) %>%
  select(NSID, partnr25)

# ============================================================
# 5. partnr32 from W9DMARSTAT (Wave 9, Age 32) - collapsed
# ============================================================
partnr32_df <- wave9 %>%
  select(NSID, W9DMARSTAT) %>%
  mutate(partnradu32_raw = convert_missing(W9DMARSTAT)) %>%
  mutate(partnr32 = partnradu32_raw) %>%
  mutate(partnr32 = case_when(
    partnradu32_raw == 1 ~ 1,
    partnradu32_raw == 2 ~ 2,
    partnradu32_raw == 3 ~ 4,  # Divorced -> 4
    partnradu32_raw == 4 ~ 3,  # Legally separated -> 3
    partnradu32_raw == 5 ~ 5,
    partnradu32_raw == 6 ~ 2,  # Civil Partner -> Married
    partnradu32_raw == 7 ~ 4,  # Former Civil Partner -> Divorced
    partnradu32_raw == 8 ~ 5,  # Surviving Civil Partner -> Widowed
    TRUE ~ partnradu32_raw  # keep missing codes as-is
  )) %>%
  mutate(partnr32 = factor(partnr32,
                           levels = c(1, 2, 3, 4, 5, -1, -2, -3, -7, -8, -9),
                           labels = c("Single (never married)",
                                      "Married",
                                      "Separated",
                                      "Divorced",
                                      "Widowed",
                                      "Not applicable",
                                      "Script error / schedule not applicable",
                                      "Not asked",
                                      "Prefer not to say",
                                      "Insufficient information",
                                      "Refused"))) %>%
  select(NSID, partnr32)

# ============================================================
# Build final output dataframe by joining all derived variables
# ============================================================
output <- cleaned %>%
  select(NSID) %>%
  left_join(partnr19_df, by = "NSID") %>%
  left_join(partnradu25_df, by = "NSID") %>%
  left_join(partnradu32_df, by = "NSID") %>%
  left_join(partnr25_df, by = "NSID") %>%
  left_join(partnr32_df, by = "NSID")

cat("Output dimensions:", dim(output), "\n")
cat("Output variables:", names(output), "\n")

# Verify distributions
for (var in names(output)[-1]) {
  cat("\n---", var, "---\n")
  print(table(output[[var]], useNA = "ifany"))
}

# Write output
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(output, "data/output/cleaned_data.csv")
cat("\nOutput written to data/output/cleaned_data.csv\n")

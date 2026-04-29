library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "data/input/wave_one_lsype_young_person_2020.tab",
  "data/input/wave_two_lsype_young_person_2020.tab",
  "data/input/wave_three_lsype_family_background_2020.tab",
  "data/input/wave_four_lsype_family_background_2020.tab"
)

# Define mapping from wave to variable name
wave_var_mapping <- c(
  "wave_one_lsype_young_person_2020.tab" = "W1englangYP",
  "wave_two_lsype_young_person_2020.tab" = "W2EnglangYP",
  "wave_three_lsype_family_background_2020.tab" = "W3englangHH",
  "wave_four_lsype_family_background_2020.tab" = "W4EngLangHH"
)

# Load each dataset - specify col_types for numeric variables
# The language variables are numeric
typed_cols <- list(
  W1englangYP = "i",
  W2EnglangYP = "i",
  W3englangHH = "i",
  W4EngLangHH = "i"
)

wave1 <- read_delim(files[1], delim = "\t", col_types = cols(
  NSID = col_character(),
  W1englangYP = col_integer(),
  .default = col_character()
))

wave2 <- read_delim(files[2], delim = "\t", col_types = cols(
  NSID = col_character(),
  W2EnglangYP = col_integer(),
  .default = col_character()
))

wave3 <- read_delim(files[3], delim = "\t", col_types = cols(
  NSID = col_character(),
  W3englangHH = col_integer(),
  .default = col_character()
))

wave4 <- read_delim(files[4], delim = "\t", col_types = cols(
  NSID = col_character(),
  W4EngLangHH = col_integer(),
  .default = col_character()
))

# Rename wave-specific variables
wave1 <- wave1 %>% rename(lang_S1 = W1englangYP)
wave2 <- wave2 %>% rename(lang_S2 = W2EnglangYP)
wave3 <- wave3 %>% rename(lang_S3 = W3englangHH)
wave4 <- wave4 %>% rename(lang_S4 = W4EngLangHH)

# Merge all datasets by NSID (keep all observations)
merged <- full_join(wave1, wave2, by = "NSID", suffix = c("_S1", "_S2"))
merged <- full_join(merged, wave3, by = "NSID", suffix = c("_S1", "_S3"))
merged <- full_join(merged, wave4, by = "NSID", suffix = c("_S1", "_S4"))

print("Merge complete!")
print(paste("Columns in merged:", paste(names(merged), collapse=", ")))

# Convert missing codes to standard codes for each wave variable
# For W1englangYP/W2EnglangYP: -99, -92, -91, -1
# For W3englangHH/W4EngLangHH: -99, -92, -91, -1

merged <- merged %>%
  mutate(
    lang_S1 = case_when(
      is.na(as.numeric(lang_S1)) ~ NA_real_,
      lang_S1 == -99 ~ -3L,
      lang_S1 == -92 ~ -9L,
      lang_S1 == -91 ~ -1L,
      lang_S1 == -1 ~ -8L,
      TRUE ~ as.numeric(lang_S1)
    ),
    lang_S2 = case_when(
      is.na(as.numeric(lang_S2)) ~ NA_real_,
      lang_S2 == -99 ~ -3L,
      lang_S2 == -92 ~ -9L,
      lang_S2 == -91 ~ -1L,
      lang_S2 == -1 ~ -8L,
      TRUE ~ as.numeric(lang_S2)
    ),
    lang_S3 = case_when(
      is.na(as.numeric(lang_S3)) ~ NA_real_,
      lang_S3 == -99 ~ -3L,
      lang_S3 == -92 ~ -9L,
      lang_S3 == -91 ~ -1L,
      lang_S3 == -1 ~ -8L,
      TRUE ~ as.numeric(lang_S3)
    ),
    lang_S4 = case_when(
      is.na(as.numeric(lang_S4)) ~ NA_real_,
      lang_S4 == -99 ~ -3L,
      lang_S4 == -92 ~ -9L,
      lang_S4 == -91 ~ -1L,
      lang_S4 == -1 ~ -8L,
      TRUE ~ as.numeric(lang_S4)
    )
  )

print("Standardization complete!")

# Create consolidated language variable
# Time-invariant variable: prioritize earliest positive value
merged <- merged %>%
  mutate(
    lang = case_when(
      !is.na(lang_S1) & lang_S1 > 0 ~ lang_S1,
      !is.na(lang_S2) & lang_S2 > 0 ~ lang_S2,
      !is.na(lang_S3) & lang_S3 > 0 ~ lang_S3,
      !is.na(lang_S4) & lang_S4 > 0 ~ lang_S4,
      !is.na(lang_S1) ~ lang_S1,
      !is.na(lang_S2) ~ lang_S2,
      !is.na(lang_S3) ~ lang_S3,
      !is.na(lang_S4) ~ lang_S4,
      TRUE ~ -3L
    )
  )

print("Consolidation complete!")

# Create factor variable with labels for lang
merged <- merged %>%
  mutate(
    lang = factor(lang, 
      levels = c(-3, -9, -1, -8, 1, 2, 3, 4),
      labels = c("Not asked/interview not conducted", "Refused", "Not applicable", "Don't know", 
                 "Yes - English only", "Yes - English first/main and speaks other languages",
                 "No - another language is first/main", "Household is bilingual")
    )
  )

print("Factor creation complete!")

# Remove wave-specific variables
merged <- merged %>%
  select(-ends_with("_S"))

print("Cleanup complete!")

# Write output
write_csv(merged, "data/output/cleaned_data.csv")

print("Processing complete!")
print(paste("Total observations:", nrow(merged)))
print(paste("Total variables:", ncol(merged)))
print(head(merged))
library(dplyr)
library(readr)
library(tidyr)
library(labelled)
library(haven)

# Load all files
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID
df <- full_join(w1, w2, by = "NSID") %>%
  full_join(w3, by = "NSID") %>%
  full_join(w4, by = "NSID")

# Helper function to recode missing values
code_missing <- function(x) {
  x[x == -94] <- -2  # Map -94 to -2
  x[x == -1] <- -8   # Map -1 (Don't know) to -8
  # Map other negative codes to standard missing values
  x[x %in% c(-999, -998, -997, -995)] <- -2
  x[x == -99] <- -3
  x[x == -92] <- -9
  x[x == -91] <- -1
  # Convert NA to -3 (not asked)
  x[is.na(x)] <- -3
  return(x)
}

# Apply missing value coding to source variables in the merged data frame
df$W1englangYP <- code_missing(df$W1englangYP)
df$W2EnglangYP <- code_missing(df$W2EnglangYP)
df$W3englangHH <- code_missing(df$W3englangHH)
df$W4EngLangHH <- code_missing(df$W4EngLangHH)

# Create the consolidated lang variable
# Use earliest valid (1-4) response first
df$lang <- coalesce(
  ifelse(df$W1englangYP %in% c(1, 2, 3, 4), df$W1englangYP, NA_real_),
  ifelse(df$W2EnglangYP %in% c(1, 2, 3, 4), df$W2EnglangYP, NA_real_),
  ifelse(df$W3englangHH %in% c(1, 2, 3, 4), df$W3englangHH, NA_real_),
  ifelse(df$W4EngLangHH %in% c(1, 2, 3, 4), df$W4EngLangHH, NA_real_)
)

# Set any remaining NAs to -3 (not asked)
df$lang[is.na(df$lang)] <- -3

# Convert lang to labelled factor
df$lang <- factor(df$lang, levels = c(1, 2, 3, 4, -1, -2, -3, -8, -9),
                  labels = c("Yes - English only",
                             "Yes - English first/main and speaks other languages",
                             "No - another language is first/main",
                             "Bilingual",
                             "Not applicable",
                             "Schedule not applicable / script error",
                             "Not asked / not interviewed",
                             "Don't know",
                             "Refused"))

# Keep only NSID and lang
df <- df %>% select(NSID, lang)

# Write output
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(df, "data/output/cleaned_data.csv")

cat("Done. Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(df), "\n")
cat("Summary of lang:\n")
print(summary(df$lang))

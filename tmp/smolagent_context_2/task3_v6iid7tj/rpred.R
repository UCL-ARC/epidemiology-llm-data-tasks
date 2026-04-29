library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
path_w1 <- "data/input/wave_one_lsype_young_person_2020.tab"
path_w2 <- "data/input/wave_two_lsype_young_person_2020.tab"
path_w3 <- "data/input/wave_three_lsype_family_background_2020.tab"
path_w4 <- "data/input/wave_four_lsype_family_background_2020.tab"

df_w1 <- read_delim(path_w1, delim = "\t", col_types = cols(.default = "c"))
df_w2 <- read_delim(path_w2, delim = "\t", col_types = cols(.default = "c"))
df_w3 <- read_delim(path_w3, delim = "\t", col_types = cols(.default = "c"))
df_w4 <- read_delim(path_w4, delim = "\t", col_types = cols(.default = "c"))

# Convert specific target variables to numeric
df_w1$W1englangYP <- as.numeric(df_w1$W1englangYP)
df_w2$W2EnglangYP <- as.numeric(df_w2$W2EnglangYP)
df_w3$W3englangHH <- as.numeric(df_w3$W3englangHH)
df_w4$W4EngLangHH <- as.numeric(df_w4$W4EngLangHH)

# Merge datasets
full_df <- df_w1 %>%
  full_join(df_w2, by = "NSID") %>%
  full_join(df_w3, by = "NSID") %>%
  full_join(df_w4, by = "NSID")

# 6 & 7. Missing Value Harmonisation Function
harmonise_missing <- function(x) {
  x <- as.numeric(x)
  # Specific mapping from additional requirements
  x[x == -94] <- -2
  x[x == -1] <- -8 # "Don't know" mapping
  
  # General mappings based on metadata/rules
  # -999, -998, -997, -995 -> -2 (Script error/info lost)
  x[x %in% c(-999, -998, -997, -995)] <- -2
  # -99 -> -3 (Not interviewed)
  x[x == -99] <- -3
  # -92 -> -9 (Refused)
  x[x == -92] <- -9
  # -91 -> -1 (Not applicable)
  x[x == -91] <- -1
  
  # Convert NA to -3
  x[is.na(x)] <- -3
  return(x)
}

# Apply harmonisation to target variables
full_df <- full_df %>%
  mutate(
    englang14 = harmonise_missing(W1englangYP),
    englang15 = harmonise_missing(W2EnglangYP),
    englang16 = harmonise_missing(W3englangHH),
    englang17 = harmonise_missing(W4EngLangHH)
  )

# 8. Response Category Harmonisation
# All variables have identical substantive categories: 1: Yes-only, 2: Yes-other, 3: No, 4: Bilingual
# No collapse needed as categories match across waves.

# 9. Consolidation Logic: Earliest-positive-first, then earliest-negative-first
# Substantive (positive) values are 1, 2, 3, 4
# Missing (negative) values are -1, -2, -3, -7, -8, -9

consolidate_englang <- function(...) {
  vals <- c(...)
  # 1. Look for earliest positive value
  pos_idx <- which(vals >= 1)
  if (length(pos_idx) > 0) return(vals[pos_idx[1]])
  
  # 2. Fallback to earliest negative value
  neg_idx <- which(vals < 0)
  if (length(neg_idx) > 0) return(vals[neg_idx[1]])
  
  # 3. Default if all are NA (though harmonise_missing already handled NAs)
  return(-3)
}

full_df$englang_consolidated <- apply(full_df[, c("englang14", "englang15", "englang16", "englang17")], 1, function(row) {
  consolidate_englang(row[1], row[2], row[3], row[4])
})

# 10. Labels
val_labels <- c(
  "1" = "Yes - English only",
  "2" = "Yes - English first/main and speaks other languages",
  "3" = "No, another language is respondent's first or main language",
  "4" = "Respondent is bilingual",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

# Create factors for final variables
final_cols <- c("englang14", "englang15", "englang16", "englang17", "englang_consolidated")
for (col in final_cols) {
  full_df[[col]] <- factor(full_df[[col]], levels = as.numeric(names(val_labels)), labels = val_labels)
}

# 12. Output Requirements
final_output <- full_df %>%
  select(NSID, all_of(final_cols))

write_csv(final_output, "data/output/cleaned_data.csv")

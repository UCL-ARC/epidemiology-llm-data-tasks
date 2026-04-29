library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load files
path_prefix <- "data/input/"
file1 <- readr::read_delim(paste0(path_prefix, "wave_one_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file2 <- readr::read_delim(paste0(path_prefix, "wave_two_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file3 <- readr::read_delim(paste0(path_prefix, "wave_three_lsype_family_background_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file4 <- readr::read_delim(paste0(path_prefix, "wave_four_lsype_family_background_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c"))

# Convert key variables to numeric for processing
file1 <- file1 %>% mutate(W1englangYP = as.numeric(W1englangYP))
file2 <- file2 %>% mutate(W2EnglangYP = as.numeric(W2EnglangYP))
file3 <- file3 %>% mutate(W3englangHH = as.numeric(W3englangHH))
file4 <- file4 %>% mutate(W4EngLangHH = as.numeric(W4EngLangHH))

# Merge datasets
df <- file1 %>%
  full_join(file2, by = "NSID") %>%
  full_join(file3, by = "NSID") %>%
  full_join(file4, by = "NSID")

# Define a cleaning function for the language variables
clean_lang <- function(x) {
  # Initialize with standard missing code for NAs
  res <- rep(-3, length(x))
  
  # Use a mask to avoid NA issues in indexing
  not_na <- !is.na(x)
  
  # Substantive values 1-4
  subst <- not_na & (x >= 1 & x <= 4)
  res[subst] <- x[subst]
  
  # Specific requirement: -94 to -2
  res[not_na & (x == -94)] <- -2
  
  # Specific requirement: -1 labelled "Don't know" to -8
  res[not_na & (x == -1)] <- -8
  
  # Standard missing values based on metadata labels
  res[not_na & (x == -92)] <- -9
  res[not_na & (x == -91)] <- -1
  res[not_na & (x == -99)] <- -3
  res[not_na & (x == -999)] <- -3
  res[not_na & (x == -997)] <- -2
  res[not_na & (x == -998)] <- -2
  res[not_na & (x == -995)] <- -2
  
  return(res)
}

# Process each wave
df <- df %>%
  mutate(
    w1_l = clean_lang(W1englangYP),
    w2_l = clean_lang(W2EnglangYP),
    w3_l = clean_lang(W3englangHH),
    w4_l = clean_lang(W4EngLangHH)
  )

# Derivation logic: earliest valid positive response (1-4)
get_consolidated_lang <- function(w1, w2, w3, w4) {
  res <- rep(NA, length(w1))
  for(i in 1:length(w1)) {
    vals <- c(w1[i], w2[i], w3[i], w4[i])
    # Filter for substantive responses (1-4)
    valid_idx <- which(vals >= 1 & vals <= 4)
    if(length(valid_idx) > 0) {
      res[i] <- vals[valid_idx[1]]
    } else {
      # Fallback to earliest missing code
      res[i] <- vals[1]
    }
  }
  return(res)
}

df <- df %>%
  mutate(lang = get_consolidated_lang(w1_l, w2_l, w3_l, w4_l))

# Define labels for the factor
lang_labels <- c(
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

# Convert to factor with explicit levels
df$lang <- factor(df$lang, levels = as.numeric(names(lang_labels)), labels = lang_labels)

# Final selection
final_df <- df %>%
  select(NSID, lang)

# Save output
readr::write_csv(final_df, "data/output/cleaned_data.csv")
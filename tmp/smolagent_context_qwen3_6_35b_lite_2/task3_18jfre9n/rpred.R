library(dplyr)
library(readr)
library(labelled)

# Load all files
df1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
df4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID using full_join
merged <- df1 %>%
  full_join(df2, by = "NSID") %>%
  full_join(df3, by = "NSID") %>%
  full_join(df4, by = "NSID")

# Function to map source missing values to standard codes
map_missing <- function(x) {
  # Map specific missing codes based on label meaning
  x[x == -999] <- -2    # HH grid missing / Script error / Missing history
  x[x == -998] <- -2    # Interviewer missed question / Script error
  x[x == -997] <- -2    # Script error
  x[x == -995] <- -2    # Missing history section data
  x[x == -99]  <- -3    # Not interviewed
  x[x == -92]  <- -9    # Refused
  x[x == -91]  <- -1    # Not applicable
  x[x == -1]   <- -8    # Don't know
  x[x == -100] <- -2    # (general case)
  x[x == -97]  <- -2    # (general case)
  x[x == -94]  <- -8    # (general case)
  x[x == -95]  <- -8    # (general case)
  # Convert remaining NA to -3 (not asked / not interviewed)
  x[is.na(x)] <- -3
  return(x)
}

# Map missing values for each wave variable
w1_lang <- map_missing(merged$W1englangYP)
w2_lang <- map_missing(merged$W2EnglangYP)
w3_lang <- map_missing(merged$W3englangHH)
w4_lang <- map_missing(merged$W4EngLangHH)

# Consolidated variable: earliest-valid-first
# Valid substantive values are 1, 2, 3, 4
# Use wave 1 first, then wave 2, then wave 3, then wave 4
lang <- rep(-3, nrow(merged))  # default to "not asked"

for (i in 1:nrow(merged)) {
  vals <- c(w1_lang[i], w2_lang[i], w3_lang[i], w4_lang[i])
  # Find first valid substantive response (1-4)
  valid_idx <- which(vals %in% c(1, 2, 3, 4))
  if (length(valid_idx) > 0) {
    lang[i] <- vals[valid_idx[1]]
  } else {
    # No valid substantive response; use first non-NA missing code
    missing_vals <- vals[!vals %in% c(1, 2, 3, 4)]
    if (length(missing_vals) > 0) {
      lang[i] <- missing_vals[1]
    }
  }
}

# Create labelled factor
lang_labels <- c(
  "1" = "Yes - English only",
  "2" = "Yes - English first/main and speaks other languages",
  "3" = "No, another language is first/main language",
  "4" = "Bilingual"
)

lang_factor <- factor(lang, levels = c(1, 2, 3, 4, -8, -9, -3, -1, -2),
                      labels = c(
                        "1" = "Yes - English only",
                        "2" = "Yes - English first/main and speaks other languages",
                        "3" = "No, another language is first/main language",
                        "4" = "Bilingual",
                        "-8" = "Don't know",
                        "-9" = "Refused",
                        "-3" = "Not asked / not interviewed",
                        "-1" = "Not applicable",
                        "-2" = "Schedule not applicable / script error / information lost"
                      ))

# Add to merged dataframe
merged$lang <- lang_factor

# Keep only NSID and lang
output <- merged %>% select(NSID, lang)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

# Print summary
cat("Output dimensions:", dim(output), "\n")
cat("Number of unique NSID:", length(unique(output$NSID)), "\n")
cat("\nDistribution of lang:\n")
print(table(output$lang, useNA = "ifany"))

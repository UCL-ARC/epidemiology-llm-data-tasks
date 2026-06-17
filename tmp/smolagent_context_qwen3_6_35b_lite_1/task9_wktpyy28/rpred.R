library(dplyr)
library(readr)
library(haven)

# Load all files
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all waves by NSID using full_join
df <- full_join(w1, w2, by = "NSID")
df <- full_join(df, w4, by = "NSID")

cat("Dimensions after merge:", dim(df), "\n")
cat("NSID count:", n_distinct(df$NSID), "\n")

# Function to map missing value codes to standard scheme
map_missing <- function(x) {
  x[is.na(x)] <- -3
  x[x == -999] <- -2
  x[x == -99] <- -3
  x[x == -98] <- -2
  x[x == -94] <- -2
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  return(x)
}

# Apply missing value mapping to source variables
w1_mum_mapped <- map_missing(w1$W1hiqualmum)
w1_dad_mapped <- map_missing(w1$W1hiqualdad)
w2_mum_mapped <- map_missing(w2$W2hiqualmum)
w2_dad_mapped <- map_missing(w2$W2hiqualdad)
w4_mum_mapped <- map_missing(w4$w4hiqualmum)
w4_dad_mapped <- map_missing(w4$w4hiqualdad)

# Function to collapse 20 detailed categories to 5 NVQ levels
collapse_to_nvq5 <- function(x) {
  result <- rep(NA_real_, length(x))
  
  valid <- !is.na(x) & x > 0
  
  # 5-level NVQ collapsed categories:
  # 1 = University degree (Higher Degree, First Degree, HE Diploma)
  # 2 = NVQ 4 (HNC/HND/NVQ4, Teaching qualification non-degree, Nursing qualification non-degree)
  # 3 = NVQ 3 (City and guilds part III, NVQ3)
  # 4 = NVQ 2 (City and guilds part II, NVQ2)
  # 5 = NVQ 1 or lower
  
  result[valid & (x == 1 | x == 2 | x == 3)] <- 1
  result[valid & (x == 4 | x == 5 | x == 6)] <- 2
  result[valid & (x == 9)] <- 3
  result[valid & (x == 14)] <- 4
  result[valid & (x %in% c(7, 8, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20))] <- 5
  
  # Preserve missing codes
  result[!valid] <- x[!valid]
  
  return(result)
}

# Create 20-category detailed consolidated variables (earliest valid first)
# Mother detailed: educdtlma
educdtlma <- rep(NA_real_, nrow(df))
for (i in seq_len(nrow(df))) {
  for (src in list(w1_mum_mapped[i], w2_mum_mapped[i], w4_mum_mapped[i])) {
    if (!is.na(src) && src > 0) {
      educdtlma[i] <- src
      break
    }
  }
}

# Father detailed: educdtlpa
educdtlpa <- rep(NA_real_, nrow(df))
for (i in seq_len(nrow(df))) {
  for (src in list(w1_dad_mapped[i], w2_dad_mapped[i], w4_dad_mapped[i])) {
    if (!is.na(src) && src > 0) {
      educdtlpa[i] <- src
      break
    }
  }
}

# Create 5-level NVQ consolidated variables (earliest valid first)
educma <- collapse_to_nvq5(educdtlma)
educpa <- collapse_to_nvq5(educdtlpa)

# Create labels for 20-category variables
labels_20 <- c(
  "Higher Degree" = 1, "First Degree" = 2, "HE Diploma" = 3, "HNC/HND/NVQ4" = 4,
  "Teaching qualification, non-degree" = 5, "Nursing qualification, non-degree" = 6,
  "A Levels" = 7, "OND/ONC" = 8, "City and guilds part III, NVQ3" = 9,
  "CSYS" = 10, "Scottish Higher Grade" = 11, "AS Level" = 12,
  "Trade apprenticeship" = 13, "City and guilds part II, NVQ2" = 14,
  "GCSE grade A-C and equivalent" = 15, "GCSE grade D-E and equivalent" = 16,
  "City and guilds part I, NVQ1" = 17, "Youth training, skill seekers" = 18,
  "Qualification, level unspecified" = 19, "No qualification mentioned" = 20
)

missing_labels_20 <- c(
  "Refusal" = -9, "Don\'t know" = -8, "Prefer not to say" = -7,
  "Not asked / not interviewed" = -3, "Schedule not applicable" = -2,
  "Item not applicable" = -1
)

labels_5 <- c(
  "University degree" = 1, "NVQ 4" = 2, "NVQ 3" = 3,
  "NVQ 2" = 4, "NVQ 1 or lower" = 5
)

# Apply labels using haven::labelled()
educdtlma <- labelled(educdtlma, labels = labels_20)
educdtlpa <- labelled(educdtlpa, labels = labels_20)
educma <- labelled(educma, labels = labels_5)
educpa <- labelled(educpa, labels = labels_5)

# Create final output dataframe
cleaned <- df %>%
  select(NSID) %>%
  mutate(
    educdtlma = educdtlma,
    educdtlpa = educdtlpa,
    educma = educma,
    educpa = educpa
  )

# Write output
write_csv(cleaned, "data/output/cleaned_data.csv")
cat("Output written to data/output/cleaned_data.csv\n")
cat("Final dimensions:", dim(cleaned), "\n")
cat("Variables:", names(cleaned), "\n")

# Summary
cat("\nSummary of educdtlma:\n")
print(table(educdtlma, useNA = "ifany"))
cat("\nSummary of educma:\n")
print(table(educma, useNA = "ifany"))
cat("\nSummary of educdtlpa:\n")
print(table(educdtlpa, useNA = "ifany"))
cat("\nSummary of educpa:\n")
print(table(educpa, useNA = "ifany"))

# Verify NSID count matches
cat("\nUnique NSIDs in output:", n_distinct(cleaned$NSID), "\n")
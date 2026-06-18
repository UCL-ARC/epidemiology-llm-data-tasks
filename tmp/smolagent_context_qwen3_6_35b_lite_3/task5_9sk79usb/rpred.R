library(dplyr)
library(readr)
library(tidyr)
library(haven)
library(labelled)

# Load all data files from the input directory
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge datasets
df <- full_join(wave6, wave8, by = "NSID")
df <- full_join(df, wave9, by = "NSID")

# Helper to recode a variable to standard codes
recode_to_standard <- function(x, standard_map, missing_map) {
  result <- rep(-3, length(x))  # Default: not asked
  
  for (i in seq_along(x)) {
    val <- x[i]
    if (is.na(val)) {
      result[i] <- -3  # Not asked / not interviewed
    } else if (val %in% names(missing_map)) {
      result[i] <- missing_map[[as.character(val)]]
    } else if (val %in% names(standard_map)) {
      result[i] <- standard_map[[as.character(val)]]
    } else {
      result[i] <- -3  # Unknown, default to not asked
    }
  }
  result
}

# ============================================================
# W6MarStatYP (Age 19) - Collapsed version partnr19
# ============================================================
standard_19 <- c("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5)
missing_19 <- c("-997" = -2, "-97" = -7, "-92" = -9, "-91" = -1, "-1" = -8)

df$partnr19 <- recode_to_standard(df$W6MarStatYP, standard_19, missing_19)

# ============================================================
# W8DMARSTAT (Age 25) - Collapsed version partnr25
# ============================================================
standard_25_collapsed <- c("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, 
                            "6" = 2, "7" = 3, "8" = 4, "9" = 5)
missing_25 <- c("-9" = -9, "-8" = -8, "-1" = -1)

df$partnr25 <- recode_to_standard(df$W8DMARSTAT, standard_25_collapsed, missing_25)

# ============================================================
# W8DMARSTAT (Age 25) - Detailed version partnradu25
# ============================================================
standard_25_detailed <- c("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5,
                           "6" = 6, "7" = 7, "8" = 8, "9" = 9)

df$partnradu25 <- recode_to_standard(df$W8DMARSTAT, standard_25_detailed, missing_25)

# ============================================================
# W9DMARSTAT (Age 32) - Collapsed version partnr32
# ============================================================
standard_32_collapsed <- c("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5,
                            "6" = 2, "7" = 4, "8" = 5)
missing_32 <- c("-9" = -9, "-8" = -8)

df$partnr32 <- recode_to_standard(df$W9DMARSTAT, standard_32_collapsed, missing_32)

# ============================================================
# W9DMARSTAT (Age 32) - Detailed version partnradu32
# ============================================================
standard_32_detailed <- c("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5,
                           "6" = 6, "7" = 7, "8" = 8)

df$partnradu32 <- recode_to_standard(df$W9DMARSTAT, standard_32_detailed, missing_32)

# ============================================================
# Create labelled factors using haven::labelled()
# Syntax: labelled(x, labels = c("Label text" = code))
# ============================================================

# Partnr19 - collapsed
labels_19 <- c("Single, that is never married" = 1, 
               "Married" = 2, 
               "Separated" = 3, 
               "Divorced" = 4, 
               "Widowed" = 5)
df$partnr19 <- haven::labelled(df$partnr19, labels = labels_19)

# Partnr25 - collapsed
labels_25_c <- c("Single and never married or in a CP" = 1,
                 "Married or Civil Partner" = 2,
                 "Separated (including legally separated from CP)" = 3,
                 "Divorced or former Civil Partner" = 4,
                 "Widowed or surviving Civil Partner" = 5)
df$partnr25 <- haven::labelled(df$partnr25, labels = labels_25_c)

# partnradu25 - detailed
labels_25_d <- c("Single and never married or in a CP" = 1,
                 "Married" = 2,
                 "Separated but still legally married" = 3,
                 "Divorced" = 4,
                 "Widowed" = 5,
                 "A Civil Partner" = 6,
                 "Separated but still legally in a CP" = 7,
                 "A former Civil Partner" = 8,
                 "A surviving Civil Partner" = 9)
df$partnradu25 <- haven::labelled(df$partnradu25, labels = labels_25_d)

# Partnr32 - collapsed
labels_32_c <- c("Single (never married or in CP)" = 1,
                 "Married or Civil Partner" = 2,
                 "Divorced" = 3,
                 "Legally separated" = 4,
                 "Widowed or surviving Civil Partner" = 5)
df$partnr32 <- haven::labelled(df$partnr32, labels = labels_32_c)

# partnradu32 - detailed
labels_32_d <- c("Single (never married or in CP)" = 1,
                 "Married" = 2,
                 "Divorced" = 3,
                 "Legally separated" = 4,
                 "Widowed" = 5,
                 "Civil Partner in a legally recognised CP" = 6,
                 "Former Civil Partner (dissolved)" = 7,
                 "Surviving Civil Partner" = 8)
df$partnradu32 <- haven::labelled(df$partnradu32, labels = labels_32_d)

# ============================================================
# Select final columns
# ============================================================
final_df <- df %>% select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# ============================================================
# Write output
# ============================================================
write_csv(final_df, "data/output/cleaned_data.csv")

cat("\nOutput saved to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(final_df), "\n")
cat("Columns:", paste(names(final_df), collapse=", "), "\n")

# Print some summary stats
for (var in c("partnr19", "partnr25", "partnr32", "partnradu25", "partnradu32")) {
  cat(sprintf("\n--- %s ---\n", var))
  print(summary(final_df[[var]]))
}

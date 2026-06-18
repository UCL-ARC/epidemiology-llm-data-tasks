# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load each file explicitly by name
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
w9_deriv <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)
w9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID
df <- full_join(wave1, wave2, by = "NSID")
df <- full_join(df, wave3, by = "NSID")
df <- full_join(df, wave4, by = "NSID")
df <- full_join(df, w8, by = "NSID")
df <- full_join(df, w9_deriv, by = "NSID")
df <- full_join(df, w9_main, by = "NSID")

# Create regub15 from wave2 urbind.x
df$regub15 <- df$urbind.x
# Create regub16 from wave3 urbind.y
df$regub16 <- df$urbind.y

# Create regov15 from wave2 gor.x
df$regov15 <- df$gor.x
# Create regov16 from wave3 gor.y
df$regov16 <- df$gor.y

# Create regor25 from W8DGOR
df$regor25 <- df$W8DGOR

# Create regor32 from W9DRGN
df$regor32 <- df$W9DRGN

# Create regint32 from W9NATIONRES
df$regint32 <- df$W9NATIONRES

# Function to convert missing values to standard codes
convert_missing <- function(x) {
  # Convert NA to -3 (not asked)
  x[is.na(x)] <- -3
  
  # Convert -94 to -8 (Insufficient information -> Don't know)
  x[x == -94] <- -8
  
  return(x)
}

# Apply missing value conversion to regub15 and regub16
df$regub15 <- convert_missing(df$regub15)
df$regub16 <- convert_missing(df$regub16)

# Apply missing value conversion to regov15 and regov16
df$regov15 <- convert_missing(df$regov15)
df$regov16 <- convert_missing(df$regov16)

# Create regor25 from W8DGOR - map 13 to -2
df$regor25 <- df$W8DGOR
# Map 13 to -2 (Unknown due to faulty/missing postcode)
df$regor25[df$regor25 == 13] <- -2
# Convert other missing codes to standard
df$regor25[is.na(df$regor25)] <- -3

# Create regor32 from W9DRGN - map 13 to -2
df$regor32 <- df$W9DRGN
# Map 13 to -2 (Unknown due to faulty/missing postcode)
df$regor32[df$regor32 == 13] <- -2
# Convert other missing codes to standard
df$regor32[is.na(df$regor32)] <- -3

# Create regint32 from W9NATIONRES
# 1-4: England/Scotland/Wales/Northern Ireland -> 1 (In UK)
# 5: Outside of UK or unknown -> 2 (Abroad)
df$regint32 <- df$W9NATIONRES
df$regint32[df$regint32 %in% 1:4] <- 1
df$regint32[df$regint32 == 5] <- 2
# Convert missing codes to standard
df$regint32[is.na(df$regint32)] <- -3

# Select only final variables
final_vars <- c("NSID", "regub15", "regub16", "regov15", "regov16", "regor25", "regor32", "regint32")
df_clean <- df[, final_vars]

# Write output
write_csv(df_clean, "data/output/cleaned_data.csv")

cat("\nCleaning complete. Output written to data/output/cleaned_data.csv\n")
cat("Output dimensions:", nrow(df_clean), "x", ncol(df_clean), "\n")

# Show first few rows
cat("\nFirst 5 rows of output:\n")
print(head(df_clean, 5))

cat("\nLast 5 rows of output:\n")
print(tail(df_clean, 5))

cat("\nUnique values in each variable:\n")
for(col in names(df_clean)) {
  cat(sprintf("%s: ", col))
  print(sort(unique(df_clean[[col]])))
}

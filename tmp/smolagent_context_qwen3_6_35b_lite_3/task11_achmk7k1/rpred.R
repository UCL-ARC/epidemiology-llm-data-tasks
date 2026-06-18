library(dplyr)
library(readr)

# Load all four wave files
df1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
df2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
df3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
df4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all files by NSID
df_full <- full_join(df1, df2, by = "NSID")
df_full <- full_join(df_full, df3, by = "NSID")
df_full <- full_join(df_full, df4, by = "NSID")

# Function to harmonize employment status
code_employment <- function(x) {
  x[is.na(x)] <- -3
  x[x == -999] <- -2
  x[x == -99] <- -3
  x[x == -98] <- -3
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -996] <- -3
  return(x)
}

# Apply harmonization
df_full$ecoactma14 <- code_employment(df_full$W1empsmum)
df_full$ecoactpa14 <- code_employment(df_full$W1empsdad)
df_full$ecoactma15 <- code_employment(df_full$W2empsmum)
df_full$ecoactpa15 <- code_employment(df_full$W2empsdad)
df_full$ecoactma16 <- code_employment(df_full$W3empsmum)
df_full$ecoactpa16 <- code_employment(df_full$W3empsdad)
df_full$ecoactma17 <- code_employment(df_full$w4empsmum)
df_full$ecoactpa17 <- code_employment(df_full$w4empsdad)

# Define all possible levels and their labels
all_levels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "-2", "-3", "-8", "-9")
all_labels <- c(
  "Doing paid work for 30 or more hours a week",
  "Doing paid work for fewer than 30 hours a week",
  "Unemployed/ Looking for a job",
  "On a training course or scheme",
  "In full-time education/ at school",
  "Looking after the family/ household",
  "Retired from work altogether",
  "Sick/ disabled",
  "Other",
  "Schedule not applicable / script error / information lost",
  "Not asked at the fieldwork stage / not interviewed",
  "Don\'t know / insufficient information",
  "Refusal"
)

# Create labelled factors
make_factor <- function(x) {
  x_chr <- as.character(x)
  fac <- factor(x_chr, levels = all_levels, ordered = FALSE)
  # Store labels as an attribute
  attr(fac, "labels") <- setNames(all_labels, all_levels)
  return(fac)
}

# Apply to all variables
for (var in c("ecoactma14", "ecoactpa14", "ecoactma15", "ecoactpa15",
              "ecoactma16", "ecoactpa16", "ecoactma17", "ecoactpa17")) {
  df_full[[var]] <- make_factor(df_full[[var]])
}

# Select only NSID and derived variables
output_df <- df_full %>% select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15,
                                 ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Write output
write_csv(output_df, "data/output/cleaned_data.csv")

# Print summary
cat("Output dimensions:", dim(output_df), "\n")
cat("Variables:", names(output_df), "\n")
cat("Sample of first 5 rows:\n")
print(head(output_df, 5))

# Check for any issues
for (var in names(output_df)[-1]) {
  cat("\n", var, ":\n")
  cat("  Unique values:", length(unique(output_df[[var]])), "\n")
  cat("  Missing (-3):", sum(output_df[[var]] == "-3"), "\n")
  cat("  Missing (-2):", sum(output_df[[var]] == "-2"), "\n")
  cat("  Missing (-8):", sum(output_df[[var]] == "-8"), "\n")
  cat("  Missing (-9):", sum(output_df[[var]] == "-9"), "\n")
}

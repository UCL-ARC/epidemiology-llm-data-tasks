library(dplyr)
library(readr)
library(haven)
library(labelled)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Define file paths
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_derived_variables.tab"
)

# Load each file into a named list
data_list <- list()
for (f in files) {
  path <- file.path("data/input", f)
  data_list[[f]] <- read_delim(path, delim = "\t", show_col_types = FALSE)
  cat(sprintf("Loaded %s: %d rows, %d cols\n", f, nrow(data_list[[f]]), ncol(data_list[[f]])))
}

# Merge all datasets by NSID using full_join
cleaned <- data_list[[1]]
for (i in 2:length(data_list)) {
  cleaned <- full_join(cleaned, data_list[[i]], by = "NSID")
  cat(sprintf("After merge with %s: %d rows\n", names(data_list)[i], nrow(cleaned)))
}

cat(sprintf("\nFinal merged dataset: %d rows, %d cols\n", nrow(cleaned), ncol(cleaned)))

# Define the 16 income band labels (substantive categories)
income_labels <- c(
  "less than 25",
  "25 to 50",
  "50 to 90",
  "90 to 140",
  "140 to 240",
  "240 to 300",
  "300 to 350",
  "350 to 400",
  "400 to 500",
  "500 to 600",
  "600 to 700",
  "700 to 800",
  "800 to 900",
  "900 to 1200",
  "1200 to 1400",
  "more than 1400"
)

# Create inc25 from W8DINCB (Wave 8, Age 25)
# W8DINCB has: -1 = Not applicable, 1-16 = income bands
# Convert to labelled factor
cleaned <- cleaned %>%
  mutate(
    inc25 = case_when(
      is.na(W8DINCB) ~ NA_real_,
      W8DINCB == -1 ~ -1,
      W8DINCB >= 1 & W8DINCB <= 16 ~ W8DINCB,
      TRUE ~ NA_real_
    )
  )

# Set value labels for inc25
cleaned <- cleaned %>%
  mutate(
    inc25 = factor(
      inc25,
      levels = c(1:16, -1),
      labels = c(income_labels, "Not applicable"),
      exclude = NULL
    )
  )

# Create inc32 from W9DINCB (Wave 9, Age 32)
# W9DINCB has: -1 = Not applicable, 1-16 = income bands
cleaned <- cleaned %>%
  mutate(
    inc32 = case_when(
      is.na(W9DINCB) ~ NA_real_,
      W9DINCB == -1 ~ -1,
      W9DINCB >= 1 & W9DINCB <= 16 ~ W9DINCB,
      TRUE ~ NA_real_
    )
  )

# Set value labels for inc32
cleaned <- cleaned %>%
  mutate(
    inc32 = factor(
      inc32,
      levels = c(1:16, -1),
      labels = c(income_labels, "Not applicable"),
      exclude = NULL
    )
  )

# Keep only NSID and the two final derived variables
output <- cleaned %>%
  select(NSID, inc25, inc32)

cat(sprintf("\nOutput dataset: %d rows, %d cols\n", nrow(output), ncol(output)))
cat("Columns:", names(output), "\n")

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")
cat("\nOutput written to data/output/cleaned_data.csv\n")

# Print summary
cat("\n=== Summary ===\n")
cat("inc25 (Age 25):\n")
print(table(output$inc25, useNA = "ifany"))
cat("\ninc32 (Age 32):\n")
print(table(output$inc32, useNA = "ifany"))

library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- list(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_derived_variables.tab"
)

# Load all files
load_data <- function(filename) {
  read_delim(file.path("data/input", filename), delim = "\t", show_col_types = FALSE)
}

datasets <- purrr::map(files, load_data)
names(datasets) <- files

# Merge all datasets by NSID using full_join
merged <- datasets[[1]]
for (nm in names(datasets)[-1]) {
  merged <- full_join(merged, datasets[[nm]], by = "NSID")
}

# Create labelled variable helper function
make_labelled <- function(x, labels_dict) {
  labelled::set_value_labels(x, labels_dict)
}

# Income band labels - names are labels, values are codes
labels_dict <- c(`less than 25` = 1L,
                 `25 to 50` = 2L,
                 `50 to 90` = 3L,
                 `90 to 140` = 4L,
                 `140 to 240` = 5L,
                 `240 to 300` = 6L,
                 `300 to 350` = 7L,
                 `350 to 400` = 8L,
                 `400 to 500` = 9L,
                 `500 to 600` = 10L,
                 `600 to 700` = 11L,
                 `700 to 800` = 12L,
                 `800 to 900` = 13L,
                 `900 to 1200` = 14L,
                 `1200 to 1400` = 15L,
                 `more than 1400` = 16L,
                 `Item not applicable` = -1L,
                 `Not asked at the fieldwork stage / not interviewed` = -3L)

# Process inc25 from W8DINCB (age 25, wave 8)
merged <- merged %>%
  mutate(
    inc25 = case_when(
      W8DINCB == -1 ~ -1L,
      is.na(W8DINCB) ~ -3L,
      W8DINCB >= 1 & W8DINCB <= 16 ~ as.integer(W8DINCB),
      TRUE ~ -3L
    ),
    inc32 = case_when(
      W9DINCB == -1 ~ -1L,
      is.na(W9DINCB) ~ -3L,
      W9DINCB >= 1 & W9DINCB <= 16 ~ as.integer(W9DINCB),
      TRUE ~ -3L
    )
  )

# Create labelled variables
merged$inc25 <- make_labelled(merged$inc25, labels_dict)
merged$inc32 <- make_labelled(merged$inc32, labels_dict)

# Select only ID and final derived variables
output <- merged %>%
  select(NSID, inc25, inc32)

# Check summary
print(summary(output$inc25))
print(summary(output$inc32))

# Write output
write_csv(output, "data/output/cleaned_data.csv")
cat("Output written to data/output/cleaned_data.csv\n")

# Verify the output
cat("\nFirst 10 rows:\n")
print(read_csv("data/output/cleaned_data.csv", show_col_types = FALSE))
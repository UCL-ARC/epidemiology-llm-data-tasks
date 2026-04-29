library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
data_w8 <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = readr::cols(.default = "c"))
data_w9 <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols(.default = "c"))

# Select and rename variables
df8 <- data_w8 %>% select(NSID, W8DINCB) %>% rename(inc25 = W8DINCB)
df9 <- data_w9 %>% select(NSID, W9DINCB) %>% rename(inc32 = W9DINCB)

# Merge datasets
merged_df <- dplyr::full_join(df8, df9, by = "NSID")

# Function to harmonize income variables
harmonize_income <- function(x) {
  x <- as.numeric(x)
  
  # Standard Missing-Value Codes:
  # -9 = Refusal, -8 = Don't know, -1 = Not applicable, -3 = Not asked, -2 = Script error
  
  # Specific mapping based on metadata: 
  # Metadata says user_missing_values are '-1.0 thru None'. 
  # Let's treat -1 as Not applicable as per standard code and metadata.
  
  # Handle NAs (Not asked/interviewed) as -3
  x[is.na(x)] <- -3
  
  # Note: The metadata only specifies -1.0 for the provided variables.
  # We apply the standard missing scheme logic.
  # Since only -1 is explicitly in metadata, other missing codes (-9, -8, -2) 
  # would only appear if they existed in the raw numeric data.
  
  return(x)
}

merged_df <- merged_df %>%
  mutate(
    inc25 = harmonize_income(inc25),
    inc32 = harmonize_income(inc32)
  )

# Define labels for factors
# Both variables have the same substantive labels in metadata
income_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-1" = "Item not applicable",
  "-3" = "Not asked/interviewed",
  "-2" = "Script error/lost",
  "1" = "less than 25",
  "2" = "25 to 50",
  "3" = "50 to 90",
  "4" = "90 to 140",
  "5" = "140 to 240",
  "6" = "240 to 300",
  "7" = "300 to 350",
  "8" = "350 to 400",
  "9" = "400 to 500",
  "10" = "500 to 600",
  "11" = "600 to 700",
  "12" = "700 to 800",
  "13" = "800 to 900",
  "14" = "900 to 1200",
  "15" = "1200 to 1400",
  "16" = "more than 1400"
)

# Convert to labelled factors
apply_labels <- function(x) {
  # Use set_labels from labelled package
  # We first convert to factor to ensure labels are applied correctly
  # but the requirement is a "labelled factor".
  # In R, this usually means a factor with labels or a haven_labelled vector.
  # We'll use factor with the specified levels and labels.
  factor(x, levels = as.numeric(names(income_labels)), labels = income_labels)
}

merged_df <- merged_df %>%
  mutate(
    inc25 = apply_labels(inc25),
    inc32 = apply_labels(inc32)
  )

# Output only the requested variables
final_df <- merged_df %>% select(NSID, inc25, inc32)

# Write to CSV
write.csv(final_df, "data/output/cleaned_data.csv", row.names = FALSE)

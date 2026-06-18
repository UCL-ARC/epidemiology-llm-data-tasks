library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define the 17 NS-SEC categories based on metadata
nssec_labels <- c(
  "1" = "Employers in large organisations",
  "2" = "Higher managerial occupations",
  "3" = "Higher professional",
  "4" = "Lower professional",
  "5" = "Lower managerial occupations",
  "6" = "Higher supervisory occupations",
  "7" = "Intermediate",
  "8" = "Employers in small orgs",
  "9" = "Own account workers",
  "10" = "Lower supervisory occupations",
  "11" = "Lower technical craft",
  "12" = "Semi routine",
  "13" = "Routine",
  "14" = "Never worked / Unemployed",
  "15" = "Full-time students",
  "16" = "Not classified or inadequately stated",
  "17" = "Not classifiable for other reasons"
)

# Helper to clean NS-SEC variables
clean_nssec <- function(var, labels_map) {
  # Convert to numeric first
  x <- as.numeric(var)
  
  # Map -98 (Parent not present) to -3
  x[x == -98] <- -3
  
  # Convert other NAs or missing codes to -3 (as per general guidance for R NA)
  x[is.na(x)] <- -3
  
  # Identify valid substantive responses (>= 1)
  valid_idx <- x >= 1
  
  # For valid responses, take the integer part to collapse to 17 categories
  x[valid_idx] <- floor(x[valid_idx])
  
  # General mapping for the rest of the negatives based on metadata
  x[x == -999] <- -2
  x[x == -99] <- -3
  x[x == -94] <- -8
  
  # Final check: any remaining negatives not handled should be -3
  x[x < 1 & !(x %in% c(-1, -2, -3, -7, -8, -9))] <- -3
  
  # Create label mapping. The keys must be numeric to match the double vector x
  final_labels <- c(
    "-9" = "Refusal",
    "-8" = "Don't know / insufficient information",
    "-7" = "Prefer not to say",
    "-3" = "Not asked / not interviewed / not present",
    "-2" = "Schedule not applicable / script error / information lost",
    "-1" = "Item not applicable"
  )
  # Add the 17 substantive categories
  for(i in 1:17) {
    final_labels[as.character(i)] <- nssec_labels[as.character(i)]
  }
  
  # IMPORTANT: set_value_labels expects a named vector where names are the labels
  # and values are the numeric codes.
  # Current final_labels: names = codes, values = labels. We need to flip them.
  val_lab <- final_labels
  names(val_lab) <- as.numeric(names(val_lab))
  
  # Correct way to use set_value_labels: 
  # The value_labels argument should be a named vector: names are labels, values are codes
  # Let's redefine it clearly:
  label_vec <- c(
    "Refusal" = -9,
    "Don't know / insufficient information" = -8,
    "Prefer not to say" = -7,
    "Not asked / not interviewed / not present" = -3,
    "Schedule not applicable / script error / information lost" = -2,
    "Item not applicable" = -1
  )
  for(i in 1:17) {
    label_vec[nssec_labels[as.character(i)]] <- i
  }
  
  x <- set_value_labels(x, label_vec)
  return(x)
}

# Load files
files <- c(
  "wave_one_lsype_family_background_2020.tab",
  "wave_two_lsype_family_background_2020.tab",
  "wave_three_lsype_family_background_2020.tab",
  "wave_four_lsype_family_background_2020.tab",
  "wave_five_lsype_family_background_2020.tab"
)

data_list <- lapply(files, function(f) {
  readr::read_delim(paste0("data/input/", f), delim = "\t", show_col_types = FALSE)
})

names(data_list) <- c("w1", "w2", "w3", "w4", "w5")

# Merge datasets
final_df <- data_list[[1]] %>%
  full_join(data_list[[2]], by = "NSID") %>%
  full_join(data_list[[3]], by = "NSID") %>%
  full_join(data_list[[4]], by = "NSID") %>%
  full_join(data_list[[5]], by = "NSID")

# Define the variables to be cleaned
var_map <- list(
  nssecma14 = "W1nsseccatmum",
  nssecpa14 = "W1nsseccatdad",
  nssecma15 = "W2nsseccatmum",
  nssecpa15 = "W2nsseccatdad",
  nssecma16 = "W3cnsseccatmum",
  nssecpa16 = "W3cnsseccatdad",
  nssecma17 = "w4cnsseccatmum",
  nssecpa17 = "w4cnsseccatdad",
  nssecma18 = "w5Cnsseccatmum",
  nssecpa18 = "w5Cnsseccatdad"
)

# Process variables
output_df <- data.frame(NSID = final_df$NSID)

for (out_name in names(var_map)) {
  src_name <- var_map[[out_name]]
  output_df[[out_name]] <- clean_nssec(final_df[[src_name]], nssec_labels)
}

# Write to CSV
readr::write_csv(output_df, "data/output/cleaned_data.csv")

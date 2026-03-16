library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define input file paths
input_files <- list(
  wave1 = "data/input/wave_one_lsype_family_background_2020.tab",
  wave2 = "data/input/wave_two_lsype_family_background_2020.tab",
  wave4 = "data/input/wave_four_lsype_family_background_2020.tab"
)

# Define collapsed mapping
collapse_map <- setNames(c(0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,2,2,2,3,4), c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))

# Define labels for collapsed variable - all as character strings
collapse_labels_str <- c(
  "-99" = "Refused",
  "-98" = "Not applicable",
  "-94" = "Insufficient information",
  "-92" = "Refused",
  "-91" = "Not applicable",
  "-1" = "Not asked",
  "0" = "NVQ 4-5: degree-level qualifications and above",
  "1" = "NVQ 1-3: sub-degree qualifications",
  "2" = "None/entry: training programmes below NVQ level",
  "3" = "Other: qualifications where the level is unspecified",
  "4" = "No qualifications mentioned"
)

# Detailed labels for W1/W2 (27 labels as character strings)
dtl_labels_str <- c(
  "-999" = "Missing - household data lost",
  "-99" = "Refused",
  "-98" = "Not applicable",
  "-94" = "Insufficient information",
  "-92" = "Refused",
  "-91" = "Not applicable",
  "-1" = "Don't know",
  "1" = "Higher Degree",
  "2" = "First Degree",
  "3" = "HE Diploma",
  "4" = "HNC/HND/NVQ4",
  "5" = "Teaching qualification, non-degree",
  "6" = "Nursing qualification, non-degree",
  "7" = "A Levels",
  "8" = "OND/ONC",
  "9" = "City and guilds part III, NVQ3",
  "10" = "CSYS",
  "11" = "Scottish Higher Grade",
  "12" = "AS Level",
  "13" = "Trade apprenticeship",
  "14" = "City and guilds part II, NVQ2",
  "15" = "GCSE grade A-C and equivalent",
  "16" = "GCSE grade D-E and equivalent",
  "17" = "City and guilds part I, NVQ1",
  "18" = "Youth training, skill seekers",
  "19" = "Qualification, level unspecified",
  "20" = "No qualification mentioned"
)

# Universal negative codes
universal_neg_codes <- c(-999, -99, -98, -94, -92, -91, -1)

# Function to consolidate detailed education value across waves
consolidate_detailed <- function(x) {
  valid_idx <- !is.na(x)
  if (sum(valid_idx) == 0) return(as.integer(-3))
  vals <- x[valid_idx]
  pos_vals <- vals[vals %in% 1:20]
  if (length(pos_vals) > 0) return(as.integer(pos_vals[1]))
  neg_vals <- vals[vals %in% universal_neg_codes]
  if (length(neg_vals) > 0) return(as.integer(neg_vals[1]))
  return(as.integer(-3))
}

# Function to harmonize missing codes to standard scheme
harmonize_code <- function(x) {
  result <- x
  result[result %in% c(-999, -92)] <- -2
  result[result %in% c(-98, -99, -91)] <- -1
  result[result %in% -94] <- -2
  result[result %in% -1] <- -3
  result[is.na(result)] <- -3
  return(result)
}

# Load waves
cat("Loading data files...\n")
wave1 <- read_delim(input_files$wave1, delim = "\t")
wave2 <- read_delim(input_files$wave2, delim = "\t")
wave4 <- read_delim(input_files$wave4, delim = "\t")
cat("Loaded wave1:", nrow(wave1), "rows\n")
cat("Loaded wave2:", nrow(wave2), "rows\n")
cat("Loaded wave4:", nrow(wave4), "rows\n")

# Get all unique NSIDs
all_nsid <- union(wave1$NSID, union(wave2$NSID, wave4$NSID))
cat("Total unique NSIDs:", length(all_nsid), "\n")

# Consolidate maternal education
maternal_detailed <- sapply(all_nsid, function(nsid) {
  vals <- c(wave1$W1hiqualmum[wave1$NSID == nsid],
            wave2$W2hiqualmum[wave2$NSID == nsid],
            wave4$w4hiqualmum[wave4$NSID == nsid])
  consolidate_detailed(vals)
})

# Consolidate paternal education  
paternal_detailed <- sapply(all_nsid, function(nsid) {
  vals <- c(wave1$W1hiqualdad[wave1$NSID == nsid],
            wave2$W2hiqualdad[wave2$NSID == nsid],
            wave4$w4hiqualdad[wave4$NSID == nsid])
  consolidate_detailed(vals)
})

cat("Consolidation complete.\n")

# Apply harmonization
maternal_harmonized <- harmonize_code(maternal_detailed)
paternal_harmonized <- harmonize_code(paternal_detailed)

# Create collapsed variables
maternal_collapsed <- as.integer(maternal_harmonized)
paternal_collapsed <- as.integer(paternal_harmonized)

# Apply collapse mapping
maternal_collapsed[maternal_harmonized %in% 1:20] <- collapse_map[maternal_harmonized[maternal_harmonized %in% 1:20]]
paternal_collapsed[paternal_harmonized %in% 1:20] <- collapse_map[paternal_harmonized[paternal_harmonized %in% 1:20]]

cat("Collapse mapping applied.\n")

# Create final dataframe
final_data <- data.frame(NSID = all_nsid,
                         educma = maternal_collapsed,
                         educpa = paternal_collapsed,
                         educdtlma = maternal_detailed,
                         educdtlpa = paternal_detailed)

cat("Dataframe created.\n")

# Convert to factors with labels
all_levels_collapsed <- c(-99,-98,-94,-92,-91,-1,0,1,2,3,4)
final_data$educma <- factor(final_data$educma, levels = all_levels_collapsed)
levels(final_data$educma) <- collapse_labels_str[as.character(all_levels_collapsed)]

all_levels_collapsed_p <- c(-99,-98,-94,-92,-91,-1,0,1,2,3,4)
final_data$educpa <- factor(final_data$educpa, levels = all_levels_collapsed_p)
levels(final_data$educpa) <- collapse_labels_str[as.character(all_levels_collapsed_p)]

# For detailed variables
dtl_levels <- c("-999","-99","-98","-94","-92","-91","-1","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20")
final_data$educdtlma <- factor(final_data$educdtlma, levels = dtl_levels)
levels(final_data$educdtlma) <- dtl_labels_str[dtl_levels]

dtl_levels_p <- c("-999","-99","-98","-94","-92","-91","-1","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20")
final_data$educdtlpa <- factor(final_data$educdtlpa, levels = dtl_levels_p)
levels(final_data$educdtlpa) <- dtl_labels_str[dtl_levels_p]

cat("Factors created.\n")

# Write output
cat("\nWriting output to data/output/cleaned_data.csv...\n")
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(final_data, "data/output/cleaned_data.csv")
cat("Output written successfully!\n")

cat("\n--- Summary ---\n")
cat("Total cases:", nrow(final_data), "\n")
cat("Variables:", names(final_data), "\n")
cat("educma distribution:\n", table(final_data$educma), "\n")
cat("educma levels:", levels(final_data$educma), "\n")
cat("educdtlma unique values:", unique(final_data$educdtlma), "\n")

cat("\nDone!\n")

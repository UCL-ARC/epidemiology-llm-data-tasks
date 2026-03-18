
# Load data using base R functions
wave1 <- read.table("data/input/wave_one_lsype_family_background_2020.tab", header=TRUE, sep="\t")
wave2 <- read.table("data/input/wave_two_lsype_family_background_2020.tab", header=TRUE, sep="\t")
wave4 <- read.table("data/input/wave_four_lsype_family_background_2020.tab", header=TRUE, sep="\t")

# Merge datasets
merged_data <- merge(wave1, wave2, by="NSID", all=TRUE)
merged_data <- merge(merged_data, wave4, by="NSID", all=TRUE)

# Consolidation function
consolidate_education <- function(w1, w2, w4) {
  vals <- c(w1, w2, w4)
  vals <- vals[!is.na(vals)]
  pos <- vals[vals > 0]
  if (length(pos) > 0) return(pos[1])
  neg <- vals[vals <= 0]
  if (length(neg) > 0) return(neg[1])
  return(NA)
}

# Apply consolidation
merged_data$educdtlma <- mapply(consolidate_education,
                               merged_data$W1hiqualmum,
                               merged_data$W2hiqualmum,
                               merged_data$w4hiqualmum)

merged_data$educdtlpa <- mapply(consolidate_education,
                               merged_data$W1hiqualdad,
                               merged_data$W2hiqualdad,
                               merged_data$w4hiqualdad)

# Handle missing values
merged_data$educdtlma[is.na(merged_data$educdtlma)] <- -3
merged_data$educdtlpa[is.na(merged_data$educdtlpa)] <- -3

# Collapse education levels
collapse_education <- function(x) {
  if (is.na(x)) return(NA)
  if (x <= 0) return(x)
  if (x %in% c(1,2,3,4)) return(0)
  if (x %in% c(5:17)) return(1)
  if (x == 18) return(2)
  if (x == 19) return(3)
  return(4)
}

# Apply collapse function
merged_data$educma <- sapply(merged_data$educdtlma, collapse_education)
merged_data$educpa <- sapply(merged_data$educdtlpa, collapse_education)

# Select final columns
final_vars <- merged_data[, c("NSID", "educma", "educpa", "educdtlma", "educdtlpa")]

# Create label vectors
detailed_labels <- c(
  "-999" = "Missing - household data lost",
  "-99" = "Parent not interviewed",
  "-98" = "Parent not present",
  "-94" = "Insufficient information",
  "-92" = "Refused",
  "-91" = "Not applicable",
  "-3" = "Not asked",
  "-1" = "Don't know",
  "1" = "Higher Degree", "2" = "First Degree", "3" = "HE Diploma",
  "4" = "HNC/HND/NVQ4", "5" = "Teaching qualification",
  "6" = "Nursing qualification", "7" = "A Levels",
  "8" = "OND/ONC", "9" = "City and Guilds Part III",
  "10" = "CSYS", "11" = "Scottish Higher Grade",
  "12" = "AS Level", "13" = "Trade Apprenticeship",
  "14" = "City and Guilds Part II", "15" = "GCSE A-C",
  "16" = "GCSE D-E", "17" = "City and Guilds Part I",
  "18" = "Youth Training", "19" = "Unspecified", "20" = "No qualification"
)

collapsed_labels <- c(
  "-999" = "Missing - household data lost",
  "-99" = "Parent not interviewed",
  "-98" = "Parent not present",
  "-94" = "Insufficient information",
  "-92" = "Refused",
  "-91" = "Not applicable",
  "-3" = "Not asked",
  "-1" = "Don't know",
  "0" = "NVQ 4-5: Degree level", "1" = "NVQ 1-3: Sub-degree",
  "2" = "Below NVQ", "3" = "Unspecified", "4" = "No qualifications"
)

# Convert to factors with labels
final_vars$educdtlma <- factor(final_vars$educdtlma,
                              levels = as.numeric(names(detailed_labels)),
                              labels = detailed_labels)

final_vars$educdtlpa <- factor(final_vars$educdtlpa,
                              levels = as.numeric(names(detailed_labels)),
                              labels = detailed_labels)

final_vars$educma <- factor(final_vars$educma,
                           levels = as.numeric(names(collapsed_labels)),
                           labels = collapsed_labels)

final_vars$educpa <- factor(final_vars$educpa,
                           levels = as.numeric(names(collapsed_labels)),
                           labels = collapsed_labels)

# Write output
write.csv(final_vars, "data/output/cleaned_data.csv", row.names=FALSE)

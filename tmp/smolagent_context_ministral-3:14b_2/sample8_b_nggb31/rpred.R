
# Load required packages
library(haven)
library(dplyr)
library(readr)

# Define file paths
file_paths <- list(
  wave8_main = "data/input/ns8_2015_main_interview.tab",
  wave8_derived = "data/input/ns8_2015_derived.tab",
  wave9_main = "data/input/ns9_2022_main_interview.tab",
  wave9_derived = "data/input/ns9_2022_derived_variables.tab"
)

# Load datasets with explicit column types
wave8_main <- read_delim(file_paths$wave8_main, delim = "\t", col_types = cols(NSID = col_character(), .default = col_double()))
wave8_derived <- read_delim(file_paths$wave8_derived, delim = "\t", col_types = cols(NSID = col_character(), .default = col_double()))
wave9_main <- read_delim(file_paths$wave9_main, delim = "\t", col_types = cols(NSID = col_character(), .default = col_double()))
wave9_derived <- read_delim(file_paths$wave9_derived, delim = "\t", col_types = cols(NSID = col_character(), .default = col_double()))

# Merge datasets by NSID
merged_data <- full_join(wave8_main, wave8_derived, by = "NSID")
merged_data <- full_join(merged_data, wave9_main, by = "NSID")
merged_data <- full_join(merged_data, wave9_derived, by = "NSID")

# Define NVQ level mapping for wave 8 vocational qualifications
vocational_nvq_map <- data.frame(
  variable = c(
    "W8VCQU0I", "W8VCQU0J", "W8VCQU0K", "W8VCQU0L", "W8VCQU0M",
    "W8VCQU0O", "W8VCQU0F", "W8VCQU0G", "W8VCQU0H", "W8VCQU0C",
    "W8VCQU0B", "W8VCQU0A", "W8VCQU0D", "W8VCQU0E"
  ),
  nvq_level = c(
    1, 4, 4, 4, 4, 3, 3, 3, 3, 2,
    2, 1, 1, 1
  ),
  stringsAsFactors = FALSE
)

# Define academic qualification labels for wave 9
academic_labels <- data.frame(
  variable = c(
    "W9ACQU0A", "W9ACQU0B", "W9ACQU0C", "W9ACQU0D", "W9ACQU0E",
    "W9ACQU0F", "W9ACQU0G", "W9ACQU0H", "W9ACQU0I", "W9ACQU0J",
    "W9ACQU0K", "W9ACQU0L", "W9ACQU0M", "W9ACQU0N", "W9ACQU0O",
    "W9ACQU0P", "W9ACQU0Q", "W9ACQU0R", "W9ACQU0S", "W9ACQU0T",
    "W9ACQU0U", "W9ACQU0V"
  ),
  label = c(
    "Doctorate or equivalent", "Masters or equivalent", "Undergraduate or equivalent",
    "Post-graduate Diplomas and Certificates", "Diplomas in higher education and other higher education qualifications",
    "Teaching qualifications for schools or further education (below degree level)",
    "A/AS Levels or equivalent", "Grade A-C, Level 4-9", "Grade D-G, Level 1-3",
    "SCE Higher", "Scottish Certificate Sixth Year Studies", "SCE Standard",
    "National 4 and 5", "National 2 and 3", "Leaving Certificate",
    "Junior Certificate grade A-C", "Junior Certificate grade D and below",
    "Other academic qualifications (including overseas)", "None of these qualifications",
    "Don't know", "Refused", "No answer"
  ),
  stringsAsFactors = FALSE
)

# Define vocational qualification labels for wave 9
vocational_labels <- data.frame(
  variable = c(
    "W9VCQU0A", "W9VCQU0B", "W9VCQU0C", "W9VCQU0D", "W9VCQU0E",
    "W9VCQU0F", "W9VCQU0G", "W9VCQU0H", "W9VCQU0I", "W9VCQU0J",
    "W9VCQU0K", "W9VCQU0L", "W9VCQU0M", "W9VCQU0N", "W9VCQU0O",
    "W9VCQU0P", "W9VCQU0Q", "W9VCQU0R", "W9VCQU0S", "W9VCQU0T",
    "W9VCQU0U", "W9VCQU0V", "W9VCQU0W", "W9VCQU0X", "W9VCQU0Y",
    "W9VCQU0Z", "W9VCQUAA", "W9VCQUAB", "W9VCQUAC", "W9VCQUAD",
    "W9VCQUAE", "W9VCQUAF", "W9VCQUAG", "W9VCQUAH", "W9VCQUAI"
  ),
  label = c(
    "Professional qualifications at degree level", "Nursing or other medical qualifications (below degree level)",
    "Level 4 or 5", "Level 3", "Level 2", "Level 1", "GNVQ Advanced",
    "GNVQ Intermediate", "Level 3", "Level 2", "Level Foundation",
    "Advanced Craft, Part III", "Craft, Part II", "Craft, Part I", "Level 3",
    "Level 2", "Level 1", "Advanced Diploma", "Higher Diploma", "RSA Diploma",
    "RSA Stage I, II, III", "Higher Level BTEC", "BTEC National", "BTEC First",
    "SCOTVEC National Certificate", "SCOTVEC first or general diploma", "SCOTVEC general diploma",
    "SCOTVEC modules", "HND or HNC", "OND or ONCM", "Junior certificate",
    "Other vocational qualifications (including some overseas)", "None of these qualifications",
    "Don't know", "Refused"
  ),
  stringsAsFactors = FALSE
)

# Create a function to derive educ25
derive_educ25 <- function(row) {
  academic_nvq <- row$W8DHANVQH

  # Get vocational NVQ levels
  vocational_vars <- row %>% select(matches("W8VCQU"))
  vocational_nvq <- sapply(names(vocational_vars), function(var) {
    level <- vocational_nvq_map$nvq_level[vocational_nvq_map$variable == var]
    if (!is.null(level)) {
      if (row[[var]] == 1) level else NA
    } else NA
  })

  combined_nvq <- pmax(academic_nvq, vocational_nvq, na.rm = TRUE)

  # Convert to numeric codes first
  educ25_code <- case_when(
    combined_nvq %in% c(4, 5, 95) ~ 0,
    combined_nvq %in% c(1, 2, 3) ~ 1,
    combined_nvq == 0 ~ 2,
    combined_nvq == 96 ~ 4,
    combined_nvq %in% c(-9, -8, -3, -2, -1) ~ combined_nvq,
    TRUE ~ 3
  )

  # Convert to factor with explicit levels
  factor(educ25_code,
         levels = c(-9, -8, -3, -2, -1, 0, 1, 2, 3, 4),
         labels = c(
           "Refused", "Don't know", "Not asked", "Not applicable", "No answer",
           "NVQ 4-5 equivalent qualifications", "NVQ 1-3 equivalent qualifications",
           "Entry level or no qualifications", "Other qualifications not mappable to NVQ framework",
           "None of these qualifications"
         ))
}

# Create a function to derive educ32
derive_educ32 <- function(row) {
  combined_nvq <- pmax(row$W9DANVQH, row$W9DVNVQH, na.rm = TRUE)

  # Convert to numeric codes first
  educ32_code <- case_when(
    combined_nvq %in% c(4, 5, 95) ~ 0,
    combined_nvq %in% c(1, 2, 3) ~ 1,
    combined_nvq == 0 ~ 2,
    combined_nvq == 96 ~ 4,
    combined_nvq %in% c(-9, -8, -3, -2, -1) ~ combined_nvq,
    TRUE ~ 3
  )

  # Convert to factor with explicit levels
  factor(educ32_code,
         levels = c(-9, -8, -3, -2, -1, 0, 1, 2, 3, 4),
         labels = c(
           "Refused", "Don't know", "Not asked", "Not applicable", "No answer",
           "NVQ 4-5 equivalent qualifications", "NVQ 1-3 equivalent qualifications",
           "Entry level or no qualifications", "Other qualifications not mappable to NVQ framework",
           "None of these qualifications"
         ))
}

# Create a function to derive educadtl32 with proper handling
derive_educadtl32 <- function(row) {
  acqu_vars <- row %>% select(matches("W9ACQU"))

  # Check for any valid responses (1)
  valid_responses <- which(acqu_vars == 1 & !is.na(acqu_vars))

  if (length(valid_responses) > 0) {
    var_name <- names(acqu_vars)[valid_responses[1]]
    label <- academic_labels$label[academic_labels$variable == var_name]
    if (length(label) > 0) {
      return(label)
    } else {
      return("None of these qualifications")
    }
  } else {
    # Check if all responses are NA or missing
    if (all(is.na(acqu_vars))) {
      return(NA_character_)
    } else {
      return("None of these qualifications")
    }
  }
}

# Create a function to derive educvdtl32 with proper handling
derive_educvdtl32 <- function(row) {
  vcqu_vars <- row %>% select(matches("W9VCQU"))

  # Check for any valid responses (1)
  valid_responses <- which(vcqu_vars == 1 & !is.na(vcqu_vars))

  if (length(valid_responses) > 0) {
    var_name <- names(vcqu_vars)[valid_responses[1]]
    label <- vocational_labels$label[vocational_labels$variable == var_name]
    if (length(label) > 0) {
      return(label)
    } else {
      return("None of these qualifications")
    }
  } else {
    # Check if all responses are NA or missing
    if (all(is.na(vcqu_vars))) {
      return(NA_character_)
    } else {
      return("None of these qualifications")
    }
  }
}

# Initialize output data frame
final_data <- data.frame(
  NSID = character(),
  educ25 = factor(),
  educ32 = factor(),
  educadtl32 = character(),
  educvdtl32 = character(),
  stringsAsFactors = FALSE
)

# Process each row individually
for (i in 1:nrow(merged_data)) {
  row <- merged_data[i, ]

  tryCatch({
    final_data <- rbind(
      final_data,
      data.frame(
        NSID = row$NSID,
        educ25 = derive_educ25(row),
        educ32 = derive_educ32(row),
        educadtl32 = derive_educadtl32(row),
        educvdtl32 = derive_educvdtl32(row)
      )
    )
  }, error = function(e) {
    # If any error occurs, store NA values for this row
    final_data <- rbind(
      final_data,
      data.frame(
        NSID = row$NSID,
        educ25 = NA,
        educ32 = NA,
        educadtl32 = NA_character_,
        educvdtl32 = NA_character_
      )
    )
  })
}

# Write output to file
write_csv(final_data, "data/output/cleaned_data.csv")

# Print success message
message("Data cleaning and harmonization completed successfully. Output saved to data/output/cleaned_data.csv")

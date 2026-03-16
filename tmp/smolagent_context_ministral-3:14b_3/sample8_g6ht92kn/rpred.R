# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
file_paths <- list(
  wave8_main = "data/input/ns8_2015_main_interview.tab",
  wave8_derived = "data/input/ns8_2015_derived.tab",
  wave9_main = "data/input/ns9_2022_main_interview.tab",
  wave9_derived = "data/input/ns9_2022_derived_variables.tab"
)

# Load datasets
wave8_main <- read_delim(file_paths$wave8_main, delim = "\t", col_types = cols(NSID = col_character()))
wave8_derived <- read_delim(file_paths$wave8_derived, delim = "\t", col_types = cols(NSID = col_character()))
wave9_main <- read_delim(file_paths$wave9_main, delim = "\t", col_types = cols(NSID = col_character()))
wave9_derived <- read_delim(file_paths$wave9_derived, delim = "\t", col_types = cols(NSID = col_character()))

# Merge datasets by NSID
merged_data <- full_join(wave8_main, wave8_derived, by = "NSID") %>%
  full_join(wave9_main, by = "NSID") %>%
  full_join(wave9_derived, by = "NSID")

# Define NVQ level mapping for vocational qualifications at age 25
vocational_nvq_mapping <- data.frame(
  variable = c("W8VCQU0I", "W8VCQU0J", "W8VCQU0K", "W8VCQU0L", "W8VCQU0M", "W8VCQU0N", "W8VCQU0O",
              "W8VCQU0G", "W8VCQU0H", "W8VCQU0F", "W8VCQU0E", "W8VCQU0C", "W8VCQU0B", "W8VCQU0A", "W8VCQU0D"),
  nvq_level = c(1, 4, 4, 4, 4, 4, 3, 1, 1, 1, 1, 1, 1, 1, 1),
  stringsAsFactors = FALSE
)

# Function to map vocational qualifications to NVQ levels
map_vocational_to_nvq <- function(data) {
  vocational_vars <- data %>% select(starts_with("W8VCQU"))
  max_nvq_level <- numeric(nrow(vocational_vars))

  for (i in 1:nrow(vocational_vars)) {
    valid_vars <- which(!is.na(vocational_vars[i, ]))
    var_names <- names(vocational_vars)[valid_vars]
    selected_vars <- vocational_vars[i, valid_vars] == 1

    if (any(selected_vars)) {
      selected_var <- var_names[which(selected_vars)[1]]
      match_idx <- which(vocational_nvq_mapping$variable == selected_var)
      if (length(match_idx) > 0) {
        max_nvq_level[i] <- vocational_nvq_mapping$nvq_level[match_idx]
      } else {
        max_nvq_level[i] <- NA_integer_
      }
    } else {
      max_nvq_level[i] <- NA_integer_
    }
  }

  return(max_nvq_level)
}

# Create educ25 variable
merged_data <- merged_data %>%
  mutate(
    educ25 = case_when(
      W8DHANVQH %in% c(4, 5) ~ 0,
      W8DHANVQH %in% c(1, 2, 3) ~ 1,
      W8DHANVQH == 0 ~ 2,
      W8DHANVQH == 95 ~ 3,
      W8DHANVQH == 96 ~ 4,
      TRUE ~ NA_integer_
    )
  )

# Map vocational qualifications to NVQ levels
vocational_nvq_levels <- map_vocational_to_nvq(merged_data)

# Update educ25 based on vocational qualifications
merged_data$educ25 <- ifelse(
  !is.na(vocational_nvq_levels) & vocational_nvq_levels >= 4, 0,
  ifelse(
    !is.na(vocational_nvq_levels) & vocational_nvq_levels %in% c(1, 2, 3), 1,
    ifelse(
      !is.na(vocational_nvq_levels) & vocational_nvq_levels == 0, 2,
      merged_data$educ25
    )
  )
)

# Define NVQ level mapping for academic and vocational qualifications at age 32
academic_nvq_mapping <- data.frame(
  value = c(0, 1, 2, 3, 4, 5, 95, 96),
  nvq_level = c(2, 1, 2, 3, 4, 4, 3, 4),
  stringsAsFactors = FALSE
)

vocational_nvq_mapping_32 <- data.frame(
  value = c(0, 1, 2, 3, 4, 5, 95, 96),
  nvq_level = c(2, 1, 2, 3, 4, 4, 3, 4),
  stringsAsFactors = FALSE
)

# Create educ32 variable
merged_data <- merged_data %>%
  mutate(
    educ32_academic = academic_nvq_mapping$nvq_level[match(W9DANVQH, academic_nvq_mapping$value)],
    educ32_vocational = vocational_nvq_mapping_32$nvq_level[match(W9DVNVQH, vocational_nvq_mapping_32$value)]
  )

# Determine the highest NVQ level for educ32
merged_data <- merged_data %>%
  mutate(
    educ32 = case_when(
      !is.na(educ32_academic) & !is.na(educ32_vocational) ~ pmax(educ32_academic, educ32_vocational),
      !is.na(educ32_academic) ~ educ32_academic,
      !is.na(educ32_vocational) ~ educ32_vocational,
      TRUE ~ NA_integer_
    )
  )

# Collapse NVQ levels for educ32
merged_data <- merged_data %>%
  mutate(
    educ32 = case_when(
      educ32 == 4 ~ 0,  # NVQ Level 4 or 5
      educ32 == 3 ~ 1,  # NVQ Level 3
      educ32 == 2 ~ 1,  # NVQ Level 2
      educ32 == 1 ~ 1,  # NVQ Level 1
      educ32 == 0 ~ 2,  # NVQ Entry Level
      TRUE ~ NA_integer_
    )
  )

# Define academic qualification labels for age 32
academic_labels <- data.frame(
  variable = c("W9ACQU0A", "W9ACQU0B", "W9ACQU0C", "W9ACQU0D", "W9ACQU0E", "W9ACQU0F", "W9ACQU0G", "W9ACQU0H", "W9ACQU0I",
              "W9ACQU0J", "W9ACQU0K", "W9ACQU0L", "W9ACQU0M", "W9ACQU0N", "W9ACQU0O", "W9ACQU0P", "W9ACQU0Q", "W9ACQU0R", "W9ACQU0S"),
  label = c("Doctorate or equivalent", "Masters or equivalent", "Undergraduate or equivalent", "Post-graduate Diplomas and Certificates",
           "Diplomas in higher education and other higher education qualifications", "Teaching qualifications for schools or further education (below degree level)",
           "A/AS Levels or equivalent", "Grade A-C, Level 4-9", "Grade D-G, Level 1-3", "SCE Higher", "Scottish Certificate Sixth Year Studies",
           "SCE Standard", "National 4 and 5", "National 2 and 3", "Leaving Certificate", "Junior Certificate grade A-C", "Junior Certificate grade D and below",
           "Other academic qualifications (including overseas)", "None of these qualifications"),
  stringsAsFactors = FALSE
)

# Function to derive educadtl32
derive_educadtl32 <- function(data) {
  data %>%
    rowwise() %>%
    mutate(
      educadtl32 = ifelse(
        any(c_across(matches("W9ACQU")) == 1),
        academic_labels$label[match(names(.)[which(c_across(matches("W9ACQU")) == 1)[1]], academic_labels$variable)],
        "None of these qualifications"
      )
    ) %>%
    ungroup()
}

# Derive educadtl32
merged_data <- derive_educadtl32(merged_data)

# Define vocational qualification labels for age 32
vocational_labels <- data.frame(
  variable = c("W9VCQU0A", "W9VCQU0B", "W9VCQU0C", "W9VCQU0D", "W9VCQU0E", "W9VCQU0F", "W9VCQU0G", "W9VCQU0H", "W9VCQU0I",
              "W9VCQU0J", "W9VCQU0K", "W9VCQU0L", "W9VCQU0M", "W9VCQU0N", "W9VCQU0O", "W9VCQU0P", "W9VCQU0Q", "W9VCQU0R",
              "W9VCQU0S", "W9VCQU0T", "W9VCQU0U", "W9VCQU0V", "W9VCQU0W", "W9VCQU0X", "W9VCQU0Y", "W9VCQU0Z", "W9VCQUAA",
              "W9VCQUAB", "W9VCQUAC", "W9VCQUAD", "W9VCQUAE", "W9VCQUAF", "W9VCQUAG"),
  label = c("Professional qualifications at degree level", "Nursing or other medical qualifications (below degree level)",
            "Level 4 or 5", "Level 3", "Level 2", "Level 1", "GNVQ Advanced", "GNVQ Intermediate", "Level 3",
            "Level 2", "Level Foundation", "Advanced Craft, Part III", "Craft, Part II", "Craft, Part I", "Level 3",
            "Level 2", "Level 1", "Advanced Diploma", "Higher Diploma", "RSA Diploma", "RSA Stage I, II, III",
            "Higher Level BTEC", "BTEC National", "BTEC First", "SCOTVEC National Certificate", "SCOTVEC first or general diploma",
            "SCOTVEC general diploma", "SCOTVEC modules", "HND or HNC", "OND or ONCM", "Junior certificate",
            "Other vocational qualifications (including some overseas)", "None of these qualifications"),
  stringsAsFactors = FALSE
)

# Function to derive educvdtl32
derive_educvdtl32 <- function(data) {
  data %>%
    rowwise() %>%
    mutate(
      educvdtl32 = ifelse(
        any(c_across(matches("W9VCQU")) == 1),
        vocational_labels$label[match(names(.)[which(c_across(matches("W9VCQU")) == 1)[1]], vocational_labels$variable)],
        "None of these qualifications"
      )
    ) %>%
    ungroup()
}

# Derive educvdtl32
merged_data <- derive_educvdtl32(merged_data)

# Clean up intermediate variables
final_data <- merged_data %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Define factor levels for educ25 and educ32
educ_level_labels <- c(
  "NVQ 4–5 equivalent qualifications" = 0,
  "NVQ 1–3 equivalent qualifications" = 1,
  "Entry level or no qualifications" = 2,
  "Other qualifications not mappable to NVQ framework" = 3,
  "None of these qualifications" = 4
)

# Apply factor levels
final_data <- final_data %>%
  mutate(
    educ25 = factor(educ25, levels = c(0, 1, 2, 3, 4),
                   labels = educ_level_labels),
    educ32 = factor(educ32, levels = c(0, 1, 2, 3, 4),
                   labels = educ_level_labels)
  )

# Write output to file
write_csv(final_data, "data/output/cleaned_data.csv")

# Print success message
message("Cleaned data has been successfully written to data/output/cleaned_data.csv")
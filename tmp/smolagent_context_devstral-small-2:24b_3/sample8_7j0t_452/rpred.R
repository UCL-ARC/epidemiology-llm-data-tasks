library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave8_main <- readr::read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave8_derived <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9_main <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
wave9_derived <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets
merged_data <- wave8_main %>% 
  full_join(wave8_derived, by = "NSID") %>% 
  full_join(wave9_main, by = "NSID") %>% 
  full_join(wave9_derived, by = "NSID")

# Create educ25 - Collapsed NVQ level at age ~25 (wave 8)
# First, create a function to map vocational qualifications to NVQ levels
map_vocational_to_nvq <- function(row) {
  voc_vars <- c("W8VCQU0A", "W8VCQU0B", "W8VCQU0C", "W8VCQU0D", "W8VCQU0E", 
                "W8VCQU0F", "W8VCQU0G", "W8VCQU0H", "W8VCQU0I", "W8VCQU0J", 
                "W8VCQU0K", "W8VCQU0L", "W8VCQU0M", "W8VCQU0N", "W8VCQU0O", 
                "W8VCQU0P", "W8VCQU0Q", "W8VCQU0R")
  
  nvq_mapping <- list(
    "Youth training certificate" = 2,
    "Key Skills" = 2,
    "Basic skills" = 1,
    "Entry level qualifications (Wales)" = 0,
    "Modern apprenticeship/trade apprenticeship" = 3,
    "RSA/OCR/Clerical and commercial qualifications" = 2,
    "City and Guilds Certificate" = 2,
    "GNVQ/GSVQ" = 3,
    "NVQ/SVQ - Level 1 - 2" = 2,
    "NVQ/SVQ - Level 3 - 5" = 4,
    "HNC/HND" = 4,
    "ONC/OND" = 3,
    "BTEC/BEC/TEC/EdExcel/LQL" = 3,
    "SCOTVEC, SCOTEC or SCOTBEC" = 3,
    "Other vocational, technical or professional" = 3,
    "None of the above" = 4
  )
  
  max_nvq <- -3
  for (var in voc_vars) {
    if (!is.na(row[[var]]) && !is.null(row[[var]]) && row[[var]] == 1) {
      var_label <- attr(row[[var]], "label")
      if (!is.null(var_label) && var_label %in% names(nvq_mapping)) {
        max_nvq <- max(max_nvq, nvq_mapping[[var_label]])
      }
    }
  }
  return(max_nvq)
}

# Apply the function row by row with error handling
educ25_values <- sapply(1:nrow(merged_data), function(i) {
  tryCatch({
    map_vocational_to_nvq(merged_data[i, ])
  }, error = function(e) {
    -3  # Return -3 for missing values
  })
})

merged_data <- merged_data %>% 
  mutate(
    educ25 = case_when(
      W8DHANVQH %in% c(-9, -8, -3, -2, -1) ~ W8DHANVQH,
      TRUE ~ educ25_values
    )
  )

# Create educ32 - Collapsed NVQ level at age ~32 (wave 9)
merged_data <- merged_data %>% 
  mutate(
    educ32 = case_when(
      W9DANVQH %in% c(-9, -8, -3, -2, -1) ~ W9DANVQH,
      W9DVNVQH %in% c(-9, -8, -3, -2, -1) ~ W9DVNVQH,
      TRUE ~ max(W9DANVQH, W9DVNVQH, na.rm = TRUE)
    )
  )

# Create educadtl32 - Detailed academic qualifications at age 32 (wave 9)
academic_vars <- c("W9ACQU0A", "W9ACQU0B", "W9ACQU0C", "W9ACQU0D", "W9ACQU0E", 
                   "W9ACQU0F", "W9ACQU0G", "W9ACQU0H", "W9ACQU0I", "W9ACQU0J", 
                   "W9ACQU0K", "W9ACQU0L", "W9ACQU0M", "W9ACQU0N", "W9ACQU0O", 
                   "W9ACQU0P", "W9ACQU0Q", "W9ACQU0R", "W9ACQU0S", "W9ACQU0T", 
                   "W9ACQU0U", "W9ACQU0V")

academic_labels <- list(
  "W9ACQU0A" = "Doctorate or equivalent",
  "W9ACQU0B" = "Masters or equivalent",
  "W9ACQU0C" = "Undergraduate or equivalent",
  "W9ACQU0D" = "Post-graduate Diplomas and Certificates",
  "W9ACQU0E" = "Diplomas in higher education and other higher education qualifications",
  "W9ACQU0F" = "Teaching qualifications for schools or further education (below degree level)",
  "W9ACQU0G" = "A/AS Levels or equivalent",
  "W9ACQU0H" = "Grade A-C, Level 4-9",
  "W9ACQU0I" = "Grade D-G, Level 1-3",
  "W9ACQU0J" = "SCE Higher",
  "W9ACQU0K" = "Scottish Certificate Sixth Year Studies",
  "W9ACQU0L" = "SCE Standard",
  "W9ACQU0M" = "National 4 and 5",
  "W9ACQU0N" = "National 2 and 3",
  "W9ACQU0O" = "Leaving Certificate",
  "W9ACQU0P" = "Junior Certificate grade A-C",
  "W9ACQU0Q" = "Junior Certificate grade D and below",
  "W9ACQU0R" = "Other academic qualifications (including overseas)",
  "W9ACQU0S" = "None of these qualifications",
  "W9ACQU0T" = "Don't know",
  "W9ACQU0U" = "Refused",
  "W9ACQU0V" = "No answer"
)

# Find first "Yes" (1) response for each row
educadtl32_values <- sapply(1:nrow(merged_data), function(i) {
  row_data <- merged_data[i, academic_vars]
  yes_positions <- which(!is.na(row_data) & row_data == 1)
  if (length(yes_positions) > 0) {
    selected_var <- academic_vars[yes_positions[1]]
    academic_labels[[selected_var]]
  } else {
    "None of these qualifications"
  }
})

merged_data <- merged_data %>% 
  mutate(educadtl32 = educadtl32_values)

# Create educvdtl32 - Detailed vocational qualifications at age 32 (wave 9)
vocational_vars <- c("W9VCQU0A", "W9VCQU0B", "W9VCQU0C", "W9VCQU0D", "W9VCQU0E", 
                     "W9VCQU0F", "W9VCQU0G", "W9VCQU0H", "W9VCQU0I", "W9VCQU0J", 
                     "W9VCQU0K", "W9VCQU0L", "W9VCQU0M", "W9VCQU0N", "W9VCQU0O", 
                     "W9VCQU0P", "W9VCQU0Q", "W9VCQU0R", "W9VCQU0S", "W9VCQU0T", 
                     "W9VCQU0U", "W9VCQU0V", "W9VCQU0W", "W9VCQU0X", "W9VCQU0Y", 
                     "W9VCQU0Z", "W9VCQUAA", "W9VCQUAB", "W9VCQUAC", "W9VCQUAD", 
                     "W9VCQUAE", "W9VCQUAF", "W9VCQUAG", "W9VCQUAH", "W9VCQUAI")

vocational_labels <- list(
  "W9VCQU0A" = "Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor",
  "W9VCQU0B" = "Nursing or other medical qualifications (below degree level)",
  "W9VCQU0C" = "Level 4 or 5",
  "W9VCQU0D" = "Level 3",
  "W9VCQU0E" = "Level 2",
  "W9VCQU0F" = "Level 1",
  "W9VCQU0G" = "GNVQ Advanced",
  "W9VCQU0H" = "GNVQ Intermediate",
  "W9VCQU0I" = "Level 3",
  "W9VCQU0J" = "Level 2",
  "W9VCQU0K" = "Level Foundation",
  "W9VCQU0L" = "Advanced Craft, Part III",
  "W9VCQU0M" = "Craft, Part II",
  "W9VCQU0N" = "Craft, Part I",
  "W9VCQU0O" = "Level 3",
  "W9VCQU0P" = "Level 2",
  "W9VCQU0Q" = "Level 1",
  "W9VCQU0R" = "Advanced Diploma",
  "W9VCQU0S" = "Higher Diploma",
  "W9VCQU0T" = "RSA Diploma",
  "W9VCQU0U" = "RSA Stage I, II,III",
  "W9VCQU0V" = "Higher Level BTEC",
  "W9VCQU0W" = "BTEC National",
  "W9VCQU0X" = "BTEC First",
  "W9VCQU0Y" = "SCOTVEC National Certificate",
  "W9VCQU0Z" = "SCOTVEC first or general diploma",
  "W9VCQUAA" = "SCOTVEC general diploma",
  "W9VCQUAB" = "SCOTVEC modules",
  "W9VCQUAC" = "HND or  HNC",
  "W9VCQUAD" = "OND or ONCM",
  "W9VCQUAE" = "Junior certificate",
  "W9VCQUAF" = "Other vocational qualifications (including some overseas)",
  "W9VCQUAG" = "None of these qualifications",
  "W9VCQUAH" = "Don't know",
  "W9VCQUAI" = "Refused"
)

# Find first "Yes" (1) response for each row
educvdtl32_values <- sapply(1:nrow(merged_data), function(i) {
  row_data <- merged_data[i, vocational_vars]
  yes_positions <- which(!is.na(row_data) & row_data == 1)
  if (length(yes_positions) > 0) {
    selected_var <- vocational_vars[yes_positions[1]]
    vocational_labels[[selected_var]]
  } else {
    "None of these qualifications"
  }
})

merged_data <- merged_data %>% 
  mutate(educvdtl32 = educvdtl32_values)

# Select final variables
final_data <- merged_data %>% 
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write output
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)
# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_eight_main = "data/input/ns8_2015_main_interview.tab",
  wave_eight_derived = "data/input/ns8_2015_derived.tab",
  wave_nine_main = "data/input/ns9_2022_main_interview.tab",
  wave_nine_derived = "data/input/ns9_2022_derived_variables.tab"
)

# Load all datasets
load_data <- function(file_path) {
  read_delim(file_path, delim = "\t", show_col_types = FALSE)
}

datasets <- map(files, load_data)

# Merge datasets using full_join by NSID
merged_data <- datasets[[1]]
for (i in 2:length(datasets)) {
  merged_data <- full_join(merged_data, datasets[[i]], by = "NSID")
}

# Function to map vocational qualifications to NVQ levels
map_vocational_to_nvq <- function(voc_vars) {
  nvq_mapping <- list(
    "W8VCQU0A" = 2,  # Youth training certificate
    "W8VCQU0B" = 2,  # Key Skills
    "W8VCQU0C" = 2,  # Basic skills
    "W8VCQU0D" = 0,  # Entry level qualifications (Wales)
    "W8VCQU0E" = 3,  # Modern apprenticeship/trade apprenticeship
    "W8VCQU0F" = 2,  # RSA/OCR/Clerical and commercial qualifications
    "W8VCQU0G" = 2,  # City and Guilds Certificate
    "W8VCQU0H" = 3,  # GNVQ/GSVQ
    "W8VCQU0I" = 1,  # NVQ/SVQ - Level 1 - 2
    "W8VCQU0J" = 4,  # NVQ/SVQ - Level 3 - 5
    "W8VCQU0K" = 4,  # HNC/HND
    "W8VCQU0L" = 3,  # ONC/OND
    "W8VCQU0M" = 3,  # BTEC/BEC/TEC/EdExcel/LQL
    "W8VCQU0N" = 3,  # SCOTVEC, SCOTEC or SCOTBEC
    "W8VCQU0O" = 95, # Other vocational, technical or professional
    "W8VCQU0P" = 96, # None of the above
    "W8VCQU0Q" = -8, # Don't know
    "W8VCQU0R" = -9  # Refused
  )
  
  max_nvq <- -3
  for (var in voc_vars) {
    if (var %in% names(nvq_mapping)) {
      val <- merged_data[[var]]
      if (!is.na(val) && val == 1) {
        nvq_level <- nvq_mapping[[var]]
        if (nvq_level > max_nvq) {
          max_nvq <- nvq_level
        }
      }
    }
  }
  return(max_nvq)
}

# Create educ25 variable
merged_data <- merged_data %>% 
  mutate(
    educ25 = case_when(
      !is.na(W8DHANVQH) ~ W8DHANVQH,
      TRUE ~ map_vocational_to_nvq(paste0("W8VCQU", letters[0:17]))
    )
  )

# Collapse educ25 to 5-level categorical variable
educ25_levels <- c(
  "0" = "NVQ 4–5 equivalent qualifications",
  "1" = "NVQ 1–3 equivalent qualifications",
  "2" = "Entry level or no qualifications",
  "3" = "Other qualifications not mappable to NVQ framework",
  "4" = "None of these qualifications"
)

merged_data <- merged_data %>% 
  mutate(
    educ25 = case_when(
      educ25 == 4 | educ25 == 5 ~ 0,
      educ25 == 1 | educ25 == 2 | educ25 == 3 ~ 1,
      educ25 == 0 ~ 2,
      educ25 == 95 ~ 3,
      educ25 == 96 ~ 4,
      educ25 == -9 | educ25 == -8 | educ25 == -3 | educ25 == -2 | educ25 == -1 ~ educ25,
      TRUE ~ -3
    )
  ) %>% 
  mutate(educ25 = factor(educ25, levels = c(-9, -8, -3, -2, -1, 0, 1, 2, 3, 4), labels = c("Refused", "Don't know", "Null", "Not applicable", "No answer", educ25_levels)))

# Create educ32 variable
merged_data <- merged_data %>% 
  mutate(
    educ32 = case_when(
      !is.na(W9DANVQH) & !is.na(W9DVNVQH) ~ max(W9DANVQH, W9DVNVQH),
      !is.na(W9DANVQH) ~ W9DANVQH,
      !is.na(W9DVNVQH) ~ W9DVNVQH,
      TRUE ~ -3
    )
  )

# Collapse educ32 to 5-level categorical variable (same as educ25)
merged_data <- merged_data %>% 
  mutate(
    educ32 = case_when(
      educ32 == 4 | educ32 == 5 ~ 0,
      educ32 == 1 | educ32 == 2 | educ32 == 3 ~ 1,
      educ32 == 0 ~ 2,
      educ32 == 95 ~ 3,
      educ32 == 96 ~ 4,
      educ32 == -9 | educ32 == -8 | educ32 == -3 | educ32 == -2 | educ32 == -1 ~ educ32,
      TRUE ~ -3
    )
  ) %>% 
  mutate(educ32 = factor(educ32, levels = c(-9, -8, -3, -2, -1, 0, 1, 2, 3, 4), labels = c("Refused", "Don't know", "Null", "Not applicable", "No answer", educ25_levels)))

# Function to extract detailed academic qualification
get_academic_detail <- function(ac_vars) {
  labels <- list(
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
  
  for (var in ac_vars) {
    if (var %in% names(labels)) {
      val <- merged_data[[var]]
      if (!is.na(val) && val == 1) {
        return(labels[[var]])
      }
    }
  }
  return("None of these qualifications")
}

# Create educadtl32 variable
merged_data <- merged_data %>% 
  mutate(educadtl32 = get_academic_detail(paste0("W9ACQU", letters[0:21])))

# Function to extract detailed vocational qualification
get_vocational_detail <- function(voc_vars) {
  labels <- list(
    "W9VCQU0A" = "Professional qualifications at degree level",
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
  
  for (var in voc_vars) {
    if (var %in% names(labels)) {
      val <- merged_data[[var]]
      if (!is.na(val) && val == 1) {
        return(labels[[var]])
      }
    }
  }
  return("None of these qualifications")
}

# Create educvdtl32 variable
merged_data <- merged_data %>% 
  mutate(educvdtl32 = get_vocational_detail(paste0("W9VCQU", letters[0:34])))

# Select only required variables
final_data <- merged_data %>% 
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")

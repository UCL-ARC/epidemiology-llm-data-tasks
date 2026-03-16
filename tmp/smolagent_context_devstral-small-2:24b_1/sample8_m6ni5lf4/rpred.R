library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load each dataset into a separate object and merge using full_join by the ID variable
# The input files are tab-delimited (.tab) text files. Use `readr::read_delim()` with `delim = "\t"` to load them.

# Load wave_one_lsype_young_person_2020.tab
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")

# Load wave_four_lsype_young_person_2020.tab
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")

# Load ns8_2015_main_interview.tab
ns8_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")

# Load ns8_2015_derived.tab
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")

# Load ns9_2022_main_interview.tab
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Load ns9_2022_derived_variables.tab
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets using full_join by the ID variable NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8_main, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

# Define the NVQ level mapping for vocational qualifications at age 25
vocational_nvq_mapping <- list(
  "Youth training certificate" = 2,
  "Key Skills" = 1,
  "Basic skills" = 1,
  "Entry level qualifications (Wales)" = 0,
  "Modern apprenticeship/trade apprenticeship" = 3,
  "RSA/OCR/Clerical and commercial qualifications" = 2,
  "City and Guilds Certificate" = 2,
  "GNVQ/GSVQ" = 3,
  "NVQ/SVQ - Level 1 - 2" = 1,
  "NVQ/SVQ - Level 3 - 5" = 3,
  "HNC/HND" = 4,
  "ONC/OND" = 3,
  "BTEC/BEC/TEC/EdExcel/LQL" = 3,
  "SCOTVEC, SCOTEC or SCOTBEC" = 3,
  "Other vocational, technical or professional" = 3,
  "None of the above" = 4,
  "Don't know" = -3,
  "Refused" = -9
)

# Function to derive educ25
derive_educ25 <- function(data) {
  # Get the highest NVQ level from academic qualifications
  academic_nvq <- data$W8DHANVQH

  # Get the highest NVQ level from vocational qualifications
  vocational_nvq <- sapply(names(vocational_nvq_mapping), function(var) {
    if (var %in% names(data)) {
      if (!is.na(data[[var]]) && data[[var]] == 1) {
        return(vocational_nvq_mapping[[var]])
      }
    }
    return(-3)
  })

  # Combine academic and vocational NVQ levels
  combined_nvq <- c(academic_nvq, vocational_nvq)

  # Find the highest valid NVQ level
  highest_nvq <- max(combined_nvq, na.rm = TRUE)

  # Map the highest NVQ level to the collapsed categorical variable
  educ25 <- case_when(
    highest_nvq == 4 | highest_nvq == 5 ~ 0,
    highest_nvq == 1 | highest_nvq == 2 | highest_nvq == 3 ~ 1,
    highest_nvq == 0 ~ 2,
    highest_nvq == 95 ~ 3,
    highest_nvq == 96 ~ 4,
    highest_nvq == -9 | highest_nvq == -8 | highest_nvq == -3 | highest_nvq == -2 | highest_nvq == -1 ~ highest_nvq,
    TRUE ~ -3
  )

  return(educ25)
}

# Function to derive educ32
derive_educ32 <- function(data) {
  # Get the highest NVQ level from academic qualifications
  academic_nvq <- data$W9DANVQH

  # Get the highest NVQ level from vocational qualifications
  vocational_nvq <- data$W9DVNVQH

  # Combine academic and vocational NVQ levels
  combined_nvq <- c(academic_nvq, vocational_nvq)

  # Find the highest valid NVQ level
  highest_nvq <- max(combined_nvq, na.rm = TRUE)

  # Map the highest NVQ level to the collapsed categorical variable
  educ32 <- case_when(
    highest_nvq == 4 | highest_nvq == 5 ~ 0,
    highest_nvq == 1 | highest_nvq == 2 | highest_nvq == 3 ~ 1,
    highest_nvq == 0 ~ 2,
    highest_nvq == 95 ~ 3,
    highest_nvq == 96 ~ 4,
    highest_nvq == -9 | highest_nvq == -8 | highest_nvq == -3 | highest_nvq == -2 | highest_nvq == -1 ~ highest_nvq,
    TRUE ~ -3
  )

  return(educ32)
}

# Function to derive educadtl32
derive_educadtl32 <- function(data) {
  # Define the priority order for academic qualifications
  priority_order <- c("W9ACQU0A", "W9ACQU0B", "W9ACQU0C", "W9ACQU0D", "W9ACQU0E", "W9ACQU0F", "W9ACQU0G", "W9ACQU0H", "W9ACQU0I", "W9ACQU0J", "W9ACQU0K", "W9ACQU0L", "W9ACQU0M", "W9ACQU0N", "W9ACQU0O", "W9ACQU0P", "W9ACQU0Q", "W9ACQU0R", "W9ACQU0S", "W9ACQU0T", "W9ACQU0U", "W9ACQU0V")

  # Define the labels for academic qualifications
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

  # Find the first valid positive response
  educadtl32 <- sapply(1:nrow(data), function(row_idx) {
    for (var in priority_order) {
      if (var %in% names(data)) {
        if (!is.na(data[row_idx, var]) && data[row_idx, var] == 1) {
          return(academic_labels[[var]])
        }
      }
    }
    return("None of these qualifications")
  })

  return(educadtl32)
}

# Function to derive educvdtl32
derive_educvdtl32 <- function(data) {
  # Define the priority order for vocational qualifications
  priority_order <- c("W9VCQU0A", "W9VCQU0B", "W9VCQU0C", "W9VCQU0D", "W9VCQU0E", "W9VCQU0F", "W9VCQU0G", "W9VCQU0H", "W9VCQU0I", "W9VCQU0J", "W9VCQU0K", "W9VCQU0L", "W9VCQU0M", "W9VCQU0N", "W9VCQU0O", "W9VCQU0P", "W9VCQU0Q", "W9VCQU0R", "W9VCQU0S", "W9VCQU0T", "W9VCQU0U", "W9VCQU0V", "W9VCQU0W", "W9VCQU0X", "W9VCQU0Y", "W9VCQU0Z", "W9VCQUAA", "W9VCQUAB", "W9VCQUAC", "W9VCQUAD", "W9VCQUAE", "W9VCQUAF", "W9VCQUAG", "W9VCQUAH", "W9VCQUAI")

  # Define the labels for vocational qualifications
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

  # Find the first valid positive response
  educvdtl32 <- sapply(1:nrow(data), function(row_idx) {
    for (var in priority_order) {
      if (var %in% names(data)) {
        if (!is.na(data[row_idx, var]) && data[row_idx, var] == 1) {
          return(vocational_labels[[var]])
        }
      }
    }
    return("None of these qualifications")
  })

  return(educvdtl32)
}

# Derive the variables
merged_data$educ25 <- derive_educ25(merged_data)
merged_data$educ32 <- derive_educ32(merged_data)
merged_data$educadtl32 <- derive_educadtl32(merged_data)
merged_data$educvdtl32 <- derive_educvdtl32(merged_data)

# Select the required variables
cleaned_data <- merged_data %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write the output to data/output/cleaned_data.csv
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)
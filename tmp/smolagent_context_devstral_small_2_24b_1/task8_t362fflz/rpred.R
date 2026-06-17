library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8_main, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

# Function to map NVQ levels to collapsed scheme
map_nvq_to_collapsed <- function(nvq_level) {
  case_when(
    nvq_level %in% c(4, 5) ~ 0,
    nvq_level %in% c(1, 2, 3) ~ 1,
    nvq_level == 0 ~ 2,
    nvq_level == 95 ~ 3,
    nvq_level == 96 ~ 4,
    TRUE ~ NA_real_
  )
}

# Derive educ25 (age 25)
# Combine W8DHANVQH with W8VCQU* vocational tick-box variables
# Map vocational qualifications to NVQ tiers based on metadata labels
vocational_nvq_mapping <- list(
  W8VCQU0A = 1,  # Youth training certificate
  W8VCQU0B = 1,  # Key Skills
  W8VCQU0C = 0,  # Basic skills
  W8VCQU0D = 0,  # Entry level qualifications (Wales)
  W8VCQU0E = 2,  # Modern apprenticeship/trade apprenticeship
  W8VCQU0F = 1,  # RSA/OCR/Clerical and commercial qualifications
  W8VCQU0G = 1,  # City and Guilds Certificate
  W8VCQU0H = 2,  # GNVQ/GSVQ
  W8VCQU0I = 2,  # NVQ/SVQ - Level 1 - 2
  W8VCQU0J = 4,  # NVQ/SVQ - Level 3 - 5
  W8VCQU0K = 4,  # HNC/HND
  W8VCQU0L = 3,  # ONC/OND
  W8VCQU0M = 2,  # BTEC/BEC/TEC/EdExcel/LQL
  W8VCQU0N = 2,  # SCOTVEC, SCOTEC or SCOTBEC
  W8VCQU0O = 1,  # Other vocational, technical or professional
  W8VCQU0P = 4,  # None of the above
  W8VCQU0Q = -8, # Don't know
  W8VCQU0R = -9  # Refused
)

# Calculate highest vocational NVQ level for each person
merged_data <- merged_data %>%
  mutate(
    highest_voc_nvq = NA_real_,
    .before = everything()
  )

for (var in names(vocational_nvq_mapping)) {
  nvq_level <- vocational_nvq_mapping[[var]]
  merged_data <- merged_data %>%
    mutate(
      highest_voc_nvq = case_when(
        is.na(highest_voc_nvq) & !is.na(!!sym(var)) & !!sym(var) == 1 ~ nvq_level,
        !is.na(highest_voc_nvq) & !is.na(!!sym(var)) & !!sym(var) == 1 & nvq_level > highest_voc_nvq ~ nvq_level,
        TRUE ~ highest_voc_nvq
      )
    )
}

# Combine academic and vocational NVQ levels
merged_data <- merged_data %>%
  mutate(
    educ25 = case_when(
      !is.na(W8DHANVQH) & !is.na(highest_voc_nvq) ~ pmax(map_nvq_to_collapsed(W8DHANVQH), map_nvq_to_collapsed(highest_voc_nvq)),
      !is.na(W8DHANVQH) ~ map_nvq_to_collapsed(W8DHANVQH),
      !is.na(highest_voc_nvq) ~ map_nvq_to_collapsed(highest_voc_nvq),
      TRUE ~ NA_real_
    )
  )

# Handle missing codes for educ25
merged_data <- merged_data %>%
  mutate(
    educ25 = case_when(
      is.na(educ25) & all(is.na(W8DHANVQH), is.na(highest_voc_nvq)) ~ -3,
      TRUE ~ educ25
    )
  )

# Derive educ32 (age 32)
# Combine W9DANVQH and W9DVNVQH
merged_data <- merged_data %>%
  mutate(
    educ32 = case_when(
      !is.na(W9DANVQH) & !is.na(W9DVNVQH) ~ pmax(map_nvq_to_collapsed(W9DANVQH), map_nvq_to_collapsed(W9DVNVQH)),
      !is.na(W9DANVQH) ~ map_nvq_to_collapsed(W9DANVQH),
      !is.na(W9DVNVQH) ~ map_nvq_to_collapsed(W9DVNVQH),
      TRUE ~ NA_real_
    )
  )

# Handle missing codes for educ32
merged_data <- merged_data %>%
  mutate(
    educ32 = case_when(
      is.na(educ32) & all(is.na(W9DANVQH), is.na(W9DVNVQH)) ~ -3,
      TRUE ~ educ32
    )
  )

# Derive educadtl32 (detailed academic qualifications at age 32)
# Scan W9ACQU* academic tick-box variables in metadata order
academic_vars <- c(
  "W9ACQU0A", "W9ACQU0B", "W9ACQU0C", "W9ACQU0D", "W9ACQU0E", "W9ACQU0F",
  "W9ACQU0G", "W9ACQU0H", "W9ACQU0I", "W9ACQU0J", "W9ACQU0K", "W9ACQU0L",
  "W9ACQU0M", "W9ACQU0N", "W9ACQU0O", "W9ACQU0P", "W9ACQU0Q", "W9ACQU0R",
  "W9ACQU0S", "W9ACQU0T", "W9ACQU0U", "W9ACQU0V"
)

merged_data <- merged_data %>%
  mutate(
    educadtl32 = NA_real_
  )

for (i in seq_along(academic_vars)) {
  var <- academic_vars[i]
  merged_data <- merged_data %>%
    mutate(
      educadtl32 = case_when(
        is.na(educadtl32) & !is.na(!!sym(var)) & !!sym(var) == 1 ~ i,
        is.na(educadtl32) & !is.na(!!sym(var)) & !!sym(var) == 2 ~ length(academic_vars) + 1,
        is.na(educadtl32) & !is.na(!!sym(var)) & !!sym(var) == -1 ~ -1,
        is.na(educadtl32) & !is.na(!!sym(var)) & !!sym(var) == -3 ~ -3,
        is.na(educadtl32) & !is.na(!!sym(var)) & !!sym(var) == -8 ~ -8,
        is.na(educadtl32) & !is.na(!!sym(var)) & !!sym(var) == -9 ~ -9,
        is.na(educadtl32) & !is.na(!!sym(var)) & !!sym(var) == -2 ~ -2,
        TRUE ~ educadtl32
      )
    )
}

# Convert educadtl32 to factor
educadtl32_labels <- c(
  "Doctorate or equivalent",
  "Masters or equivalent",
  "Undergraduate or equivalent",
  "Post-graduate Diplomas and Certificates",
  "Diplomas in higher education and other higher education qualifications",
  "Teaching qualifications for schools or further education (below degree level)",
  "A/AS Levels or equivalent",
  "Grade A-C, Level 4-9",
  "Grade D-G, Level 1-3",
  "SCE Higher",
  "Scottish Certificate Sixth Year Studies",
  "SCE Standard",
  "National 4 and 5",
  "National 2 and 3",
  "Leaving Certificate",
  "Junior Certificate grade A-C",
  "Junior Certificate grade D and below",
  "Other academic qualifications (including overseas)",
  "None of these qualifications",
  "Don't know",
  "Refused",
  "No answer"
)

merged_data <- merged_data %>%
  mutate(
    educadtl32 = factor(educadtl32, levels = c(1:length(academic_vars), -8, -9, -2), labels = c(educadtl32_labels[1:length(academic_vars)], "Don't know", "Refused", "No answer"))
  )

# Derive educvdtl32 (detailed vocational qualifications at age 32)
# Identical logic applied to W9VCQU* variables
vocational_vars <- c(
  "W9VCQU0A", "W9VCQU0B", "W9VCQU0C", "W9VCQU0D", "W9VCQU0E", "W9VCQU0F",
  "W9VCQU0G", "W9VCQU0H", "W9VCQU0I", "W9VCQU0J", "W9VCQU0K", "W9VCQU0L",
  "W9VCQU0M", "W9VCQU0N", "W9VCQU0O", "W9VCQU0P", "W9VCQU0Q", "W9VCQU0R",
  "W9VCQU0S", "W9VCQU0T", "W9VCQU0U", "W9VCQU0V", "W9VCQU0W", "W9VCQU0X",
  "W9VCQU0Y", "W9VCQU0Z", "W9VCQUAA", "W9VCQUAB", "W9VCQUAC", "W9VCQUAD",
  "W9VCQUAE", "W9VCQUAF", "W9VCQUAG", "W9VCQUAH", "W9VCQUAI"
)

merged_data <- merged_data %>%
  mutate(
    educvdtl32 = NA_real_
  )

for (i in seq_along(vocational_vars)) {
  var <- vocational_vars[i]
  merged_data <- merged_data %>%
    mutate(
      educvdtl32 = case_when(
        is.na(educvdtl32) & !is.na(!!sym(var)) & !!sym(var) == 1 ~ i,
        is.na(educvdtl32) & !is.na(!!sym(var)) & !!sym(var) == 2 ~ length(vocational_vars) + 1,
        is.na(educvdtl32) & !is.na(!!sym(var)) & !!sym(var) == -1 ~ -1,
        is.na(educvdtl32) & !is.na(!!sym(var)) & !!sym(var) == -3 ~ -3,
        is.na(educvdtl32) & !is.na(!!sym(var)) & !!sym(var) == -8 ~ -8,
        is.na(educvdtl32) & !is.na(!!sym(var)) & !!sym(var) == -9 ~ -9,
        TRUE ~ educvdtl32
      )
    )
}

# Convert educvdtl32 to factor
vocational_labels <- c(
  "Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor",
  "Nursing or other medical qualifications (below degree level)",
  "Level 4 or 5",
  "Level 3",
  "Level 2",
  "Level 1",
  "GNVQ Advanced",
  "GNVQ Intermediate",
  "Level 3",
  "Level 2",
  "Level Foundation",
  "Advanced Craft, Part III",
  "Craft, Part II",
  "Craft, Part I",
  "Level 3",
  "Level 2",
  "Level 1",
  "Advanced Diploma",
  "Higher Diploma",
  "RSA Diploma",
  "RSA Stage I, II,III",
  "Higher Level BTEC",
  "BTEC National",
  "BTEC First",
  "SCOTVEC National Certificate",
  "SCOTVEC first or general diploma",
  "SCOTVEC general diploma",
  "SCOTVEC modules",
  "HND or HNC",
  "OND or ONCM",
  "Junior certificate",
  "Other vocational qualifications (including some overseas)",
  "None of these qualifications",
  "Don't know",
  "Refused"
)

merged_data <- merged_data %>%
  mutate(
    educvdtl32 = factor(educvdtl32, levels = c(1:length(vocational_vars), -8, -9), labels = c(vocational_labels[1:length(vocational_vars)], "Don't know", "Refused"))
  )

# Select final variables
final_data <- merged_data %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")
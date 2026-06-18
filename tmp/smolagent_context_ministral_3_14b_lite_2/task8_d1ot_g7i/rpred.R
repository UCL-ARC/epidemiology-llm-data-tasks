
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all required files
wave_one_yp <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four_yp <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets on NSID to ensure full cohort frame
merged_data <- full_join(wave_one_yp, wave_four_yp, by = "NSID")
merged_data <- full_join(merged_data, ns8_main, by = "NSID")
merged_data <- full_join(merged_data, ns8_derived, by = "NSID")
merged_data <- full_join(merged_data, ns9_main, by = "NSID")
merged_data <- full_join(merged_data, ns9_derived, by = "NSID")

# Function to map missing values to standard codes
map_missing_values <- function(x, wave) {
  if (wave == "ns8") {
    x <- ifelse(is.na(x), -3, x)
    x <- recode(x, `-9` = -9, `-8` = -8, `-7` = -7, `-3` = -3, `-2` = -2, `-1` = -1)
  } else if (wave == "ns9") {
    x <- ifelse(is.na(x), -3, x)
    x <- recode(x, `-9` = -9, `-8` = -8, `-7` = -7, `-3` = -3, `-2` = -2, `-1` = -1)
  }
  return(x)
}

# --- Derive educ25 (NVQ scheme at age 25) ---
# Use W8DHANVQH from ns8_derived
merged_data <- merged_data %>%
  mutate(educ25 = case_when(
    W8DHANVQH %in% c(1, 2, 3, 4, 5) ~ W8DHANVQH,
    W8DHANVQH == 95 ~ 5, # Map 'Other academic qualification' to NVQ Level 5
    W8DHANVQH == 96 ~ 0, # Map 'None of these qualifications' to NVQ Entry Level
    TRUE ~ NA_real_
  )) %>%
  mutate(educ25 = map_missing_values(educ25, "ns8"))

# --- Derive educ32 (NVQ scheme at age 32) ---
# Use W9DANVQH from ns9_derived for academic qualifications
merged_data <- merged_data %>%
  mutate(educ32 = case_when(
    W9DANVQH %in% c(1, 2, 3, 4, 5) ~ W9DANVQH,
    W9DANVQH == 95 ~ 5, # Map 'Other academic qualification' to NVQ Level 5
    W9DANVQH == 96 ~ 0, # Map 'None of these qualifications' to NVQ Entry Level
    W9DANVQH == 0 ~ 0,  # NVQ Entry Level
    TRUE ~ NA_real_
  )) %>%
  mutate(educ32 = map_missing_values(educ32, "ns9"))

# --- Create labelled factors for educ25 and educ32 ---
# Define labels for NVQ levels
nvq_labels <- c(
  `0` = "NVQ Entry Level",
  `1` = "NVQ Level 1",
  `2` = "NVQ Level 2",
  `3` = "NVQ Level 3",
  `4` = "NVQ Level 4",
  `5` = "NVQ Level 5",
  `-1` = "Item not applicable",
  `-2` = "Schedule not applicable / script error / information lost",
  `-3` = "Not asked at the fieldwork stage / not interviewed",
  `-7` = "Prefer not to say",
  `-8` = "Don't know / insufficient information",
  `-9` = "Refusal"
)

# Apply labels to educ25 and educ32
merged_data <- merged_data %>%
  mutate(educ25 = factor(educ25, levels = c(-9, -8, -7, -3, -2, -1, 0, 1, 2, 3, 4, 5), labels = nvq_labels)) %>%
  mutate(educ32 = factor(educ32, levels = c(-9, -8, -7, -3, -2, -1, 0, 1, 2, 3, 4, 5), labels = nvq_labels))

# --- Derive educadtl32 (detailed academic qualifications at age 32) ---
# Use W9ACQU variables to create a detailed academic qualification variable
educadtl32 <- merged_data %>%
  transmute(
    NSID,
    educadtl32 = case_when(
      any(W9ACQU0A == 1) ~ "Doctorate or equivalent",
      any(W9ACQU0B == 1) ~ "Masters or equivalent",
      any(W9ACQU0C == 1) ~ "Undergraduate or equivalent",
      any(W9ACQU0D == 1) ~ "Post-graduate Diplomas and Certificates",
      any(W9ACQU0E == 1) ~ "Diplomas in higher education and other higher education qualifications",
      any(W9ACQU0F == 1) ~ "Teaching qualifications for schools or further education (below degree level)",
      any(W9ACQU0G == 1) ~ "A/AS Levels or equivalent",
      any(W9ACQU0H == 1) ~ "Grade A-C, Level 4-9",
      any(W9ACQU0I == 1) ~ "Grade D-G, Level 1-3",
      any(W9ACQU0J == 1) ~ "SCE Higher",
      any(W9ACQU0K == 1) ~ "Scottish Certificate Sixth Year Studies",
      any(W9ACQU0L == 1) ~ "SCE Standard",
      any(W9ACQU0M == 1) ~ "National 4 and 5",
      any(W9ACQU0N == 1) ~ "National 2 and 3",
      any(W9ACQU0O == 1) ~ "Leaving Certificate",
      any(W9ACQU0P == 1) ~ "Junior Certificate grade A-C",
      any(W9ACQU0Q == 1) ~ "Junior Certificate grade D and below",
      any(W9ACQU0R == 1) ~ "Other academic qualifications (including overseas)",
      any(W9ACQU0S == 1) ~ "None of these qualifications",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(educadtl32 = ifelse(is.na(educadtl32), "No academic qualifications", educadtl32))

# --- Derive educvdtl32 (detailed vocational qualifications at age 32) ---
# Use W9VCQU variables to create a detailed vocational qualification variable
educvdtl32 <- merged_data %>%
  transmute(
    NSID,
    educvdtl32 = case_when(
      any(W9VCQU0A == 1) ~ "Professional qualifications at degree level",
      any(W9VCQU0B == 1) ~ "Nursing or other medical qualifications (below degree level)",
      any(W9VCQU0C == 1) ~ "Level 4 or 5",
      any(W9VCQU0D == 1) ~ "Level 3",
      any(W9VCQU0E == 1) ~ "Level 2",
      any(W9VCQU0F == 1) ~ "Level 1",
      any(W9VCQU0G == 1) ~ "GNVQ Advanced",
      any(W9VCQU0H == 1) ~ "GNVQ Intermediate",
      any(W9VCQU0I == 1) ~ "Level 3 (other)",
      any(W9VCQU0J == 1) ~ "Level 2 (other)",
      any(W9VCQU0K == 1) ~ "Level Foundation",
      any(W9VCQU0L == 1) ~ "Advanced Craft, Part III",
      any(W9VCQU0M == 1) ~ "Craft, Part II",
      any(W9VCQU0N == 1) ~ "Craft, Part I",
      any(W9VCQU0O == 1) ~ "Level 3 (other)",
      any(W9VCQU0P == 1) ~ "Level 2 (other)",
      any(W9VCQU0Q == 1) ~ "Level 1 (other)",
      any(W9VCQU0R == 1) ~ "Advanced Diploma",
      any(W9VCQU0S == 1) ~ "Higher Diploma",
      any(W9VCQU0T == 1) ~ "RSA Diploma",
      any(W9VCQU0U == 1) ~ "RSA Stage I, II, III",
      any(W9VCQU0V == 1) ~ "Higher Level BTEC",
      any(W9VCQU0W == 1) ~ "BTEC National",
      any(W9VCQU0X == 1) ~ "BTEC First",
      any(W9VCQU0Y == 1) ~ "SCOTVEC National Certificate",
      any(W9VCQU0Z == 1) ~ "SCOTVEC first or general diploma",
      any(W9VCQUAA == 1) ~ "SCOTVEC general diploma",
      any(W9VCQUAB == 1) ~ "SCOTVEC modules",
      any(W9VCQUAC == 1) ~ "HND or HNC",
      any(W9VCQUAD == 1) ~ "OND or ONCM",
      any(W9VCQUAE == 1) ~ "Junior certificate",
      any(W9VCQUAF == 1) ~ "Other vocational qualifications (including some overseas)",
      any(W9VCQUAG == 1) ~ "None of these qualifications",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(educvdtl32 = ifelse(is.na(educvdtl32), "No vocational qualifications", educvdtl32))

# Merge detailed qualification variables back to the main dataset
final_data <- merged_data %>%
  left_join(educadtl32, by = "NSID") %>%
  left_join(educvdtl32, by = "NSID") %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write the output to CSV
write_csv(final_data, "data/output/cleaned_data.csv")

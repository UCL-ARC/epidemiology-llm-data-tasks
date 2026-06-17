
library(readr)
library(dplyr)

# Load all required files
wave8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Load main interview files for tick-box variables
wave8_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets by NSID
merged_data <- full_join(wave8_derived, wave8_main, by = "NSID")
merged_data <- full_join(merged_data, wave9_derived, by = "NSID")
merged_data <- full_join(merged_data, wave9_main, by = "NSID")

# Function to derive educ25
derive_educ25 <- function(data) {
  educ_academic <- case_when(
    !is.na(data$W8DHANVQH) & data$W8DHANVQH %in% c(4, 5) ~ 0,
    !is.na(data$W8DHANVQH) & data$W8DHANVQH %in% c(1, 2, 3) ~ 1,
    !is.na(data$W8DHANVQH) & data$W8DHANVQH == 0 ~ 2,
    !is.na(data$W8DHANVQH) & data$W8DHANVQH == 95 ~ 3,
    !is.na(data$W8DHANVQH) & data$W8DHANVQH == 96 ~ 4,
    TRUE ~ -3
  )

  # Vocational NVQ mapping
  vocational_nvq_map <- list(
    W8VCQU0I = 2, W8VCQU0J = 4, W8VCQU0K = 4, W8VCQU0L = 4, W8VCQU0M = 4,
    W8VCQU0N = 4, W8VCQU0O = 3, W8VCQU0A = 1, W8VCQU0B = 1, W8VCQU0C = 1,
    W8VCQU0D = 1, W8VCQU0E = 3, W8VCQU0F = 1, W8VCQU0G = 2, W8VCQU0H = 3
  )

  vocational_vars <- data %>% select(all_of(names(vocational_nvq_map)))
  mapped_vars <- vocational_vars %>%
    mutate(across(everything(), ~ ifelse(. == 1, vocational_nvq_map[[cur_column()]], NA))) %>%
    mutate(across(everything(), ~ ifelse(. %in% c(-9, -8, -7, -3, -2, -1), ., NA)))

  max_vocational_nvq <- apply(mapped_vars, 1, function(row) {
    valid_values <- row[!is.na(row)]
    if (length(valid_values) == 0) return(-3)
    max(valid_values, na.rm = TRUE)
  })

  educ_vocational <- case_when(
    max_vocational_nvq %in% c(4, 5) ~ 0,
    max_vocational_nvq %in% c(1, 2, 3) ~ 1,
    max_vocational_nvq == 0 ~ 2,
    TRUE ~ 4
  )

  educ25 <- pmax(educ_academic, educ_vocational, na.rm = TRUE)
  educ25[is.na(educ25)] <- -3

  educ25_labels <- c(
    "-9" = "Refusal", "-8" = "Don't know", "-7" = "Prefer not to say", "-3" = "Not asked",
    "-2" = "Schedule not applicable", "-1" = "Not applicable", "0" = "NVQ 4–5 equivalent",
    "1" = "NVQ 1–3 equivalent", "2" = "Entry level or no qualifications", "3" = "Other qualifications",
    "4" = "None of these qualifications"
  )

  factor(educ25, levels = c(-9, -8, -7, -3, -2, -1, 0, 1, 2, 3, 4), labels = educ25_labels)
}

# Derive educ25
merged_data$educ25 <- derive_educ25(merged_data)

# Derive educ32
educ32 <- case_when(
  !is.na(merged_data$W9DANVQH) & merged_data$W9DANVQH %in% c(4, 5) ~ 0,
  !is.na(merged_data$W9DANVQH) & merged_data$W9DANVQH %in% c(1, 2, 3) ~ 1,
  !is.na(merged_data$W9DANVQH) & merged_data$W9DANVQH == 0 ~ 2,
  !is.na(merged_data$W9DANVQH) & merged_data$W9DANVQH == 95 ~ 3,
  !is.na(merged_data$W9DANVQH) & merged_data$W9DANVQH == 96 ~ 4,
  TRUE ~ -3
)

educ_vocational_32 <- case_when(
  !is.na(merged_data$W9DVNVQH) & merged_data$W9DVNVQH %in% c(4, 5) ~ 0,
  !is.na(merged_data$W9DVNVQH) & merged_data$W9DVNVQH %in% c(1, 2, 3) ~ 1,
  !is.na(merged_data$W9DVNVQH) & merged_data$W9DVNVQH == 0 ~ 2,
  !is.na(merged_data$W9DVNVQH) & merged_data$W9DVNVQH == 95 ~ 3,
  TRUE ~ 4
)

merged_data$educ32 <- pmax(educ32, educ_vocational_32, na.rm = TRUE)
merged_data$educ32[is.na(merged_data$educ32)] <- -3
merged_data$educ32 <- factor(merged_data$educ32, levels = levels(merged_data$educ25), labels = levels(merged_data$educ25))

# Derive educadtl32
academic_vars <- merged_data %>% select(starts_with("W9ACQU"))

# Map non-substantive responses
academic_vars$W9ACQU0T <- ifelse(academic_vars$W9ACQU0T == 1, -8, academic_vars$W9ACQU0T)
academic_vars$W9ACQU0U <- ifelse(academic_vars$W9ACQU0U == 1, -9, academic_vars$W9ACQU0U)
academic_vars$W9ACQU0V <- ifelse(academic_vars$W9ACQU0V == 1, -2, academic_vars$W9ACQU0V)

educadtl32 <- apply(academic_vars, 1, function(row) {
  valid_indicators <- row[!is.na(row) & row != 2]
  if (length(valid_indicators) == 0) {
    if (all(!is.na(row)) && all(row == 2)) {
      max(row) + 1
    } else {
      -3
    }
  } else {
    max(valid_indicators)
  }
})

educadtl32[is.na(educadtl32)] <- -3

academic_labels <- c(
  "-9" = "Refusal", "-8" = "Don't know", "-7" = "Prefer not to say", "-3" = "Not asked",
  "-2" = "Schedule not applicable", "-1" = "Not applicable", "1" = "Doctorate or equivalent",
  "2" = "Masters or equivalent", "3" = "Undergraduate or equivalent", "4" = "Post-graduate Diplomas",
  "5" = "Higher education diplomas", "6" = "Teaching qualifications", "7" = "A/AS Levels",
  "8" = "Grade A-C", "9" = "Grade D-G", "10" = "SCE Higher", "11" = "Scottish Certificate",
  "12" = "SCE Standard", "13" = "National 4 and 5", "14" = "National 2 and 3",
  "15" = "Leaving Certificate", "16" = "Junior Certificate A-C", "17" = "Junior Certificate D and below",
  "18" = "Other academic qualifications", "19" = "None of these qualifications"
)

merged_data$educadtl32 <- factor(educadtl32, levels = c(-9, -8, -7, -3, -2, -1, 1:19), labels = academic_labels)

# Derive educvdtl32
vocational_vars <- merged_data %>% select(starts_with("W9VCQU"))

vocational_vars$W9VCQUAH <- ifelse(vocational_vars$W9VCQUAH == 1, -8, vocational_vars$W9VCQUAH)
vocational_vars$W9VCQUAI <- ifelse(vocational_vars$W9VCQUAI == 1, -9, vocational_vars$W9VCQUAI)

educvdtl32 <- apply(vocational_vars, 1, function(row) {
  valid_indicators <- row[!is.na(row) & row != 2]
  if (length(valid_indicators) == 0) {
    if (all(!is.na(row)) && all(row == 2)) {
      max(row) + 1
    } else {
      -3
    }
  } else {
    max(valid_indicators)
  }
})

educvdtl32[is.na(educvdtl32)] <- -3

# Correct number of labels to match the levels
vocational_labels <- c(
  "-9" = "Refusal", "-8" = "Don't know", "-3" = "Not asked", "-2" = "Schedule not applicable",
  "-1" = "Not applicable", "1" = "Professional qualifications", "2" = "Medical qualifications",
  "3" = "Level 4 or 5", "4" = "Level 3", "5" = "Level 2", "6" = "Level 1", "7" = "GNVQ Advanced",
  "8" = "GNVQ Intermediate", "9" = "Advanced Diploma", "10" = "Higher Diploma", "11" = "RSA Diploma",
  "12" = "RSA Stages", "13" = "Higher Level BTEC", "14" = "BTEC National", "15" = "BTEC First",
  "16" = "SCOTVEC National", "17" = "SCOTVEC Diploma", "18" = "SCOTVEC Modules", "19" = "HND or HNC",
  "20" = "OND or ONCM", "21" = "Junior Certificate", "22" = "Other vocational qualifications",
  "23" = "None of these qualifications"
)

merged_data$educvdtl32 <- factor(educvdtl32, levels = c(-9, -8, -3, -2, -1, 1:23), labels = vocational_labels)

# Select and write final data
final_data <- merged_data %>% select(NSID, educ25, educ32, educadtl32, educvdtl32)
write_csv(final_data, "data/output/cleaned_data.csv")

# Verify file creation
file.exists("data/output/cleaned_data.csv")

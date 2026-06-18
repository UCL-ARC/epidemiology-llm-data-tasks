# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# Define file paths
files <- list(
  wave_one_lsype_young_person_2020 = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_four_lsype_young_person_2020 = "data/input/wave_four_lsype_young_person_2020.tab",
  ns8_2015_main_interview = "data/input/ns8_2015_main_interview.tab",
  ns8_2015_derived = "data/input/ns8_2015_derived.tab",
  ns9_2022_main_interview = "data/input/ns9_2022_main_interview.tab",
  ns9_2022_derived_variables = "data/input/ns9_2022_derived_variables.tab"
)

# Load all files into a list of data frames
raw_data <- lapply(files, function(p) {
  read_delim(p, delim = "\t", col_types = cols(.default = "c"))
})

# Merge all data frames on NSID using full_join
merged <- raw_data[[1]]
for(i in 2:length(raw_data)) {
  merged <- full_join(merged, raw_data[[i]], by = "NSID")
}

# Helper: map academic NVQ numeric (0–5, 95–96) to collapsed NVQ scheme (NVQ4-5=0, NVQ1-3=1, Entry=2, Other=3, None=4)
map_academic_collapsed <- function(x) {
  if(is.na(x)) return(NA_integer_)
  val <- as.numeric(x)
  if(val < 0) return(as.integer(val))
  if(val %in% c(4,5)) return(0L)
  if(val %in% c(1,2,3)) return(1L)
  if(val == 0) return(2L)
  if(val == 95) return(3L)
  if(val == 96) return(4L)
  return(NA_integer_)
}

# --------- educ25 ---------------------------------
# Academic source W8DHANVQH
acad25_raw <- merged$W8DHANVQH
acad25 <- sapply(acad25_raw, map_academic_collapsed)

# Vocational tick boxes (W8VCQU0A–R)
voc_vars_25 <- names(merged)[grepl("^W8VCQU0[A-R]", names(merged))]
# Mapping for each tick box to collapsed level or negative code
voc_level_map_25 <- list(
  W8VCQU0A = 1L, W8VCQU0B = 1L, W8VCQU0C = 1L, W8VCQU0D = 2L,
  W8VCQU0E = 0L, W8VCQU0F = 1L, W8VCQU0G = 0L, W8VCQU0H = 0L,
  W8VCQU0I = 1L, W8VCQU0J = 0L, W8VCQU0K = 0L, W8VCQU0L = 1L,
  W8VCQU0M = 0L, W8VCQU0N = 0L, W8VCQU0O = 0L,
  W8VCQU0P = NA_integer_, W8VCQU0Q = -8L, W8VCQU0R = -9L
)

# Compute collapsed level for each tick box per person
voc25_codes <- sapply(voc_vars_25, function(v) {
  val <- merged[[v]]
  out <- ifelse(val == 1, voc_level_map_25[[v]], NA_integer_)
  out[as.numeric(val) == -9] <- -9
  out[as.numeric(val) == -8] <- -8
  out[as.numeric(val) == -1] <- -1
  return(out)
})

# For each person, choose minimal positive code; if none, choose minimal negative; if all NA, return -3
educ25_code <- apply(cbind(acad25, voc25_codes), 1, function(row) {
  pos <- row[row >= 0]
  if(length(pos) > 0) return(min(pos))
  neg <- row[row < 0]
  if(length(neg) > 0) return(min(neg))
  return(-3L)
})

# --------- educ32 ---------------------------------
# Academic source W9DANVQH
acad32_raw <- merged$W9DANVQH
acad32 <- sapply(acad32_raw, map_academic_collapsed)
# Vocational source W9DVNVQH
voc32_raw <- merged$W9DVNVQH
voc32 <- sapply(voc32_raw, map_academic_collapsed)
# Choose minimal positive; if none, minimal negative; if all NA, -3
educ32_code <- mapply(function(a, v) {
  vals <- c(a, v)
  pos <- vals[vals >= 0]
  if(length(pos) > 0) return(min(pos))
  neg <- vals[vals < 0]
  if(length(neg) > 0) return(min(neg))
  return(-3L)
}, acad32, voc32)

# --------- educadtl32 ---------------------------------
# Substantive tick boxes A–S (W9ACQU0A–S)
sub_adtl_vars <- paste0("W9ACQU0", LETTERS[1:19])
# Non‑substantive: T, U, V
non_adtl_vars <- c("W9ACQU0T", "W9ACQU0U", "W9ACQU0V")

# Derivation function for educadtl32
educadtl32_raw <- sapply(1:nrow(merged), function(i) {
  row <- merged[i, ]
  # Non‑substantive first
  if(any(row[non_adtl_vars] == 1, na.rm = TRUE)) {
    if(row$W9ACQU0T == 1) return(-8)
    if(row$W9ACQU0U == 1) return(-9)
    if(row$W9ACQU0V == 1) return(-2)
  }
  # -1
  if(any(row[sub_adtl_vars] == -1, na.rm = TRUE)) return(-1)
  # All substantive 2 or NA/-3? If all NA, set -3
  sub_vals <- row[sub_adtl_vars]
  if(all(is.na(sub_vals) | sub_vals == 2 | sub_vals == -3)) return(-3)
  # Find first Yes
  for(j in seq_along(sub_adtl_vars)) {
    if(row[[sub_adtl_vars[j]]] == 1) return(j)
  }
  # All No -> next code after last substantive
  return(length(sub_adtl_vars) + 1)
})

# Factor levels and labels
educadtl_labels <- c(
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
  "None of these qualifications"
)
levels_educadtl32 <- c(1:19, -9, -8, -7, -3, -2, -1)
labels_educadtl32 <- c(educadtl_labels,
                     "Refused", "Don\'t know", "Prefer not to say",
                     "Not asked at fieldwork stage",
                     "Schedule not applicable", "Item not applicable")

educadtl32_factor <- factor(educadtl32_raw, levels = levels_educadtl32, labels = labels_educadtl32)

# --------- educvdtl32 ---------------------------------
# Substantive tick boxes A–Z + AA, AB, AC
possible_vars <- c(paste0("W9VCQU0", LETTERS[1:26]), "W9VCQU0AA", "W9VCQU0AB", "W9VCQU0AC")
sub_vact_vars <- intersect(possible_vars, names(merged))
# Non‑substantive: AH, AI
non_vact_vars <- c("W9VCQUAH", "W9VCQUAI")
non_vact_vars <- intersect(non_vact_vars, names(merged))

# Derivation function for educvdtl32
educvdtl32_raw <- sapply(1:nrow(merged), function(i) {
  row <- merged[i, ]
  # Non‑substantive first
  if(any(row[non_vact_vars] == 1, na.rm = TRUE)) {
    if(row$W9VCQUAH == 1) return(-8)
    if(row$W9VCQUAI == 1) return(-9)
  }
  # -1
  if(any(row[sub_vact_vars] == -1, na.rm = TRUE)) return(-1)
  # All substantive 2 or NA/-3? If all NA, set -3
  sub_vals <- row[sub_vact_vars]
  if(all(is.na(sub_vals) | sub_vals == 2 | sub_vals == -3)) return(-3)
  # Find first Yes
  for(j in seq_along(sub_vact_vars)) {
    if(row[[sub_vact_vars[j]]] == 1) return(j)
  }
  # All No -> next code after last substantive
  return(length(sub_vact_vars) + 1)
})

# Labels for vocational qualifications (strip prefix)
educvdtl_labels <- c(
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
  "None of these qualifications"
)
# 32 substantive labels => levels 1:32 plus missing codes
levels_educvdtl32 <- c(1:32, -9, -8, -7, -3, -2, -1)
labels_educvdtl32 <- c(educvdtl_labels,
                     "Refused", "Don\'t know", "Prefer not to say",
                     "Not asked at fieldwork stage",
                     "Schedule not applicable", "Item not applicable")

educvdtl32_factor <- factor(educvdtl32_raw, levels = levels_educvdtl32, labels = labels_educvdtl32)

# --------- Final data frame ---------------------------------
final_df <- merged %>% select(NSID) %>%
  mutate(
    educ25 = educ25_code,
    educ32 = educ32_code,
    educadtl32 = educadtl32_factor,
    educvdtl32 = educvdtl32_factor
  ) %>% select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write to CSV
write_csv(final_df, "data/output/cleaned_data.csv", na = "")

cat("Cleaning complete. Output written to data/output/cleaned_data.csv\n")

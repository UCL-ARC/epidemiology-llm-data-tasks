# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(haven)

# Helper to convert NA to standard -3
standard_missing <- function(x) {
  case_when(
    is.na(x) ~ -3,
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    TRUE ~ x
  )
}

# Paths
input_dir <- "data/input/"

# Load files (only ID or relevant vars)
wave1 <- read_delim(paste0(input_dir, "wave_one_lsype_young_person_2020.tab"), delim = "\t", col_types = cols(), na = c("", "NA"))
wave4 <- read_delim(paste0(input_dir, "wave_four_lsype_young_person_2020.tab"), delim = "\t", col_types = cols(), na = c("", "NA"))

wave8_main <- read_delim(paste0(input_dir, "ns8_2015_main_interview.tab"), delim = "\t", col_types = cols(), na = c("", "NA"))
wave8_derived <- read_delim(paste0(input_dir, "ns8_2015_derived.tab"), delim = "\t", col_types = cols(), na = c("", "NA"))

wave9_main <- read_delim(paste0(input_dir, "ns9_2022_main_interview.tab"), delim = "\t", col_types = cols(), na = c("", "NA"))
wave9_derived <- read_delim(paste0(input_dir, "ns9_2022_derived_variables.tab"), delim = "\t", col_types = cols(), na = c("", "NA"))

# Keep only ID and variables needed from wave8_main (none needed for educ25 except W8DHANVQH from derived)
# Keep only ID from wave8_main
wave8_main <- wave8_main %>% select(NSID)

# Process derived variables
wave8_derived <- wave8_derived %>% select(NSID, W8DHANVQH) %>% mutate(W8DHANVQH = standard_missing(W8DHANVQH))

wave9_derived <- wave9_derived %>% select(NSID, W9DANVQH, W9DVNVQH) %>% mutate(
  W9DANVQH = standard_missing(W9DANVQH),
  W9DVNVQH = standard_missing(W9DVNVQH)
)

# Academic qualification yes/no variables (Age 32) – keep ID and all academic vars
academic_vars <- c(
  "W9ACQU0A", "W9ACQU0B", "W9ACQU0C", "W9ACQU0D", "W9ACQU0E",
  "W9ACQU0F", "W9ACQU0G", "W9ACQU0H", "W9ACQU0I", "W9ACQU0J",
  "W9ACQU0K", "W9ACQU0L", "W9ACQU0M", "W9ACQU0N", "W9ACQU0O",
  "W9ACQU0P", "W9ACQU0Q", "W9ACQU0R", "W9ACQU0S"
)

# Vocational qualification yes/no variables (Age 32) – keep ID and all voc vars
vocational_vars <- c(
  "W9VCQU0A", "W9VCQU0B", "W9VCQU0C", "W9VCQU0D", "W9VCQU0E", "W9VCQU0F",
  "W9VCQU0G", "W9VCQU0H", "W9VCQU0I", "W9VCQU0J", "W9VCQU0K", "W9VCQU0L",
  "W9VCQU0M", "W9VCQU0N", "W9VCQU0O", "W9VCQU0P", "W9VCQU0Q", "W9VCQU0R",
  "W9VCQU0S", "W9VCQU0T", "W9VCQU0U", "W9VCQU0V", "W9VCQU0W", "W9VCQU0X",
  "W9VCQU0Y", "W9VCQU0Z", "W9VCQUAA", "W9VCQUAB", "W9VCQUAC", "W9VCQUAD",
  "W9VCQUAE", "W9VCQUAF", "W9VCQUAG", "W9VCQUAH", "W9VCQUAI"
)

# Select and standardise missing for academic and vocational vars
wave9_main <- wave9_main %>% select(NSID, all_of(academic_vars), all_of(vocational_vars)) %>% mutate(
  across(all_of(c(academic_vars, vocational_vars)), ~standard_missing(.))
)

# Merge all datasets by NSID
full_df <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8_main, by = "NSID") %>%
  full_join(wave8_derived, by = "NSID") %>%
  full_join(wave9_main, by = "NSID") %>%
  full_join(wave9_derived, by = "NSID")

# Derive educ25 (NVQ level at 25)
full_df <- full_df %>% mutate(educ25 = W8DHANVQH)

# Derive educ32: highest NVQ level (academic or vocational) at 32
full_df <- full_df %>% mutate(
  educ32 = pmax(
    ifelse(W9DANVQH %in% c(-9,-8,-1,-3), NA_real_, W9DANVQH),
    ifelse(W9DVNVQH %in% c(-9,-8,-1,-3), NA_real_, W9DVNVQH),
    na.rm = TRUE
  )
)
# Convert result missing to standard code
full_df <- full_df %>% mutate(educ32 = standard_missing(educ32))

# Academic qualification hierarchy (highest to lowest) for educadtl32
academic_order <- c(
  "W9ACQU0A",  # Doctorate
  "W9ACQU0B",  # Masters
  "W9ACQU0C",  # Undergraduate
  "W9ACQU0D",  # Post-graduate Diplomas and Certificates
  "W9ACQU0E",  # Diplomas in higher education
  "W9ACQU0F",  # Teaching qualifications
  "W9ACQU0G",  # A/AS Levels
  "W9ACQU0H",  # Grade A-C, Level 4-9
  "W9ACQU0I",  # Grade D-G, Level 1-3
  "W9ACQU0J",  # SCE Higher
  "W9ACQU0K",  # Scottish Certificate Sixth Year Studies
  "W9ACQU0L",  # Scottish Standard
  "W9ACQU0M",  # National 4 and 5
  "W9ACQU0N",  # National 2 and 3
  "W9ACQU0O",  # Leaving Certificate
  "W9ACQU0P",  # Junior Certificate grade A-C
  "W9ACQU0Q",  # Junior Certificate grade D and below
  "W9ACQU0S",  # Other academic qualifications
  "W9ACQU0R"   # None of these qualifications
)

# Map to numeric 1-19
academic_codes <- 1:19
names(academic_codes) <- academic_order

# Derive educadtl32
full_df <- full_df %>% mutate(
  educadtl32 = case_when(
    W9ACQU0A == 1 ~ 1,
    W9ACQU0B == 1 ~ 2,
    W9ACQU0C == 1 ~ 3,
    W9ACQU0D == 1 ~ 4,
    W9ACQU0E == 1 ~ 5,
    W9ACQU0F == 1 ~ 6,
    W9ACQU0G == 1 ~ 7,
    W9ACQU0H == 1 ~ 8,
    W9ACQU0I == 1 ~ 9,
    W9ACQU0J == 1 ~ 10,
    W9ACQU0K == 1 ~ 11,
    W9ACQU0L == 1 ~ 12,
    W9ACQU0M == 1 ~ 13,
    W9ACQU0N == 1 ~ 14,
    W9ACQU0O == 1 ~ 15,
    W9ACQU0P == 1 ~ 16,
    W9ACQU0Q == 1 ~ 17,
    W9ACQU0S == 1 ~ 18,
    W9ACQU0R == 1 ~ 19,
    TRUE ~ NA_real_
  )
)

# Vocational qualification hierarchy for educvdtl32
vocational_order <- c(
  "W9VCQU0I",  # NVQ Level 5
  "W9VCQU0C",  # Level 4 or 5
  "W9VCQU0D",  # Level 3
  "W9VCQU0E",  # Level 2
  "W9VCQU0F",  # Level 1
  "W9VCQU0G",  # GNVQ Advanced
  "W9VCQU0H",  # GNVQ Intermediate
  "W9VCQU0L",  # Level Foundation
  "W9VCQU0K",  # Advanced Craft Part III
  "W9VCQU0M",  # Craft Part II
  "W9VCQU0N",  # Craft Part I
  "W9VCQU0O",  # Level 3 (duplicate)
  "W9VCQU0P",  # Level 2 (duplicate)
  "W9VCQU0Q",  # Level 1 (duplicate)
  "W9VCQU0R",  # Advanced Diploma
  "W9VCQU0S",  # Higher Diploma
  "W9VCQU0T",  # RSA Diploma
  "W9VCQU0U",  # RSA Stage I, II, III
  "W9VCQU0V",  # Higher Level BTEC
  "W9VCQU0W",  # BTEC National
  "W9VCQU0X",  # BTEC First
  "W9VCQU0Y",  # SCOTVEC National Certificate
  "W9VCQU0Z",  # SCOTVEC first or general diploma
  "W9VCQUAA",  # SCOTVEC general diploma
  "W9VCQUAB",  # SCOTVEC modules
  "W9VCQUAC",  # HND/HNC
  "W9VCQUAD",  # OND/OND
  "W9VCQUAE",  # Junior certificate
  "W9VCQUAF",  # Other vocational qualifications
  "W9VCQUAG"   # None of these qualifications
)

# Derive educvdtl32
full_df <- full_df %>% mutate(
  educvdtl32 = case_when(
    W9VCQU0I == 1 ~ 1,
    W9VCQU0C == 1 ~ 2,
    W9VCQU0D == 1 ~ 3,
    W9VCQU0E == 1 ~ 4,
    W9VCQU0F == 1 ~ 5,
    W9VCQU0G == 1 ~ 6,
    W9VCQU0H == 1 ~ 7,
    W9VCQU0L == 1 ~ 8,
    W9VCQU0K == 1 ~ 9,
    W9VCQU0M == 1 ~ 10,
    W9VCQU0N == 1 ~ 11,
    W9VCQU0O == 1 ~ 12,
    W9VCQU0P == 1 ~ 13,
    W9VCQU0Q == 1 ~ 14,
    W9VCQU0R == 1 ~ 15,
    W9VCQU0S == 1 ~ 16,
    W9VCQU0T == 1 ~ 17,
    W9VCQU0U == 1 ~ 18,
    W9VCQU0V == 1 ~ 19,
    W9VCQU0W == 1 ~ 20,
    W9VCQU0X == 1 ~ 21,
    W9VCQU0Y == 1 ~ 22,
    W9VCQU0Z == 1 ~ 23,
    W9VCQUAA == 1 ~ 24,
    W9VCQUAB == 1 ~ 25,
    W9VCQUAC == 1 ~ 26,
    W9VCQUAD == 1 ~ 27,
    W9VCQUAE == 1 ~ 28,
    W9VCQUAF == 1 ~ 29,
    W9VCQUAG == 1 ~ 30,
    TRUE ~ NA_real_
  )
)

# Assign factor labels for educational variables
educadtl_labels <- c(
  "Doctorate or equivalent", "Masters or equivalent", "Undergraduate or equivalent",
  "Post‑graduate Diplomas and Certificates", "Diplomas in higher education and other higher education qualifications",
  "Teaching qualifications for schools or further education", "A/AS Levels or equivalent",
  "Grade A–C, Level 4–9", "Grade D–G, Level 1–3", "SCE Higher",
  "Scottish Certificate Sixth Year Studies", "Scottish Standard",
  "National 4 and 5", "National 2 and 3", "Leaving Certificate",
  "Junior Certificate grade A–C", "Junior Certificate grade D and below",
  "Other academic qualifications (including overseas)", "None of these qualifications"
)

educvdtl_labels <- c(
  "NVQ Level 5", "Level 4 or 5", "Level 3", "Level 2", "Level 1",
  "GNVQ Advanced", "GNVQ Intermediate", "Level Foundation",
  "Advanced Craft Part III", "Craft Part II", "Craft Part I",
  "Level 3 (duplicate)", "Level 2 (duplicate)", "Level 1 (duplicate)",
  "Advanced Diploma", "Higher Diploma", "RSA Diploma",
  "RSA Stage I, II, III", "Higher Level BTEC", "BTEC National",
  "BTEC First", "SCOTVEC National Certificate",
  "SCOTVEC first or general diploma", "SCOTVEC general diploma",
  "SCOTVEC modules", "HND/HNC", "OND/OND",
  "Junior certificate", "Other vocational qualifications",
  "None of these qualifications"
)

full_df <- full_df %>% mutate(
  educadtl32 = factor(educadtl32, levels = 1:19, labels = educadtl_labels, ordered = TRUE),
  educvdtl32 = factor(educvdtl32, levels = 1:length(vocational_order), labels = educvdtl_labels, ordered = FALSE)
)

# Keep only final variables
final_df <- full_df %>% select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write output
output_path <- "data/output/cleaned_data.csv"
write_csv(final_df, output_path)

cat("Cleaning complete. Output written to", output_path, "\n")

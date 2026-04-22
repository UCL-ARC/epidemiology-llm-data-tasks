library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c('ns8_2015_main_interview.tab', 'ns8_2015_derived.tab', 
           'ns9_2022_main_interview.tab', 'ns9_2022_derived_variables.tab')

load_tab <- function(f) {
  readr::read_delim(paste0('data/input/', f), delim = '\t', show_col_types = FALSE)
}

data_list <- map(files, load_tab)
names(data_list) <- files

full_df <- data_list[[1]] %>%
  full_join(data_list[[2]], by = 'NSID') %>%
  full_join(data_list[[3]], by = 'NSID') %>%
  full_join(data_list[[4]], by = 'NSID')

# Helper for NVQ mapping
map_nvq_val <- function(x) {
  case_when(
    x %in% c(4, 5) ~ 0,
    x %in% c(1, 2, 3) ~ 1,
    x == 0 ~ 2,
    x == 95 ~ 3,
    x == 96 ~ 4,
    x %in% c(-9, -8, -3, -2, -1) ~ x,
    TRUE ~ -3
  )
}

# 4. educ25
full_df <- full_df %>%
  mutate(
    voc_level_w8 = case_when(
      (W8VCQU0J == 1 | W8VCQU0K == 1 | W8VCQU0L == 1) ~ 0,
      (W8VCQU0I == 1 | W8VCQU0H == 1 | W8VCQU0E == 1) ~ 1,
      (W8VCQU0A == 1 | W8VCQU0B == 1 | W8VCQU0C == 1 | W8VCQU0D == 1 | W8VCQU0F == 1 | W8VCQU0G == 1) ~ 2,
      W8VCQU0P == 1 ~ 4,
      TRUE ~ NA_real_
    ),
    acad_level_w8 = map_nvq_val(W8DHANVQH),
    educ25_raw = pmin(voc_level_w8, acad_level_w8, na.rm = TRUE),
    educ25 = case_when(
      !is.na(educ25_raw) ~ educ25_raw,
      W8DHANVQH %in% c(-9, -8, -3, -2, -1) ~ W8DHANVQH,
      TRUE ~ -3
    )
  )

# 5. educ32
full_df <- full_df %>%
  mutate(
    acad_32 = map_nvq_val(W9DANVQH),
    voc_32 = map_nvq_val(W9DVNVQH),
    educ32_raw = pmin(acad_32, voc_32, na.rm = TRUE),
    educ32 = case_when(
      !is.na(educ32_raw) ~ educ32_raw,
      W9DANVQH %in% c(-9, -8, -3, -2, -1) ~ W9DANVQH,
      W9DVNVQH %in% c(-9, -8, -3, -2, -1) ~ W9DVNVQH,
      TRUE ~ -3
    )
  )

# Metadata mappings for labels
meta_labels <- list(
  W9ACQU0A = "Doctorate or equivalent", W9ACQU0B = "Masters or equivalent",
  W9ACQU0C = "Undergraduate or equivalent", W9ACQU0D = "Post-graduate Diplomas and Certificates",
  W9ACQU0E = "Diplomas in higher education and other higher education qualifications",
  W9ACQU0F = "Teaching qualifications for schools or further education (below degree level)",
  W9ACQU0G = "A/AS Levels or equivalent", W9ACQU0H = "Grade A-C, Level 4-9",
  W9ACQU0I = "Grade D-G, Level 1-3", W9ACQU0J = "SCE Higher",
  W9ACQU0K = "Scottish Certificate Sixth Year Studies", W9ACQU0L = "SCE Standard",
  W9ACQU0M = "National 4 and 5", W9ACQU0N = "National 2 and 3",
  W9ACQU0O = "Leaving Certificate", W9ACQU0P = "Junior Certificate grade A-C",
  W9ACQU0Q = "Junior Certificate grade D and below", W9ACQU0R = "Other academic qualifications (including overseas)",
  W9VCQU0A = "Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor",
  W9VCQU0B = "Nursing or other medical qualifications (below degree level)",
  W9VCQU0C = "Level 4 or 5", W9VCQU0D = "Level 3",
  W9VCQU0E = "Level 2", W9VCQU0F = "Level 1",
  W9VCQU0G = "GNVQ Advanced", W9VCQU0H = "GNVQ Intermediate",
  W9VCQU0I = "Level 3", W9VCQU0J = "Level 2",
  W9VCQU0K = "Level Foundation", W9VCQU0L = "Advanced Craft, Part III",
  W9VCQU0M = "Craft, Part II", W9VCQU0N = "Craft, Part I",
  W9VCQU0O = "Level 3", W9VCQU0P = "Level 2",
  W9VCQU0Q = "Level 1", W9VCQU0R = "Advanced Diploma",
  W9VCQU0S = "Higher Diploma", W9VCQU0T = "RSA Diploma",
  W9VCQU0U = "RSA Stage I, II,III", W9VCQU0V = "Higher Level BTEC",
  W9VCQU0W = "BTEC National", W9VCQU0X = "BTEC First",
  W9VCQU0Y = "SCOTVEC National Certificate", W9VCQU0Z = "SCOTVEC first or general diploma",
  W9VCQUAA = "SCOTVEC general diploma", W9VCQUAB = "SCOTVEC modules",
  W9VCQUAC = "HND or  HNC", W9VCQUAD = "OND or ONCM",
  W9VCQUAE = "Junior certificate", W9VCQUAF = "Other vocational qualifications (including some overseas)"
)

get_detail_val <- function(df, vars, labels_map) {
  apply(df[, vars, drop = FALSE], 1, function(row) {
    idx <- which(row == 1)[1]
    if (is.na(idx)) {
      if (all(row == 2, na.rm = TRUE)) return("None of these qualifications")
      return(NA_character_)
    }
    labels_map[[vars[idx]]]
  })
}

acad_vars <- names(meta_labels)[grep("W9ACQU0[A-R]", names(meta_labels))]
voc_vars <- names(meta_labels)[grep("W9VCQU", names(meta_labels))]

full_df$educadtl32 <- get_detail_val(full_df, acad_vars, meta_labels)
full_df$educvdtl32 <- get_detail_val(full_df, voc_vars, meta_labels)

# Final Formatting
nvq_levels <- c("0", "1", "2", "3", "4", "-9", "-8", "-3", "-2", "-1")
nvq_labels <- c("NVQ 4–5 equivalent qualifications", "NVQ 1–3 equivalent qualifications", 
                "Entry level or no qualifications", "Other qualifications not mappable to NVQ framework", 
                "None of these qualifications", "Refused", "Don't know", "Null", "Not applicable", "Not applicable")

final_df <- full_df %>%
  mutate(
    educ25 = factor(educ25, levels = nvq_levels, labels = nvq_labels),
    educ32 = factor(educ32, levels = nvq_levels, labels = nvq_labels)
  ) %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

write_csv(final_df, "data/output/cleaned_data.csv")

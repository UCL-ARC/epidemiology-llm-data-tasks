library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

df1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
df2 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
df3 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
df4 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
df5 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
df6 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

merged_df <- full_join(df1, df2, by = "NSID") %>%
  full_join(df3, by = "NSID") %>%
  full_join(df4, by = "NSID") %>%
  full_join(df5, by = "NSID") %>%
  full_join(df6, by = "NSID")

w8vcqu_map <- c(
  "W8VCQU0A" = 2,
  "W8VCQU0B" = 2,
  "W8VCQU0C" = 2,
  "W8VCQU0D" = 2,
  "W8VCQU0E" = 1,
  "W8VCQU0F" = 3,
  "W8VCQU0G" = 1,
  "W8VCQU0H" = 1,
  "W8VCQU0I" = 1,
  "W8VCQU0J" = 0,
  "W8VCQU0K" = 0,
  "W8VCQU0L" = 0,
  "W8VCQU0M" = 1,
  "W8VCQU0N" = 1,
  "W8VCQU0O" = 3,
  "W8VCQU0P" = 4,
  "W8VCQU0Q" = NA,
  "W8VCQU0R" = NA
)

acqu_labels <- c(
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

voc_labels <- c(
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
  "W9VCQUAC" = "HND or HNC",
  "W9VCQUAD" = "OND or ONCM",
  "W9VCQUAE" = "Junior certificate",
  "W9VCQUAF" = "Other vocational qualifications (including some overseas)",
  "W9VCQUAG" = "None of these qualifications",
  "W9VCQUAH" = "Don't know",
  "W9VCQUAI" = "Refused"
)

merged_df <- merged_df %>%
  mutate(academic_25 = case_when(
    W8DHANVQH %in% c(4, 5) ~ 0,
    W8DHANVQH %in% c(1, 2, 3) ~ 1,
    W8DHANVQH == 95 ~ 3,
    W8DHANVQH == 96 ~ 4,
    W8DHANVQH %in% c(-9, -8, -1, -2, -3) ~ W8DHANVQH,
    TRUE ~ -3
  ))

for (var in names(w8vcqu_map)) {
  if (var %in% colnames(merged_df)) {
    merged_df <- merged_df %>%
      mutate(!!var := ifelse(.data[[var]] == 1, w8vcqu_map[[var]], .data[[var]]))
  }
}

vocational_25 <- apply(merged_df[, grep("^W8VCQU", colnames(merged_df))], 1, function(x) min(x, na.rm = TRUE))
merged_df <- merged_df %>%
  mutate(educ25 = pmin(academic_25, vocational_25, na.rm = TRUE)) %>%
  mutate(educ25 = ifelse(is.na(educ25), -3, educ25))

merged_df <- merged_df %>%
  mutate(academic_32 = case_when(
    W9DANVQH %in% c(4, 5) ~ 0,
    W9DANVQH %in% c(1, 2, 3) ~ 1,
    W9DANVQH == 0 ~ 2,
    W9DANVQH == 95 ~ 3,
    W9DANVQH == 96 ~ 4,
    W9DANVQH %in% c(-9, -8, -1, -2, -3) ~ W9DANVQH,
    TRUE ~ -3
  )) %>%
  mutate(vocational_32 = case_when(
    W9DVNVQH %in% c(4, 5) ~ 0,
    W9DVNVQH %in% c(1, 2, 3) ~ 1,
    W9DVNVQH == 0 ~ 2,
    W9DVNVQH == 95 ~ 3,
    W9DVNVQH == 96 ~ 4,
    W9DVNVQH %in% c(-9, -8, -1, -2, -3) ~ W9DVNVQH,
    TRUE ~ -3
  )) %>%
  mutate(educ32 = pmin(academic_32, vocational_32, na.rm = TRUE)) %>%
  mutate(educ32 = ifelse(is.na(educ32), -3, educ32))

merged_df <- merged_df %>%
  mutate(educadtl32 = pmap_chr(across(starts_with("W9ACQU")), ~ {
    args <- list(...)
    for (i in 1:length(args)) {
      value <- args[[i]]
      var_name <- names(args)[i]
      if (!is.na(value) && value == 1) {
        return(acqu_labels[var_name])
      }
    }
    non_na_values <- unlist(args)[!is.na(unlist(args))]
    if (length(non_na_values) > 0 && all(non_na_values == 2)) {
      return("None of these qualifications")
    }
    missing_codes <- c(-3, -1, -9, -8)
    for (i in 1:length(args)) {
      value <- args[[i]]
      if (!is.na(value) && value %in% missing_codes) {
        return(as.character(value))
      }
    }
    return(NA_character_)
  })) %>%
  mutate(educvdtl32 = pmap_chr(across(starts_with("W9VCQU")), ~ {
    args <- list(...)
    for (i in 1:length(args)) {
      value <- args[[i]]
      var_name <- names(args)[i]
      if (!is.na(value) && value == 1) {
        return(voc_labels[var_name])
      }
    }
    non_na_values <- unlist(args)[!is.na(unlist(args))]
    if (length(non_na_values) > 0 && all(non_na_values == 2)) {
      return("None of these qualifications")
    }
    missing_codes <- c(-3, -1, -9, -8)
    for (i in 1:length(args)) {
      value <- args[[i]]
      if (!is.na(value) && value %in% missing_codes) {
        return(as.character(value))
      }
    }
    return(NA_character_)
  }))

final_df <- merged_df %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

write_csv(final_df, "data/output/cleaned_data.csv")
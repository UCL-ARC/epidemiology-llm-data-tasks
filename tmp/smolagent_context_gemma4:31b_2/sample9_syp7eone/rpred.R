library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
file1 <- "data/input/wave_one_lsype_family_background_2020.tab"
file2 <- "data/input/wave_two_lsype_family_background_2020.tab"
file4 <- "data/input/wave_four_lsype_family_background_2020.tab"

data1 <- read_delim(file1, delim = "\t", col_types = cols(NSID = col_character(), .default = "numeric"), show_col_types = FALSE)
data2 <- read_delim(file2, delim = "\t", col_types = cols(NSID = col_character(), .default = "numeric"), show_col_types = FALSE)
data4 <- read_delim(file4, delim = "\t", col_types = cols(NSID = col_character(), .default = "numeric"), show_col_types = FALSE)

data1 <- data1 %>% select(NSID, W1hiqualmum, W1hiqualdad) %>% distinct(NSID, .keep_all = TRUE)
data2 <- data2 %>% select(NSID, W2hiqualmum, W2hiqualdad) %>% distinct(NSID, .keep_all = TRUE)
data4 <- data4 %>% select(NSID, w4hiqualmum, w4hiqualdad) %>% distinct(NSID, .keep_all = TRUE)

merged_data <- data1 %>%
  full_join(data2, by = "NSID") %>%
  full_join(data4, by = "NSID")

# 6. Missing Value Code Harmonization
lbls1_m <- c("-999.0"="Missing - household data lost", "-99.0"="Mother not interviewed", "-98.0"="Mother not present", "-94.0"="Insufficient information", "-92.0"="Refused", "-91.0"="Not applicable")
lbls1_f <- c("-999.0"="Missing - household data lost", "-99.0"="Father not interviewed", "-98.0"="Father not present", "-94.0"="Insufficient information", "-92.0"="Refused", "-91.0"="Not applicable", "-1.0"="Don't know")
lbls4_m <- c("-99.0"="Mother not interviewed", "-98.0"="Mother not present", "-94.0"="Insufficient information")
lbls4_f <- c("-99.0"="Father not interviewed", "-98.0"="Father not present", "-94.0"="Insufficient information")

process_wave <- function(df_col, labels) {
  sapply(df_col, function(x) {
    if (is.na(x)) return(-3)
    if (x > 0) return(x)
    label <- labels[as.character(x)]
    if (is.na(label)) return(-3)
    label <- tolower(label)
    if (grepl("refused", label)) return(-9)
    if (grepl("don't know|insufficient information", label)) return(-8)
    if (grepl("not interviewed|not present|not asked", label)) return(-3)
    if (grepl("lost|error", label)) return(-2)
    if (grepl("not applicable", label)) return(-1)
    return(-3)
  })
}

merged_data <- merged_data %>%
  mutate(
    m1 = process_wave(W1hiqualmum, lbls1_m),
    f1 = process_wave(W1hiqualdad, lbls1_f),
    m2 = process_wave(W2hiqualmum, lbls1_m),
    f2 = process_wave(W2hiqualdad, lbls1_f),
    m4 = process_wave(w4hiqualmum, lbls4_m),
    f4 = process_wave(w4hiqualdad, lbls4_f)
  )

# 4. Consolidation Logic
consolidate <- function(v1, v2, v4) {
  res <- rep(NA, length(v1))
  pos_idx <- which(v1 > 0)
  res[pos_idx] <- v1[pos_idx]
  remaining <- which(is.na(res))
  pos_idx2 <- remaining[v2[remaining] > 0]
  res[pos_idx2] <- v2[pos_idx2]
  remaining <- which(is.na(res))
  pos_idx4 <- remaining[v4[remaining] > 0]
  res[pos_idx4] <- v4[pos_idx4]
  remaining <- which(is.na(res))
  res[remaining] <- v1[remaining]
  remaining <- which(is.na(res))
  res[remaining] <- v2[remaining]
  remaining <- which(is.na(res))
  res[remaining] <- v4[remaining]
  res[is.na(res)] <- -3
  return(res)
}

merged_data <- merged_data %>%
  mutate(
    educdtlma = consolidate(m1, m2, m4),
    educdtlpa = consolidate(f1, f2, f4)
  )

# 5. Derived Collapsed Variables
collapse_educ <- function(x) {
  case_when(
    x >= 1 & x <= 4 ~ 0, 
    x >= 5 & x <= 17 ~ 1, 
    x == 18 ~ 2,         
    x == 19 ~ 3,         
    x == 20 ~ 4,         
    TRUE ~ x            
  )
}

merged_data <- merged_data %>%
  mutate(
    educma = collapse_educ(educdtlma),
    educpa = collapse_educ(educdtlpa)
  )

# 7. Factor Variables and Labels
det_labels <- c(
  "1" = "Higher Degree", "2" = "First Degree", "3" = "HE Diploma", "4" = "HNC/HND/NVQ4",
  "5" = "Teaching qualification, non-degree", "6" = "Nursing qualification, non-degree",
  "7" = "A Levels", "8" = "OND/ONC", "9" = "City and guilds part III, NVQ3",
  "10" = "CSYS", "11" = "Scottish Higher Grade", "12" = "AS Level",
  "13" = "Trade apprenticeship", "14" = "City and guilds part II, NVQ2",
  "15" = "GCSE grade A-C and equivalent", "16" = "GCSE grade D-E and equivalent",
  "17" = "City and guilds part I, NVQ1", "18" = "Youth training, skill seekers",
  "19" = "Qualification, level unspecified", "20" = "No qualification mentioned",
  "-9" = "Refusal", "-8" = "Don't know/insufficient information",
  "-3" = "Not asked at the fieldwork stage", "-2" = "Schedule not applicable",
  "-1" = "Item not applicable"
)

coll_labels <- c(
  "0" = "NVQ 4–5: degree-level", "1" = "NVQ 1–3: sub-degree",
  "2" = "None/entry", "3" = "Other", "4" = "No qualifications mentioned",
  "-9" = "Refusal", "-8" = "Don't know/insufficient information",
  "-3" = "Not asked at the fieldwork stage", "-2" = "Schedule not applicable",
  "-1" = "Item not applicable"
)

final_df <- merged_data %>%
  select(NSID, educma, educpa, educdtlma, educdtlpa) %>%
  mutate(across(starts_with("educdtl"), ~ factor(.x, levels = as.numeric(names(det_labels)), labels = det_labels))) %>%
  mutate(across(starts_with("educ") & !starts_with("educdtl"), ~ factor(.x, levels = as.numeric(names(coll_labels)), labels = coll_labels)))

write_csv(final_df, "data/output/cleaned_data.csv")
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- list(
  w1 = 'data/input/wave_one_lsype_young_person_2020.tab',
  w2 = 'data/input/wave_two_lsype_family_background_2020.tab',
  w3 = 'data/input/wave_three_lsype_family_background_2020.tab',
  w4 = 'data/input/wave_four_lsype_young_person_2020.tab',
  w9 = 'data/input/ns9_2022_derived_variables.tab'
)

load_tab <- function(path) {
  readr::read_delim(path, delim = "\t", col_types = readr::cols(.default = "c"))
}

df1 <- load_tab(files$w1)
df2 <- load_tab(files$w2)
df3 <- load_tab(files$w3)
df4 <- load_tab(files$w4)
df9 <- load_tab(files$w9)

# 2. Selection and Renaming before join to avoid conflicts and missing object errors
data1 <- df1 %>% select(NSID)
data2 <- df2 %>% select(NSID, IMDRSCORE) %>% rename(imd15_src = IMDRSCORE)
data3 <- df3 %>% select(NSID, IMDRSCORE) %>% rename(imd16_src = IMDRSCORE)
data4 <- df4 %>% select(NSID)
data9 <- df9 %>% select(NSID, W9DIMDD) %>% rename(imd32_src = W9DIMDD)

# Merge datasets by NSID
final_df <- data1 %>%
  full_join(data2, by = "NSID") %>%
  full_join(data3, by = "NSID") %>%
  full_join(data4, by = "NSID") %>%
  full_join(data9, by = "NSID")

# 3. Harmonisation Logic

process_imd_score <- function(x) {
  x_num <- as.numeric(x)
  res <- rep(-3, length(x_num))
  
  # Valid substantive responses
  valid_idx <- !is.na(x_num) & x_num > 0
  res[valid_idx] <- x_num[valid_idx]
  
  # Mapping missing codes based on metadata
  # -94.0: Insufficient Information -> -8
  res[!is.na(x_num) & x_num == -94] <- -8
  # Other negative values in range -999 to -1 -> -2 (Schedule not applicable / info lost)
  res[!is.na(x_num) & x_num >= -999 & x_num <= -1 & x_num != -94] <- -2
  
  # Fill remaining NAs with -3
  res[is.na(res)] <- -3
  return(res)
}

process_imd_decile <- function(x) {
  x_num <- as.numeric(x)
  res <- rep(-3, length(x_num))
  
  # Valid substantive (1 to 10)
  valid_idx <- !is.na(x_num) & x_num >= 1 & x_num <= 10
  res[valid_idx] <- x_num[valid_idx]
  
  # Missing codes
  # -8.0: Insufficient information -> -8
  res[!is.na(x_num) & x_num == -8] <- -8
  
  # Fill NAs with -3
  res[is.na(res)] <- -3
  return(res)
}

final_df <- final_df %>%
  mutate(
    imd15 = process_imd_score(imd15_src),
    imd16 = process_imd_score(imd16_src),
    imd32 = process_imd_decile(imd32_src)
  )

# Create factor for imd32 as per metadata labels
final_df$imd32 <- factor(
  final_df$imd32, 
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, -8, -3),
  labels = c(
    "Most deprived decile", "2", "3", "4", "5", "6", "7", "8", "9", "Least deprived decile",
    "Insufficient information", "Not asked at the fieldwork stage / not interviewed"
  )
)

# 4. Output Requirements
output_df <- final_df %>% select(NSID, imd15, imd16, imd32)

readr::write_csv(output_df, "data/output/cleaned_data.csv")

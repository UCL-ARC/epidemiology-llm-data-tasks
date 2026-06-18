library(readr)
library(dplyr)
library(tidyr)
library(purrr)

# Function to harmonise missing values based on mapping
harmonise_missing <- function(x, mapping) {
  x <- as.numeric(x)
  for (orig in names(mapping)) {
    x[x == as.numeric(orig)] <- as.numeric(mapping[[orig]])
  }
  x[x < 0 & !x %in% mapping] <- -3
  x[is.na(x)] <- -3
  return(x)
}

# Read all files
files <- list(
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_five = "data/input/wave_five_lsype_young_person_2020.tab",
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave_eight = "data/input/ns8_2015_derived.tab",
  wave_nine = "data/input/ns9_2022_derived_variables.tab"
)

# Only load files that contain variables we need (skip wave_one as it has only NSID)
dfs <- lapply(files, function(f) {
  read_delim(f, delim = "\t", col_types = cols(.default = "c"))
})

# Wave 4 (age 17)
df4 <- dfs$wave_four
mapping_w4 <- c("-999" = -2, "-94" = -8, "-92" = -9, "-91" = -1)
df4$W4empsYP <- as.numeric(df4$W4empsYP)
df4$W4empsYP <- harmonise_missing(df4$W4empsYP, mapping_w4)
df4$ecoact17 <- case_when(
  df4$W4empsYP %in% c(1, 2) ~ 1L,
  df4$W4empsYP %in% c(4) ~ 2L,
  df4$W4empsYP %in% c(5) ~ 3L,
  df4$W4empsYP %in% c(3) ~ 4L,
  df4$W4empsYP %in% c(6) ~ 5L,
  df4$W4empsYP %in% c(7, 8, 9) ~ 6L,
  TRUE ~ as.integer(df4$W4empsYP)
)

# Wave 5 (age 18)
df5 <- dfs$wave_five
mapping_w5 <- c("-999" = -2, "-94" = -8, "-92" = -9, "-91" = -1)
df5$W5mainactYP <- as.numeric(df5$W5mainactYP)
df5$W5mainactYP <- harmonise_missing(df5$W5mainactYP, mapping_w5)
df5$ecoact18 <- case_when(
  df5$W5mainactYP %in% c(2, 3) ~ 1L,
  df5$W5mainactYP %in% c(1, 5, 6) ~ 2L,
  df5$W5mainactYP %in% c(4) ~ 3L,
  df5$W5mainactYP %in% c(7) ~ 4L,
  df5$W5mainactYP %in% c(8) ~ 5L,
  df5$W5mainactYP %in% c(9, 10, 11) ~ 6L,
  TRUE ~ as.integer(df5$W5mainactYP)
)

# Wave 6 (age 19)
df6 <- dfs$wave_six
mapping_w6 <- c("-999" = -2, "-94" = -8, "-92" = -9, "-91" = -1)
df6$W6TCurrentAct <- as.numeric(df6$W6TCurrentAct)
df6$W6TCurrentAct <- harmonise_missing(df6$W6TCurrentAct, mapping_w6)
df6$ecoact19 <- case_when(
  df6$W6TCurrentAct %in% c(3, 10) ~ 1L,
  df6$W6TCurrentAct %in% c(4, 5) ~ 2L,
  df6$W6TCurrentAct %in% c(1, 2) ~ 3L,
  df6$W6TCurrentAct %in% c(8) ~ 4L,
  df6$W6TCurrentAct %in% c(7) ~ 5L,
  df6$W6TCurrentAct %in% c(6, 9, 11) ~ 6L,
  TRUE ~ as.integer(df6$W6TCurrentAct)
)

# Wave 7 (age 20)
df7 <- dfs$wave_seven
mapping_w7 <- c("-999" = -2, "-94" = -8, "-92" = -9, "-91" = -1)
df7$W7TCurrentAct <- as.numeric(df7$W7TCurrentAct)
df7$W7TCurrentAct <- harmonise_missing(df7$W7TCurrentAct, mapping_w7)
df7$ecoact20 <- case_when(
  df7$W7TCurrentAct %in% c(3, 9) ~ 1L,
  df7$W7TCurrentAct %in% c(4, 5) ~ 2L,
  df7$W7TCurrentAct %in% c(1, 2) ~ 3L,
  df7$W7TCurrentAct %in% c(8) ~ 4L,
  df7$W7TCurrentAct %in% c(7) ~ 5L,
  df7$W7TCurrentAct %in% c(6, 10, 11, 12, 13, 14, 15) ~ 6L,
  TRUE ~ as.integer(df7$W7TCurrentAct)
)

# Wave 8 (age 25)
df8 <- dfs$wave_eight
# convert to numeric and preserve standard missing codes (-9,-8,-1)
df8$W8DACTIVITYC <- as.numeric(df8$W8DACTIVITYC)
df8$W8DACTIVITYC[is.na(df8$W8DACTIVITYC)] <- -3
# collapsed
df8$ecoact25 <- case_when(
  df8$W8DACTIVITYC %in% c(1, 2) ~ 1L,
  df8$W8DACTIVITYC %in% c(6, 7) ~ 2L,
  df8$W8DACTIVITYC %in% c(5) ~ 3L,
  df8$W8DACTIVITYC %in% c(4) ~ 4L,
  df8$W8DACTIVITYC %in% c(9) ~ 5L,
  df8$W8DACTIVITYC %in% c(3, 8, 10) ~ 6L,
  TRUE ~ as.integer(df8$W8DACTIVITYC)
)
# detailed
df8$ecoactadu25 <- df8$W8DACTIVITYC

# Wave 9 (age 32)
df9 <- dfs$wave_nine
# convert to numeric
df9$W9DACTIVITYC <- as.numeric(df9$W9DACTIVITYC)
df9$W9DACTIVITYC[is.na(df9$W9DACTIVITYC)] <- -3
# collapsed
df9$ecoact32 <- case_when(
  df9$W9DACTIVITYC %in% c(1, 2) ~ 1L,
  df9$W9DACTIVITYC %in% c(6, 7) ~ 2L,
  df9$W9DACTIVITYC %in% c(5) ~ 3L,
  df9$W9DACTIVITYC %in% c(4) ~ 4L,
  df9$W9DACTIVITYC %in% c(9) ~ 5L,
  df9$W9DACTIVITYC %in% c(3, 8, 10) ~ 6L,
  TRUE ~ as.integer(df9$W9DACTIVITYC)
)
# detailed
df9$ecoactadu32 <- df9$W9DACTIVITYC

# Merge datasets
merged <- df4 %>%
  full_join(df5, by = "NSID") %>%
  full_join(df6, by = "NSID") %>%
  full_join(df7, by = "NSID") %>%
  full_join(df8, by = "NSID") %>%
  full_join(df9, by = "NSID")

# Keep only required variables
final_df <- merged %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

write_csv(final_df, "data/output/cleaned_data.csv")

library(readr)
library(dplyr)

# Create output directory if it does not exist
if (!dir.exists("data/output")) dir.create("data/output", recursive = TRUE)

# Helper to harmonise NVQ levels to the 5‑level scheme
map_nvq <- function(x) {
  if (is.null(x)) return(NA_real_)
  x %>%
    replace(x == 0, 1) %>%   # entry level treated as 1
    replace(x == 1, 1) %>%
    replace(x == 2, 2) %>%
    replace(x == 3, 3) %>%
    replace(x == 4, 4) %>%
    replace(x == 5, 5) %>%
    replace(x %in% c(95, 96), 0)
}

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = "c"))
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = "c"))
ns8_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", col_types = cols(.default = "c"))
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = cols(.default = "c"))
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", col_types = cols(.default = "c"))
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = cols(.default = "c"))

# Convert numeric columns
ns8_main <- ns8_main %>% mutate(across(starts_with("W8VCQU0"), as.numeric))
ns8_derived <- ns8_derived %>% mutate(across(c("W8DHANVQH"), as.numeric))
ns9_main <- ns9_main %>% mutate(across(starts_with("W9ACQU0"), as.numeric), across(starts_with("W9VCQU0"), as.numeric))
ns9_derived <- ns9_derived %>% mutate(across(c("W9DANVQH", "W9DVNVQH"), as.numeric))

# Merge by NSID
merged <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(ns8_main, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

# Derive education variables
merged <- merged %>%
  mutate(
    educ25 = map_nvq(W8DHANVQH),
    educ32 = case_when(
      is.na(W9DANVQH) & is.na(W9DVNVQH) ~ NA_real_,
      TRUE ~ pmax(map_nvq(W9DANVQH), map_nvq(W9DVNVQH), na.rm = TRUE)
    ),
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
      W9ACQU0J == 1 ~10,
      W9ACQU0K == 1 ~11,
      W9ACQU0L == 1 ~12,
      W9ACQU0M == 1 ~13,
      W9ACQU0N == 1 ~14,
      W9ACQU0O == 1 ~15,
      W9ACQU0P == 1 ~16,
      W9ACQU0Q == 1 ~17,
      W9ACQU0R == 1 ~18,
      W9ACQU0S == 1 ~0,
      TRUE ~ NA_real_
    ),
    educvdtl32 = case_when(
      W9VCQU0A == 1 ~ 1,
      W9VCQU0B == 1 ~ 2,
      W9VCQU0C == 1 ~ 3,
      W9VCQU0D == 1 ~ 4,
      W9VCQU0E == 1 ~ 5,
      W9VCQU0F == 1 ~ 6,
      W9VCQU0G == 1 ~ 7,
      W9VCQU0H == 1 ~ 8,
      W9VCQU0K == 1 ~ 9,
      W9VCQU0L == 1 ~10,
      W9VCQU0M == 1 ~11,
      W9VCQU0N == 1 ~12,
      W9VCQU0R == 1 ~13,
      W9VCQU0S == 1 ~14,
      W9VCQU0T == 1 ~15,
      W9VCQU0U == 1 ~16,
      W9VCQU0V == 1 ~17,
      W9VCQU0W == 1 ~18,
      W9VCQU0X == 1 ~19,
      W9VCQU0Y == 1 ~20,
      W9VCQU0Z == 1 ~21,
      W9VCQUAA == 1 ~22,
      W9VCQUAB == 1 ~23,
      W9VCQUAC == 1 ~24,
      W9VCQUAD == 1 ~25,
      W9VCQUAE == 1 ~26,
      W9VCQUAF == 1 ~27,
      W9VCQUAG == 1 ~0,
      TRUE ~ NA_real_
    )
  )

# Keep only final variables
final_df <- merged %>% select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write CSV
write_csv(final_df, "data/output/cleaned_data.csv", na = "")

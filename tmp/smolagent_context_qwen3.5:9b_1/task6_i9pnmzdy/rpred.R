library(haven)
library(dplyr)
library(purrr)
library(readr)
library(labelled)

# Load wave 1
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")

# Load wave 2
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")

# Load wave 3
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")

# Load wave 4
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")

# Load ns8
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")

# Load ns9 derived
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Load ns9 main
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Recode urbind in wave2 and wave3 to standard missing codes
# -94 = Insufficient information -> -8
# -999 = Not applicable -> -3
wave2$urbind <- ifelse(wave2$urbind == -94, -8, ifelse(wave2$urbind == -999, -3, wave2$urbind))
wave3$urbind <- ifelse(wave3$urbind == -94, -8, ifelse(wave3$urbind == -999, -3, wave3$urbind))

# Recode gor in wave2 and wave3 to standard missing codes
wave2$gor <- ifelse(wave2$gor == -94, -8, ifelse(wave2$gor == -999, -3, wave2$gor))
wave3$gor <- ifelse(wave3$gor == -94, -8, ifelse(wave3$gor == -999, -3, wave3$gor))

# Recode W8DGOR in ns8 to standard missing codes
ns8$W8DGOR <- ifelse(ns8$W8DGOR == -9, -9, ifelse(ns8$W8DGOR == -8, -8, ifelse(ns8$W8DGOR == -1, -1, ns8$W8DGOR)))

# Recode W9DRGN in ns9_derived to standard missing codes
ns9_derived$W9DRGN <- ifelse(ns9_derived$W9DRGN == -9, -9, ifelse(ns9_derived$W9DRGN == -8, -8, ifelse(ns9_derived$W9DRGN == -1, -1, ns9_derived$W9DRGN)))

# Recode W9NATIONRES in ns9_main to standard missing codes
ns9_main$W9NATIONRES <- ifelse(ns9_main$W9NATIONRES == -9, -9, ifelse(ns9_main$W9NATIONRES == -8, -8, ifelse(ns9_main$W9NATIONRES == -3, -3, ifelse(ns9_main$W9NATIONRES == -1, -1, ns9_main$W9NATIONRES))))

# Merge all datasets by NSID
combined <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID")

# Output cleaned data
write_csv(combined, "data/output/cleaned_data.csv")
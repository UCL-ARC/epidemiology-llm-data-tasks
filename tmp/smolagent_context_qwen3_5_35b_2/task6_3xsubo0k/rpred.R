library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all input files - use read_delim for tab-delimited files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", "\t", col_types = cols())
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", "\t", col_types = cols())
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", "\t", col_types = cols())
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", "\t", col_types = cols())
wave8 <- read_delim("data/input/ns8_2015_derived.tab", "\t", col_types = cols())
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", "\t", col_types = cols())
wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", "\t", col_types = cols())

# Rename columns from wave 2 (age 15) to avoid conflicts
wave2_renamed <- wave2 %>%
  rename(
    urbind_w2 = urbind,
    gor_w2 = gor
  )

# Rename columns from wave 3 (age 16) to avoid conflicts
wave3_renamed <- wave3 %>%
  rename(
    urbind_w3 = urbind,
    gor_w3 = gor
  )

# Merge all files by NSID
cleaned <- full_join(wave1, wave2_renamed, by = "NSID")
cleaned <- full_join(cleaned, wave3_renamed, by = "NSID")
cleaned <- full_join(cleaned, wave4, by = "NSID")
cleaned <- full_join(cleaned, wave8, by = "NSID")
cleaned <- full_join(cleaned, wave9_derived, by = "NSID")
cleaned <- full_join(cleaned, wave9_main, by = "NSID")

# Helper function to convert NA to -3
convert_na_to_missing <- function(x) {
  x[is.na(x)] <- -3
  return(x)
}

# Derive regub15 from W2 urbind_w2 (Age 15)
# Map missing codes according to standard scheme
cleaned$regub15 <- cleaned$urbind_w2
cleaned$regub15[cleaned$urbind_w2 == -94] <- -8  # Insufficient information
cleaned$regub15[cleaned$urbind_w2 %in% c(-999, -998, -997, -995)] <- -2  # Schedule not applicable
cleaned$regub15[cleaned$urbind_w2 == -9] <- -9  # Refusal
cleaned$regub15[cleaned$urbind_w2 == -8] <- -8  # Don't know
cleaned$regub15[cleaned$urbind_w2 == -7] <- -7  # Prefer not to say
cleaned$regub15[cleaned$urbind_w2 == -3] <- -3  # Not asked
cleaned$regub15[cleaned$urbind_w2 == -2] <- -2  # Not applicable
cleaned$regub15[cleaned$urbind_w2 == -1] <- -1  # Item not applicable
cleaned$regub15 <- convert_na_to_missing(cleaned$regub15)

# Derive regub16 from W3 urbind_w3 (Age 16)
cleaned$regub16 <- cleaned$urbind_w3
cleaned$regub16[cleaned$urbind_w3 == -94] <- -8  # Insufficient information
cleaned$regub16[cleaned$urbind_w3 %in% c(-999, -998, -997, -995)] <- -2  # Schedule not applicable
cleaned$regub16[cleaned$urbind_w3 == -9] <- -9  # Refusal
cleaned$regub16[cleaned$urbind_w3 == -8] <- -8  # Don't know
cleaned$regub16[cleaned$urbind_w3 == -7] <- -7  # Prefer not to say
cleaned$regub16[cleaned$urbind_w3 == -3] <- -3  # Not asked
cleaned$regub16[cleaned$urbind_w3 == -2] <- -2  # Not applicable
cleaned$regub16[cleaned$urbind_w3 == -1] <- -1  # Item not applicable
cleaned$regub16 <- convert_na_to_missing(cleaned$regub16)

# Derive regov15 from W2 gor_w2 (Age 15)
cleaned$regov15 <- cleaned$gor_w2
cleaned$regov15[cleaned$gor_w2 == -94] <- -8  # Insufficient information
cleaned$regov15[cleaned$gor_w2 %in% c(-999, -998, -997, -995)] <- -2  # Schedule not applicable
cleaned$regov15[cleaned$gor_w2 == -9] <- -9  # Refusal
cleaned$regov15[cleaned$gor_w2 == -8] <- -8  # Don't know
cleaned$regov15[cleaned$gor_w2 == -7] <- -7  # Prefer not to say
cleaned$regov15[cleaned$gor_w2 == -3] <- -3  # Not asked
cleaned$regov15[cleaned$gor_w2 == -2] <- -2  # Not applicable
cleaned$regov15[cleaned$gor_w2 == -1] <- -1  # Item not applicable
cleaned$regov15 <- convert_na_to_missing(cleaned$regov15)

# Derive regov16 from W3 gor_w3 (Age 16)
cleaned$regov16 <- cleaned$gor_w3
cleaned$regov16[cleaned$gor_w3 == -94] <- -8  # Insufficient information
cleaned$regov16[cleaned$gor_w3 %in% c(-999, -998, -997, -995)] <- -2  # Schedule not applicable
cleaned$regov16[cleaned$gor_w3 == -9] <- -9  # Refusal
cleaned$regov16[cleaned$gor_w3 == -8] <- -8  # Don't know
cleaned$regov16[cleaned$gor_w3 == -7] <- -7  # Prefer not to say
cleaned$regov16[cleaned$gor_w3 == -3] <- -3  # Not asked
cleaned$regov16[cleaned$gor_w3 == -2] <- -2  # Not applicable
cleaned$regov16[cleaned$gor_w3 == -1] <- -1  # Item not applicable
cleaned$regov16 <- convert_na_to_missing(cleaned$regov16)

# Derive regor25 from W8DGOR (Age 25)
# Map source value 13 to -2 (Unknown due to faulty/missing postcode)
cleaned$regor25 <- cleaned$W8DGOR
cleaned$regor25[cleaned$W8DGOR == 13] <- -2  # Unknown due to faulty/missing postcode
cleaned$regor25[cleaned$W8DGOR == -9] <- -9  # Refusal
cleaned$regor25[cleaned$W8DGOR == -8] <- -8  # Insufficient information
cleaned$regor25[cleaned$W8DGOR == -1] <- -1  # Not applicable
cleaned$regor25 <- convert_na_to_missing(cleaned$regor25)

# Derive regor32 from W9DRGN (Age 32)
# Map source value 13 to -2 (Unknown due to faulty/missing postcode)
cleaned$regor32 <- cleaned$W9DRGN
cleaned$regor32[cleaned$W9DRGN == 13] <- -2  # Unknown due to faulty/missing postcode
cleaned$regor32[cleaned$W9DRGN == -9] <- -9  # Refusal
cleaned$regor32[cleaned$W9DRGN == -8] <- -8  # Insufficient information
cleaned$regor32[cleaned$W9DRGN == -1] <- -1  # Not applicable
cleaned$regor32 <- convert_na_to_missing(cleaned$regor32)

# Derive regint32 from W9NATIONRES (Age 32)
# 1 = In UK (England, Scotland, Wales, Northern Ireland), 2 = Abroad (Outside UK or unknown)
cleaned$regint32 <- cleaned$W9NATIONRES
cleaned$regint32[cleaned$W9NATIONRES %in% c(1, 2, 3, 4)] <- 1  # In UK
cleaned$regint32[cleaned$W9NATIONRES == 5] <- 2  # Outside UK or unknown
cleaned$regint32[cleaned$W9NATIONRES == -9] <- -9  # Refusal
cleaned$regint32[cleaned$W9NATIONRES == -8] <- -8  # Don't know
cleaned$regint32[cleaned$W9NATIONRES == -3] <- -3  # Not asked
cleaned$regint32[cleaned$W9NATIONRES == -1] <- -1  # Not applicable
cleaned$regint32 <- convert_na_to_missing(cleaned$regint32)

# Remove raw source variables - keep only final derived variables
keep_vars <- c("NSID", "regub15", "regub16", "regov15", "regov16", "regor25", "regor32", "regint32")
cleaned_final <- cleaned %>% select(all_of(keep_vars))

# Write output
write_csv(cleaned_final, "data/output/cleaned_data.csv")

# Verify output
print(paste("Output dimensions:", nrow(cleaned_final), "rows x", ncol(cleaned_final), "columns"))
print(head(cleaned_final))
print(paste("Any NA values remaining:", sum(is.na(cleaned_final))))
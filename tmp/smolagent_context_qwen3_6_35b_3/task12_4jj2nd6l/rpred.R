library(dplyr)
library(readr)
library(tidyr)

# Create output directory if needed
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Helper function: map negative codes to standard missing-value scheme
map_missing_standard <- function(x) {
  ifelse(x == -9, -9,
    ifelse(x == -8, -8,
      ifelse(x == -7, -7,
        ifelse(x == -3, -3,
          ifelse(x == -2, -2,
            ifelse(x == -1, -1,
              ifelse(x == -99, -3,
                ifelse(x == -91, -1,
                  ifelse(x == -999, -2,
                    ifelse(x == -998, -2,
                      ifelse(x == -997, -9,
                        ifelse(x == -995, -2,
                          ifelse(x == -94, -8,
                            ifelse(x == -92, -9,
                              ifelse(x == -100, -2,
                                ifelse(x == -97, -2,
                                  -3
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

# Helper function: floor NS-SEC codes (convert fractional to major category)
# while preserving missing-value codes
floor_nssec <- function(x) {
  ifelse(x >= 0, floor(x), x)
}

# ===== Load all files =====

# Wave 4 (Age 17)
df4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df4$nssec17 <- map_missing_standard(df4$W4nsseccatYP)
df4$nssec17 <- floor_nssec(df4$nssec17)

# Wave 5 (Age 18)
df5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df5$nssec18 <- map_missing_standard(df5$W5nsseccatYP)
df5$nssec18 <- floor_nssec(df5$nssec18)

# Wave 6 (Age 19)
df6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df6$nssec19 <- map_missing_standard(df6$w6nsseccatYP)
df6$nssec19 <- floor_nssec(df6$nssec19)

# Wave 7 (Age 20)
df7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df7$nssec20 <- map_missing_standard(df7$W7NSSECCat)
df7$nssec20 <- floor_nssec(df7$nssec20)

# Wave 8 (Age 25)
df8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)

# Special logic for nssec25: if W8DACTIVITYC == 5 (full-time education), assign 15
# Otherwise, floor the W8DNSSEC17 code
nssec25_raw <- ifelse(df8$W8DACTIVITYC == 5, 15,
                 ifelse(df8$W8DACTIVITYC == 5.0, 15,
                        map_missing_standard(df8$W8DNSSEC17)))
df8$nssec25 <- floor_nssec(nssec25_raw)

# Wave 9 (Age 32)
df9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)
df9$nssec32 <- map_missing_standard(df9$W9NSSEC)
df9$nssec32 <- floor_nssec(df9$nssec32)

# ===== Merge all datasets =====
result <- df4 %>%
  select(NSID, nssec17) %>%
  full_join(df5 %>% select(NSID, nssec18), by = "NSID") %>%
  full_join(df6 %>% select(NSID, nssec19), by = "NSID") %>%
  full_join(df7 %>% select(NSID, nssec20), by = "NSID") %>%
  full_join(df8 %>% select(NSID, nssec25), by = "NSID") %>%
  full_join(df9 %>% select(NSID, nssec32), by = "NSID")

# ===== Write output =====
write_csv(result, "data/output/cleaned_data.csv")

cat("Output written successfully.\n")
cat("Number of rows:", nrow(result), "\n")
cat("Columns:", paste(names(result), collapse = ", "), "\n")

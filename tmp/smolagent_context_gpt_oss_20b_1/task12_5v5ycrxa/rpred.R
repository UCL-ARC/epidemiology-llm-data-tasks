library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

input_dir <- "data/input"
output_dir <- "data/output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
output_file <- file.path(output_dir, "cleaned_data.csv")

file_names <- c(
  "wave_one_lsype_young_person_2020.tab",
  "ns9_2022_main_interview.tab",
  "ns8_2015_derived.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "wave_five_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab"
)

# Read files safely
read_file_safely <- function(fname) {
  path <- file.path(input_dir, fname)
  if (!file.exists(path) || file.info(path)$size == 0) return(NULL)
  tryCatch(read_delim(path, delim="\t", col_types=cols(), progress=FALSE), error=function(e) NULL)
}

data_list <- lapply(file_names, read_file_safely)
names(data_list) <- file_names
# Remove NULLs
data_list <- Filter(Negate(is.null), data_list)

# Merge
merged <- reduce(data_list, full_join, by="NSID")

# Function to map missing codes
map_missing <- function(x) {
  if (!is.numeric(x)) return(x)
  case_when(
    x %in% c(-9,-8,-7,-3,-2,-1) ~ x,
    x %in% c(-91,-99) ~ ifelse(x==-91,-1,-3),
    x %in% c(-999) ~ -2,
    TRUE ~ x
  )
}

# Add raw columns if present
if ("W4nsseccatYP" %in% names(merged)) merged <- merged %>% mutate(nssec17_raw = map_missing(`W4nsseccatYP`)) else merged$nssec17_raw <- NA_real_
if ("W5nsseccatYP" %in% names(merged)) merged <- merged %>% mutate(nssec18_raw = map_missing(`W5nsseccatYP`)) else merged$nssec18_raw <- NA_real_
if ("w6nsseccatYP" %in% names(merged)) merged <- merged %>% mutate(nssec19_raw = map_missing(`w6nsseccatYP`)) else merged$nssec19_raw <- NA_real_
if ("W7NSSECCat" %in% names(merged)) merged <- merged %>% mutate(nssec20_raw = map_missing(`W7NSSECCat`)) else merged$nssec20_raw <- NA_real_
if ("W8DNSSEC17" %in% names(merged)) merged <- merged %>% mutate(nssec25_raw = map_missing(`W8DNSSEC17`)) else merged$nssec25_raw <- NA_real_
if ("W8DACTIVITYC" %in% names(merged)) merged <- merged %>% mutate(activity25_raw = map_missing(`W8DACTIVITYC`)) else merged$activity25_raw <- NA_real_
if ("W9NSSEC" %in% names(merged)) merged <- merged %>% mutate(nssec32_raw = map_missing(`W9NSSEC`)) else merged$nssec32_raw <- NA_real_

# Collapse fractional codes
merged <- merged %>%
  mutate(
    nssec17 = ifelse(nssec17_raw %in% c(-9,-8,-7,-3,-2,-1), nssec17_raw, floor(nssec17_raw)),
    nssec18 = ifelse(nssec18_raw %in% c(-9,-8,-7,-3,-2,-1), nssec18_raw, floor(nssec18_raw)),
    nssec19 = ifelse(nssec19_raw %in% c(-9,-8,-7,-3,-2,-1), nssec19_raw, floor(nssec19_raw)),
    nssec20 = ifelse(nssec20_raw %in% c(-9,-8,-7,-3,-2,-1), nssec20_raw, floor(nssec20_raw)),
    nssec32 = ifelse(nssec32_raw %in% c(-9,-8,-7,-3,-2,-1), nssec32_raw, floor(nssec32_raw))
  )

# nssec25 special rule
nssec25_collapsed <- ifelse(merged$nssec25_raw %in% c(-9,-8,-7,-3,-2,-1), merged$nssec25_raw, floor(merged$nssec25_raw))
merged$nssec25 <- ifelse(merged$activity25_raw == 5, 15, nssec25_collapsed)

# Labels
codes <- c(-9,-8,-7,-3,-2,-1,1:17)
labels <- c(
  "-9 Refusal",
  "-8 Don\'t know / insufficient information",
  "-7 Prefer not to say",
  "-3 Not asked at fieldwork stage / not interviewed",
  "-2 Schedule not applicable / script error / information lost",
  "-1 Item not applicable",
  "Employers in large organisations",
  "Higher managerial and administrative occupations",
  "Higher professional occupations",
  "Lower professional and higher technical occupations",
  "Lower managerial and administrative occupations",
  "Higher supervisory occupations",
  "Intermediate occupations",
  "Employers in small establishments",
  "Own account workers",
  "Lower supervisory occupations",
  "Lower technical occupations",
  "Semi-routine occupations",
  "Routine occupations",
  "Never worked and Long-term unemployed",
  "Full-time students",
  "Occupations not stated or inadequately described",
  "Not classifiable for other reasons"
)

merged <- merged %>%
  mutate(
    nssec17 = factor(nssec17, levels=codes, labels=labels),
    nssec18 = factor(nssec18, levels=codes, labels=labels),
    nssec19 = factor(nssec19, levels=codes, labels=labels),
    nssec20 = factor(nssec20, levels=codes, labels=labels),
    nssec25 = factor(nssec25, levels=codes, labels=labels),
    nssec32 = factor(nssec32, levels=codes, labels=labels)
  )

final_df <- merged %>% select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)
write_csv(final_df, output_file, na="")
cat("Cleaning complete. Output written to", output_file, "\n")

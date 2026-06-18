library(readr)
library(dplyr)

# Utility functions -------------------------------------------------
read_if_exists <- function(path){
  if(file.exists(path) && file.info(path)$size>0){
    read_delim(path,delim="\t",col_types=cols(.default=col_guess()),show_col_types=FALSE)
  } else NULL
}
standardise_missing <- function(x){
  case_when(
    is.na(x) ~ NA_real_,
    x %in% c(-999,-998,-997,-995) ~ -9,
    x %in% c(-99) ~ -3,
    x %in% c(-91) ~ -1,
    x %in% c(-9) ~ -9,
    x %in% c(-8) ~ -8,
    x %in% c(-7) ~ -7,
    x %in% c(-3) ~ -3,
    x %in% c(-2) ~ -2,
    x %in% c(-1) ~ -1,
    TRUE ~ x
  )
}

# Load files -----------------------------------------------------------
file_list <- list(
  "wave_one_lsype_young_person_2020.tab" = c("NSID"),
  "wave_four_lsype_young_person_2020.tab" = c("NSID","W4nsseccatYP"),
  "wave_five_lsype_young_person_2020.tab" = c("NSID","W5nsseccatYP"),
  "wave_six_lsype_young_person_2020.tab" = c("NSID","w6nsseccatYP"),
  "wave_seven_lsype_young_person_2020.tab" = c("NSID","W7NSSECCat"),
  "ns8_2015_derived.tab" = c("NSID","W8DACTIVITYC","W8DNSSEC17"),
  "ns9_2022_main_interview.tab" = c("NSID","W9NSSEC")
)

# Read each file if present
dfs <- lapply(names(file_list), function(f){
  path <- file.path("data","input",f)
  df <- read_if_exists(path)
  if(!is.null(df)){
    missing <- setdiff(file_list[[f]], names(df))
    if(length(missing)>0) df[missing] <- NA_real_
  }
  df
})

# Remove NULL entries
dfs <- dfs[!sapply(dfs, is.null)]

# Merge by NSID using full_join
merged <- dfs[[1]]
if(length(dfs)>1){
  for(i in 2:length(dfs)){
    merged <- full_join(merged, dfs[[i]], by="NSID")
  }
}

# Helper to safely extract column
get_col <- function(col, def = NA_real_){
  if(col %in% names(merged)) merged[[col]] else def
}

# Derive NS-SEC variables (vectorised)
nssec17_val <- ifelse(is.na(v <- standardise_missing(get_col("W4nsseccatYP"))), -3, floor(v))
nssec18_val <- ifelse(is.na(v <- standardise_missing(get_col("W5nsseccatYP"))), -3, floor(v))
nssec19_val <- ifelse(is.na(v <- standardise_missing(get_col("w6nsseccatYP"))), -3, floor(v))
nssec20_val <- ifelse(is.na(v <- standardise_missing(get_col("W7NSSECCat"))), -3, floor(v))

# nssec25 special rule
act <- get_col("W8DACTIVITYC")
nssec25_val <- ifelse(!is.na(act) & act==5, 15, ifelse(is.na(v <- standardise_missing(get_col("W8DNSSEC17"))), -3, floor(v)))

# nssec32
nssec32_val <- ifelse("W9NSSEC" %in% names(merged), ifelse(is.na(v <- standardise_missing(get_col("W9NSSEC"))), -3, floor(v)), NA_real_)

# Build final dataframe
final_df <- data.frame(
  NSID = merged$NSID,
  nssec17 = nssec17_val,
  nssec18 = nssec18_val,
  nssec19 = nssec19_val,
  nssec20 = nssec20_val,
  nssec25 = nssec25_val,
  nssec32 = nssec32_val,
  stringsAsFactors = FALSE
)

# Write to CSV
write_csv(final_df, "data/output/cleaned_data.csv")

cat("Finished writing cleaned_data.csv\n")

library(haven)
library(dplyr)
library(readr)

# Create output directory
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

files <- list(
  wave_14 = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_17 = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_18 = "data/input/wave_five_lsype_young_person_2020.tab",
  wave_19 = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_20 = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave_25 = "data/input/ns8_2015_derived.tab",
  wave_32 = "data/input/ns9_2022_main_interview.tab"
)

loaded_data <- list()
for (wave_name in names(files)) {
  file_path <- files[[wave_name]]
  cat("Loading:", file_path, "\n")
  df <- read_delim(file_path, delim = "\t")
  loaded_data[[wave_name]] <- df
  cat("Loaded ", nrow(df), "rows\n")
}

# Standard labels for NS-SEC categories
nssec_labels <- c(
  "1" = "Employers in large organisations",
  "2" = "Higher managerial and administrative occupations",
  "3" = "Higher professional occupations",
  "4" = "Lower professional and higher technical occupations",
  "5" = "Lower managerial and administrative occupations",
  "6" = "Higher supervisory occupations",
  "7" = "Intermediate occupations",
  "8" = "Employers in small establishments",
  "9" = "Own account workers",
  "10" = "Lower supervisory occupations",
  "11" = "Lower technical occupations",
  "12" = "Semi-routine occupations",
  "13" = "Routine occupations",
  "14" = "Never worked and Long-term unemployed",
  "15" = "Full-time students",
  "16" = "Occupations not stated or inadequately described",
  "17" = "Not classifiable for other reasons",
  "-1" = "Not applicable",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-7" = "Prefer not to say",
  "-8" = "Don't know/insufficient information",
  "-9" = "Refusal"
)

process_nssec <- function(df, wave_name, nssec_var, age) {
  if (is.null(nssec_var)) {
    return(df)
  }
  
  df <- df %>%
    mutate(
      raw_nssec = !!sym(nssec_var),
      nssec = raw_nssec
    )
  
  # Harmonize missing values
  df <- df %>%
    mutate(
      nssec = case_when(
        is.na(nssec) ~ -3,
        nssec == -99 ~ -3,
        nssec == -999 ~ -3,
        nssec == -1 ~ -1,
        nssec == -91 ~ -1,
        nssec == -9 ~ -9,
        nssec == -8 ~ -8,
        TRUE ~ nssec
      )
    )
  
  # Collapse fractional codes to integers using floor(abs())
  df <- df %>%
    mutate(
      nssec = case_when(
        nssec <= 0 | nssec > 17 ~ nssec,
        TRUE ~ floor(abs(nssec))
      )
    )
  
  # Ensure only valid categories (1-17) or missing codes
  df <- df %>%
    mutate(
      nssec = case_when(
        nssec >= 1 & nssec <= 17 | nssec >= -9 & nssec <= -1 ~ nssec,
        TRUE ~ -3
      )
    )
  
  # Create factor by converting to factor and assigning labels
  nssec_levels <- sort(unique(df$nssec))
  nssec_labels_vec <- nssec_labels[as.character(nssec_levels)]
  
  df[[paste0("nssec", age)]] <- factor(df$nssec, 
    levels = as.character(nssec_levels),
    labels = nssec_labels_vec
  )
  
  df$nssec <- NULL
  df$raw_nssec <- NULL
  
  return(df)
}

nssec_vars <- list(
  wave_14 = NULL,
  wave_17 = "W4nsseccatYP",
  wave_18 = "W5nsseccatYP",
  wave_19 = "w6nsseccatYP",
  wave_20 = "W7NSSECCat",
  wave_25 = "W8DNSSEC17",
  wave_32 = "W9NSSEC"
)

for (wave_name in names(nssec_vars)) {
  nssec_var <- nssec_vars[[wave_name]]
  age <- as.integer(sub("wave_", "", wave_name))
  
  if (!is.null(nssec_var)) {
    cat("Processing", wave_name, "(age", age, ")\n")
    loaded_data[[wave_name]] <- process_nssec(loaded_data[[wave_name]], wave_name, nssec_var, age)
  }
}

combined <- loaded_data[["wave_14"]]
cat("Starting merge with", nrow(combined), "rows\n")

for (wave_name in names(loaded_data)) {
  if (wave_name != "wave_14") {
    combined <- full_join(combined, loaded_data[[wave_name]], by = "NSID")
    cat("Merged", wave_name, "-", nrow(combined), "rows\n")
  }
}

final_output <- combined %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

cat("Final output has", nrow(final_output), "rows and", ncol(final_output), "columns\n")
cat("Variables:", paste(names(final_output), collapse = ", "), "\n")

write_csv(final_output, "data/output/cleaned_data.csv")
cat("\nScript completed successfully!\n")
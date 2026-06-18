library(dplyr)
library(readr)
library(haven)
library(labelled)
library(purrr)
library(tidyr)

# Define file paths
files <- c(
  "ns9_2022_derived_variables.tab",
  "wave_four_lsype_family_background_2020.tab",
  "wave_three_lsype_family_background_2020.tab",
  "wave_two_lsype_family_background_2020.tab",
  "wave_one_lsype_family_background_2020.tab",
  "ns8_2015_main_interview.tab",
  "wave_five_lsype_family_background_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab"
)

# Load all files
files_list <- lapply(files, function(f) {
  read_delim(file.path("data/input", f), delim = "\t", show_col_types = FALSE)
})
names(files_list) <- files

# Start with the first file (wave 1) to have NSID
merged <- files_list[["wave_one_lsype_family_background_2020.tab"]]

# Merge all files
for (f in files[-1]) {
  merged <- full_join(merged, files_list[[f]], by = "NSID")
}

cat("Merged dataset dimensions:", dim(merged), "\n")
cat("Number of unique NSIDs:", nrow(unique(merged[, "NSID"])), "\n")

# Define source variable names for each age
detailed_waves <- c("14" = "W1hous12HH",
                    "15" = "W2Hous12HH",
                    "16" = "W3hous12HH",
                    "17" = "W4Hous12HH",
                    "18" = "W5Hous12HH",
                    "19" = "W6Hous12YP",
                    "20" = "W7Hous12YP")

# Map detailed tenure codes
map_detailed <- function(x) {
  result <- x
  result[result == 1] <- 1
  result[result == 2] <- 2
  result[result == 3] <- 3
  result[result == 4] <- 4
  result[result == 5] <- 4
  result[result == 6] <- 4
  result[result == 7] <- 5
  result[result == 8] <- 7
  
  result[result == -999] <- -3
  result[result == -998] <- -2
  result[result == -997] <- -2
  result[result == -995] <- -2
  result[result == -94] <- -8
  result[result == -92] <- -9
  result[result == -91] <- -1
  result[result == -1] <- -3
  result[result == -8] <- -8
  result[result == -7] <- -7
  result[result == -9] <- -9
  result[result == -2] <- -2
  result[result == -3] <- -3
  
  result[is.na(result)] <- -3
  return(result)
}

map_w6_w7_detailed <- function(type_var, owned_var, rented_var) {
  result <- rep(-3, length(type_var))
  
  owned_idx <- !is.na(type_var) & type_var == 1
  owned_valid <- !is.na(owned_var) & owned_var %in% c(1, 2, 3, 4)
  if (sum(owned_idx & owned_valid) > 0) {
    owned_vals <- owned_var[owned_idx & owned_valid]
    mapped_owned <- owned_vals
    mapped_owned[mapped_owned == 4] <- 7
    result[owned_idx & owned_valid] <- mapped_owned
  }
  
  rented_idx <- !is.na(type_var) & type_var == 2
  result[rented_idx] <- 4
  
  other_idx <- !is.na(type_var) & type_var == 3
  result[other_idx] <- 7
  
  result[is.na(result)] <- -3
  return(result)
}

# Process detailed tenure for waves 14-18
for (age in c("14", "15", "16", "17", "18")) {
  src_var <- detailed_waves[age]
  out_var <- paste0("hownteen", age)
  
  if (src_var %in% names(merged)) {
    mapped <- map_detailed(merged[[src_var]])
    merged[[out_var]] <- mapped
  } else {
    merged[[out_var]] <- -3
  }
}

# Handle W6 (age 19)
if ("W6Hous12YP" %in% names(merged) && "W6Hous12bYP" %in% names(merged) && "W6Hous12cYP" %in% names(merged)) {
  merged[["hownteen19"]] <- map_w6_w7_detailed(merged[["W6Hous12YP"]], 
                                                  merged[["W6Hous12bYP"]], 
                                                  merged[["W6Hous12cYP"]])
} else {
  merged[["hownteen19"]] <- -3
}

# Handle W7 (age 20)
if ("W7Hous12YP" %in% names(merged) && "W7Hous12bYP" %in% names(merged) && "W7Hous12cYP" %in% names(merged)) {
  merged[["hownteen20"]] <- map_w6_w7_detailed(merged[["W7Hous12YP"]], 
                                                  merged[["W7Hous12bYP"]], 
                                                  merged[["W7Hous12cYP"]])
} else {
  merged[["hownteen20"]] <- -3
}

# Collapsed hown variables
collapsed_ages <- c("14", "15", "16", "17", "18", "19", "20", "25", "32")

map_collapsed_detailed <- function(x) {
  result <- x
  result[result == 1] <- 1
  result[result == 2] <- 2
  result[result == 3] <- 3
  result[result == 4] <- 4
  result[result == 5] <- 5
  result[result == 6] <- 6
  result[result == 7] <- 7
  result[result == 8] <- 7
  
  result[result == -999] <- -3
  result[result == -998] <- -2
  result[result == -997] <- -2
  result[result == -995] <- -2
  result[result == -94] <- -8
  result[result == -92] <- -9
  result[result == -91] <- -1
  result[result == -1] <- -3
  result[result == -8] <- -8
  result[result == -7] <- -7
  result[result == -9] <- -9
  result[result == -2] <- -2
  result[result == -3] <- -3
  
  result[is.na(result)] <- -3
  return(result)
}

map_collapsed_type <- function(x) {
  result <- x
  result[result == 1] <- 1
  result[result == 2] <- 4
  result[result == 3] <- 7
  
  result[result == -999] <- -3
  result[result == -998] <- -2
  result[result == -997] <- -2
  result[result == -995] <- -2
  result[result == -94] <- -8
  result[result == -92] <- -9
  result[result == -91] <- -1
  result[result == -1] <- -3
  result[result == -8] <- -8
  result[result == -7] <- -7
  result[result == -9] <- -9
  result[result == -2] <- -2
  result[result == -3] <- -3
  
  result[is.na(result)] <- -3
  return(result)
}

# Process collapsed variables
for (age in c("14", "15", "16", "17", "18")) {
  src_var <- detailed_waves[age]
  out_var <- paste0("hown", age)
  
  if (src_var %in% names(merged)) {
    mapped <- map_collapsed_detailed(merged[[src_var]])
    merged[[out_var]] <- mapped
  } else {
    merged[[out_var]] <- -3
  }
}

if ("W6Hous12YP" %in% names(merged)) {
  merged[["hown19"]] <- map_collapsed_type(merged[["W6Hous12YP"]])
} else {
  merged[["hown19"]] <- -3
}

if ("W7Hous12YP" %in% names(merged)) {
  merged[["hown20"]] <- map_collapsed_type(merged[["W7Hous12YP"]])
} else {
  merged[["hown20"]] <- -3
}

if ("W8TENURE" %in% names(merged)) {
  mapped <- map_collapsed_detailed(merged[["W8TENURE"]])
  merged[["hown25"]] <- mapped
} else {
  merged[["hown25"]] <- -3
}

if ("W9DTENURE" %in% names(merged)) {
  mapped <- map_collapsed_detailed(merged[["W9DTENURE"]])
  merged[["hown32"]] <- mapped
} else {
  merged[["hown32"]] <- -3
}

# Try to apply labels using labelled package
tryCatch({
  detailed_labels <- c(`1` = "Own outright", `2` = "Own with mortgage", `3` = "Shared ownership",
                       `4` = "Rent", `5` = "Rent-free", `6` = "Squatting", `7` = "Other",
                       `-9` = "Refusal", `-8` = "Don't know/insufficient information",
                       `-7` = "Prefer not to say", `-3` = "Not asked/not interviewed",
                       `-2` = "Schedule not applicable", `-1` = "Item not applicable")
  
  collapsed_labels <- c(`1` = "Own outright", `2` = "Own with mortgage", `3` = "Shared ownership",
                        `4` = "Rent it", `5` = "Rent-free", `6` = "Squatting", `7` = "Other",
                        `-9` = "Refusal", `-8` = "Don't know/insufficient information",
                        `-7` = "Prefer not to say", `-3` = "Not asked/not interviewed",
                        `-2` = "Schedule not applicable", `-1` = "Item not applicable")
  
  for (age in c("14", "15", "16", "17", "18", "19", "20")) {
    out_var <- paste0("hownteen", age)
    if (out_var %in% names(merged)) {
      merged[[out_var]] <- labelled::set_labels(merged[[out_var]], detailed_labels)
    }
  }
  
  for (age in collapsed_ages) {
    out_var <- paste0("hown", age)
    if (out_var %in% names(merged)) {
      merged[[out_var]] <- labelled::set_labels(merged[[out_var]], collapsed_labels)
    }
  }
  
  cat("Labels applied successfully\n")
}, error = function(e) {
  cat("Warning: Could not apply labels:", conditionMessage(e), "\n")
})

# Select only NSID and final derived variables
final_vars <- c("NSID", 
                paste0("hownteen", c("14", "15", "16", "17", "18", "19", "20")),
                paste0("hown", collapsed_ages))

output <- merged %>% select(all_of(final_vars))

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Output dimensions:", dim(output), "\n")
cat("Output columns:", names(output), "\n")

# Print summary for a few variables
for (v in c("NSID", "hownteen14", "hown14", "hown32")) {
  if (v %in% names(output)) {
    cat(sprintf("\n%s:\n", v))
    print(table(output[[v]], useNA = "ifany"))
  }
}
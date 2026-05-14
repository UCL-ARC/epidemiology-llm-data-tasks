library(haven)
library(dplyr)
library(purrr)
library(readr)

# Load datasets
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

# Load and merge datasets
data_frames <- lapply(files, function(file) {
  readr::read_delim(paste0("data/input/", file), delim = "\t")
})

merged_data <- data_frames[[1]]
for (i in 2:length(data_frames)) {
  merged_data <- full_join(merged_data, data_frames[[i]], by = "NSID")
  cat("Merged data has", nrow(merged_data), "rows after joining", i, "datasets\n")
}

# Create detailed variables for ages 14-17
for (age in 14:17) {
  var_name <- case_when(
    age == 14 ~ "W1hous12HH",
    age == 15 ~ "W2Hous12HH",
    age == 16 ~ "W3hous12HH",
    age == 17 ~ "W4Hous12HH"
  )

  if (var_name %in% names(merged_data)) {
    cat("Processing age", age, "variable\n")
    merged_data[[paste0("hownteen", age)]] <- case_when(
      merged_data[[var_name]] == 1 ~ "Owned outright",
      merged_data[[var_name]] == 2 ~ "Being bought on a mortgage/bank loan",
      merged_data[[var_name]] == 3 ~ "Shared ownership (owns & rents property)",
      merged_data[[var_name]] == 4 ~ "Rented from a Council or New Town",
      merged_data[[var_name]] == 5 ~ "Rented from a Housing Association",
      merged_data[[var_name]] == 6 ~ "Rented privately",
      merged_data[[var_name]] == 7 ~ "Rent free",
      merged_data[[var_name]] == 8 ~ "Some other arrangement",
      merged_data[[var_name]] == -999 ~ "Not asked",
      merged_data[[var_name]] == -997 ~ "Script error",
      merged_data[[var_name]] == -99 ~ "Script error",
      merged_data[[var_name]] == -98 ~ "Script error",
      merged_data[[var_name]] == -92 ~ "Refusal",
      merged_data[[var_name]] == -91 ~ "Item not applicable",
      merged_data[[var_name]] == -1 ~ "Don't know",
      TRUE ~ "Not asked"
    )
    
    levels <- c("Owned outright", "Being bought on a mortgage/bank loan", "Shared ownership (owns & rents property)",
                "Rented from a Council or New Town", "Rented from a Housing Association", "Rented privately",
                "Rent free", "Some other arrangement", "Item not applicable", "Script error",
                "Not asked", "Don't know", "Refusal")
    merged_data[[paste0("hownteen", age)]] <- factor(merged_data[[paste0("hownteen", age)]], levels = levels)
  }
}

# Create split variables for ages 18-20
for (age in c(5, 6, 7)) {
  if (age == 5) {
    owned_var <- "W5Hous12BHH"
    rented_var <- "W5Hous12CHH"
    type_var <- "W5Hous12HH"
  } else if (age == 6) {
    owned_var <- "W6Hous12bYP"
    rented_var <- "W6Hous12cYP"
    type_var <- "W6Hous12YP"
  } else if (age == 7) {
    owned_var <- "W7Hous12bYP"
    rented_var <- "W7Hous12cYP"
    type_var <- "W7Hous12YP"
  }

  if (!all(c(owned_var, rented_var, type_var) %in% names(merged_data))) {
    cat("Skipping age", age + 13, "due to missing variables\n")
    next
  }

  cat("Processing age", age + 13, "split variables\n")
  merged_data[[paste0("hownteen", age + 13)]] <- "Not asked"
  
  merged_data[[paste0("hownteen", age + 13)]] <- case_when(
    merged_data[[type_var]] == 1 & !is.na(merged_data[[owned_var]]) ~ case_when(
      merged_data[[owned_var]] == 1 ~ "Owned outright",
      merged_data[[owned_var]] == 2 ~ "Being bought on a mortgage/bank loan",
      merged_data[[owned_var]] == 3 ~ "Shared ownership (owns & rents property)",
      merged_data[[owned_var]] == 4 ~ "Some other arrangement"
    ),
    merged_data[[type_var]] == 2 & !is.na(merged_data[[rented_var]]) ~ case_when(
      merged_data[[rented_var]] == 1 ~ "Rented from a Council or New Town",
      merged_data[[rented_var]] == 2 ~ "Rented from a Housing Association",
      merged_data[[rented_var]] == 3 ~ "Rented privately",
      merged_data[[rented_var]] == 4 ~ "Rent free",
      merged_data[[rented_var]] == 5 ~ "Some other arrangement"
    ),
    merged_data[[type_var]] == 3 ~ "Some other arrangement",
    merged_data[[type_var]] == -92 ~ "Refusal",
    merged_data[[type_var]] == -91 ~ "Item not applicable",
    merged_data[[type_var]] == -1 ~ "Don't know",
    merged_data[[type_var]] < -90 & merged_data[[type_var]] >= -999 ~ "Script error",
    TRUE ~ "Not asked"
  )

  levels <- c("Owned outright", "Being bought on a mortgage/bank loan", "Shared ownership (owns & rents property)",
              "Rented from a Council or New Town", "Rented from a Housing Association", "Rented privately",
              "Rent free", "Some other arrangement", "Item not applicable", "Script error",
              "Not asked", "Don't know", "Refusal")
  merged_data[[paste0("hownteen", age + 13)]] <- factor(merged_data[[paste0("hownteen", age + 13)]], levels = levels)
}

# Create collapsed variables for ages 14-20
for (age in 14:20) {
  var_name <- paste0("hownteen", age)
  if (!var_name %in% names(merged_data)) {
    cat("Skipping collapsed variable for age", age, "as detailed variable doesn't exist\n")
    next
  }

  cat("Creating collapsed variable for age", age, "\n")
  merged_data[[paste0("hown", age)]] <- case_when(
    merged_data[[var_name]] == "Owned outright" ~ "Owned outright",
    merged_data[[var_name]] == "Being bought on a mortgage/bank loan" ~ "Owned, buying with help of mortgage/loan",
    merged_data[[var_name]] == "Shared ownership (owns & rents property)" ~ "Part rent, part mortgage",
    merged_data[[var_name]] %in% c("Rented from a Council or New Town", "Rented from a Housing Association", "Rented privately") ~ "Rent it",
    merged_data[[var_name]] == "Rent free" ~ "live rent-free",
    merged_data[[var_name]] == "Some other arrangement" ~ "Other",
    TRUE ~ merged_data[[var_name]]
  )

  levels <- c("Owned outright", "Owned, buying with help of mortgage/loan", "Part rent, part mortgage",
              "Rent it", "live rent-free", "Other", "Item not applicable", "Script error",
              "Not asked", "Don't know", "Refusal")
  merged_data[[paste0("hown", age)]] <- factor(merged_data[[paste0("hown", age)]], levels = levels)
}

# Create collapsed variables for age 25
if ("W8TENURE" %in% names(merged_data)) {
  cat("Creating collapsed variable for age 25\n")
  merged_data <- merged_data %>%
    mutate(hown25 = case_when(
      W8TENURE == 1 ~ "Owned outright",
      W8TENURE == 2 ~ "Owned, buying with help of mortgage/loan",
      W8TENURE == 3 ~ "Part rent, part mortgage",
      W8TENURE == 4 ~ "Rent it",
      W8TENURE == 5 ~ "live rent-free",
      W8TENURE == 6 ~ "Other",
      W8TENURE == -9 ~ "Refusal",
      W8TENURE == -8 ~ "Don't know",
      W8TENURE == -1 ~ "Item not applicable",
      W8TENURE == -2 ~ "Script error",
      W8TENURE == -3 ~ "Not asked",
      TRUE ~ "Not asked"
    ))

  levels <- c("Owned outright", "Owned, buying with help of mortgage/loan", "Part rent, part mortgage",
              "Rent it", "live rent-free", "Other", "Item not applicable", "Script error",
              "Not asked", "Don't know", "Refusal")
  merged_data$hown25 <- factor(merged_data$hown25, levels = levels)
}

# Create collapsed variables for age 32
if ("W9DTENURE" %in% names(merged_data)) {
  cat("Creating collapsed variable for age 32\n")
  merged_data <- merged_data %>%
    mutate(hown32 = case_when(
      W9DTENURE == 1 ~ "Owned outright",
      W9DTENURE == 2 ~ "Owned, buying with help of mortgage/loan",
      W9DTENURE == 3 ~ "Part rent, part mortgage",
      W9DTENURE == 4 ~ "Rent it",
      W9DTENURE == 5 ~ "live rent-free",
      W9DTENURE == 6 ~ "Other",
      W9DTENURE == -8 ~ "Don't know",
      W9DTENURE == -9 ~ "Refusal",
      TRUE ~ "Not asked"
    ))

  levels <- c("Owned outright", "Owned, buying with help of mortgage/loan", "Part rent, part mortgage",
              "Rent it", "live rent-free", "Other", "Item not applicable", "Script error",
              "Not asked", "Don't know", "Refusal")
  merged_data$hown32 <- factor(merged_data$hown32, levels = levels)
}

# Select required variables
required_vars <- c("NSID")
for (age in c(14:20, 25, 32)) {
  if (paste0("hownteen", age) %in% names(merged_data)) {
    required_vars <- c(required_vars, paste0("hownteen", age))
  }
  if (paste0("hown", age) %in% names(merged_data)) {
    required_vars <- c(required_vars, paste0("hown", age))
  }
}

# Ensure we only select existing variables
cleaned_data <- merged_data %>% select(required_vars[required_vars %in% names(merged_data)])

# Check what variables we have
cat("Final dataset contains", ncol(cleaned_data), "variables:\n")
cat(names(cleaned_data), "\n")

# Write output
output_path <- "data/output/cleaned_data.csv"
write.csv(cleaned_data, output_path, row.names = FALSE)
cat("Successfully wrote output to", output_path, "\n")
cat("Dataset has", nrow(cleaned_data), "rows and", ncol(cleaned_data), "columns\n")
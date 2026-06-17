library(dplyr)
library(readr)

# Load all files
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all by NSID
merged <- full_join(w1, w2, by = "NSID")
merged <- full_join(merged, w3, by = "NSID")
merged <- full_join(merged, w4, by = "NSID")
merged <- full_join(merged, w5, by = "NSID")
merged <- full_join(merged, w6, by = "NSID")
merged <- full_join(merged, w7, by = "NSID")
merged <- full_join(merged, w8, by = "NSID")
merged <- full_join(merged, w9, by = "NSID")

# Function to map 8-category to 6-category (for sweeps 1-7)
# 8-cat: 1=Own outright, 2=Own buying, 3=Shared ownership, 4=Council, 5=Housing Assoc, 6=Private, 7=Rent free, 8=Some other
# 6-cat: 1=Own outright, 2=Own buying, 3=Shared ownership, 4=Rent it, 5=Rent free, 6=Other
map_8_to_6 <- function(x) {
  result <- rep(-3L, length(x))
  for (i in seq_along(x)) {
    v <- x[i]
    if (is.na(v)) {
      result[i] <- -3L
    } else if (v == 1) {
      result[i] <- 1L
    } else if (v == 2) {
      result[i] <- 2L
    } else if (v == 3) {
      result[i] <- 3L
    } else if (v %in% c(4, 5, 6)) {
      result[i] <- 4L
    } else if (v == 7) {
      result[i] <- 5L
    } else if (v == 8) {
      result[i] <- 6L
    } else if (v == -9) {
      result[i] <- -9L
    } else if (v == -8) {
      result[i] <- -8L
    } else if (v == -7) {
      result[i] <- -7L
    } else if (v == -2) {
      result[i] <- -2L
    } else if (v == -1) {
      result[i] <- -1L
    }
  }
  result
}

# Function to recode 8-category source variable to detailed hownteenXX (sweeps 1-4)
recode_8cat <- function(x) {
  result <- rep(-3, length(x))
  for (i in seq_along(x)) {
    v <- x[i]
    if (is.na(v)) {
      result[i] <- -3
    } else if (v == -999) {
      result[i] <- -3
    } else if (v == -997) {
      result[i] <- -2
    } else if (v == -995) {
      result[i] <- -2
    } else if (v == -94) {
      result[i] <- -2
    } else if (v == -92) {
      result[i] <- -9
    } else if (v == -91) {
      result[i] <- -1
    } else if (v == -1) {
      result[i] <- -8
    } else if (v >= 1 && v <= 8) {
      result[i] <- as.integer(v)
    } else {
      result[i] <- -3
    }
  }
  result
}

# Sweep 1 (Age 14): W1hous12HH -> hownteen14, hown14
merged$hownteen14 <- recode_8cat(merged$W1hous12HH)
merged$hown14 <- map_8_to_6(merged$hownteen14)

# Sweep 2 (Age 15): W2Hous12HH -> hownteen15, hown15
merged$hownteen15 <- recode_8cat(merged$W2Hous12HH)
merged$hown15 <- map_8_to_6(merged$hownteen15)

# Sweep 3 (Age 16): W3hous12HH -> hownteen16, hown16
merged$hownteen16 <- recode_8cat(merged$W3hous12HH)
merged$hown16 <- map_8_to_6(merged$hownteen16)

# Sweep 4 (Age 17): W4Hous12HH -> hownteen17, hown17
merged$hownteen17 <- recode_8cat(merged$W4Hous12HH)
merged$hown17 <- map_8_to_6(merged$hownteen17)

# Function to derive tenure from three source variables (type, owned-subtype, rented-subtype)
derive_three_vars <- function(type_var, owned_var, rented_var) {
  n <- length(type_var)
  result <- rep(-3, n)
  
  for (i in seq_len(n)) {
    t <- type_var[i]
    o <- owned_var[i]
    r <- rented_var[i]
    
    # Map type missing codes
    t_std <- -3
    if (!is.na(t)) {
      if (t == -999) t_std <- -3
      else if (t == -92) t_std <- -9
      else if (t == -91) t_std <- -1
      else if (t == -1) t_std <- -8
      else if (t == 6) t_std <- -3
      else if (t %in% c(1, 2, 3)) t_std <- as.integer(t)
    }
    
    # Map owned subtype missing codes
    o_std <- -3
    if (!is.na(o)) {
      if (o == -999) o_std <- -3
      else if (o == -92) o_std <- -9
      else if (o == -91) o_std <- -1
      else if (o == -1) o_std <- -8
      else if (o %in% c(1, 2, 3, 4)) o_std <- as.integer(o)
      else o_std <- -3
    }
    
    # Map rented subtype missing codes
    r_std <- -3
    if (!is.na(r)) {
      if (r == -999) r_std <- -3
      else if (r == -92) r_std <- -9
      else if (r == -91) r_std <- -1
      else if (r == -1) r_std <- -8
      else if (r %in% c(1, 2, 3, 4, 5)) r_std <- as.integer(r)
      else r_std <- -3
    }
    
    if (t_std == 1) {
      if (o_std >= 1 && o_std <= 8) {
        result[i] <- o_std
      } else {
        result[i] <- o_std
      }
    } else if (t_std == 2) {
      if (r_std >= 1 && r_std <= 8) {
        result[i] <- r_std
      } else {
        result[i] <- r_std
      }
    } else if (t_std == 3) {
      result[i] <- 8
    } else {
      if (o_std != -3 && !is.na(o_std)) {
        result[i] <- o_std
      } else if (r_std != -3 && !is.na(r_std)) {
        result[i] <- r_std
      }
    }
  }
  result
}

# Sweep 5 (Age 18)
merged$hownteen18 <- derive_three_vars(merged$W5Hous12HH, merged$W5Hous12BHH, merged$W5Hous12CHH)
merged$hown18 <- map_8_to_6(merged$hownteen18)

# Sweep 6 (Age 19)
merged$hownteen19 <- derive_three_vars(merged$W6Hous12YP, merged$W6Hous12bYP, merged$W6Hous12cYP)
merged$hown19 <- map_8_to_6(merged$hownteen19)

# Sweep 7 (Age 20)
merged$hownteen20 <- derive_three_vars(merged$W7Hous12YP, merged$W7Hous12bYP, merged$W7Hous12cYP)
merged$hown20 <- map_8_to_6(merged$hownteen20)

# Sweep 8 (Age 25): single source variable
map_s8_detailed <- function(x) {
  result <- rep(-3, length(x))
  for (i in seq_along(x)) {
    v <- x[i]
    if (is.na(v)) {
      result[i] <- -3
    } else if (v == -9) {
      result[i] <- -9
    } else if (v == -8) {
      result[i] <- -8
    } else if (v == -1) {
      result[i] <- -1
    } else if (v >= 1 && v <= 7) {
      result[i] <- as.integer(v)
    }
  }
  result
}

# Sweep 9 (Age 32): single source variable
map_s9_detailed <- function(x) {
  result <- rep(-3, length(x))
  for (i in seq_along(x)) {
    v <- x[i]
    if (is.na(v)) {
      result[i] <- -3
    } else if (v == -8) {
      result[i] <- -8
    } else if (v >= 1 && v <= 7) {
      result[i] <- as.integer(v)
    }
  }
  result
}

# Function to map sweep 8-9 source to collapsed 6-category
# Squatting (6) -> 6 (Other), Other (7) -> 6 (Other)
map_s89_6cat <- function(x) {
  result <- rep(-3L, length(x))
  for (i in seq_along(x)) {
    v <- x[i]
    if (is.na(v)) {
      result[i] <- -3L
    } else if (v == 1) {
      result[i] <- 1L
    } else if (v == 2) {
      result[i] <- 2L
    } else if (v == 3) {
      result[i] <- 3L
    } else if (v == 4) {
      result[i] <- 4L
    } else if (v == 5) {
      result[i] <- 5L
    } else if (v == 6) {
      result[i] <- 6L
    } else if (v == 7) {
      result[i] <- 6L
    } else if (v == -9) {
      result[i] <- -9L
    } else if (v == -8) {
      result[i] <- -8L
    } else if (v == -1) {
      result[i] <- -1L
    }
  }
  result
}

# Create collapsed variables for sweeps 8 and 9
merged$hown25 <- map_s89_6cat(merged$W8TENURE)
merged$hown32 <- map_s89_6cat(merged$W9DTENURE)

# Select only NSID and the required derived variables
# Detailed (8-category): hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20
# Collapsed (6-category): hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32
output <- merged %>%
  select(NSID, 
         hownteen14, hown14, 
         hownteen15, hown15, 
         hownteen16, hown16, 
         hownteen17, hown17, 
         hownteen18, hown18, 
         hownteen19, hown19, 
         hownteen20, hown20, 
         hown25, 
         hown32)

# Create output directory if needed
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Write output
write_csv(output, "data/output/cleaned_data.csv")

cat("Done! Output written to data/output/cleaned_data.csv")
cat("\nNumber of rows:", nrow(output), "\n")
cat("Variables:", paste(names(output), collapse=", "), "\n")

# Print summary of derived variables
for (var in names(output)[-1]) {
  cat(sprintf("%s: unique values = %s, NAs = %d\n", var, 
              paste(sort(unique(output[[var]])), collapse=", "), 
              sum(is.na(output[[var]]))))
}

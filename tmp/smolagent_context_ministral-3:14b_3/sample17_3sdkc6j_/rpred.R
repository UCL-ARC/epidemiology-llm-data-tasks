
# Load only base R and readr for file reading
library(readr)

# Step 1: Load datasets
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Step 2: Extract columns by position
wave_two_dep <- wave_two[, c(1, 1030)]
names(wave_two_dep)[2] <- "imd15"

wave_three_dep <- wave_three[, c(1, 161)]
names(wave_three_dep)[2] <- "imd16"

ns9_dep <- ns9_derived[, c(1, 3)]
names(ns9_dep)[2] <- "imd32"

# Step 3: Merge datasets using NSID
result <- merge(wave_two_dep, wave_three_dep, by = "NSID", all = TRUE)
result <- merge(result, ns9_dep, by = "NSID", all = TRUE)

# Step 4: Handle missing values using base R
result$imd15[result$imd15 == -94] <- -8
result$imd15[is.na(result$imd15)] <- -3

result$imd16[result$imd16 == -94] <- -8
result$imd16[is.na(result$imd16)] <- -3

result$imd32[is.na(result$imd32)] <- -3

# Step 5: Save to CSV
write.csv(result, "data/output/cleaned_data.csv", row.names = FALSE)

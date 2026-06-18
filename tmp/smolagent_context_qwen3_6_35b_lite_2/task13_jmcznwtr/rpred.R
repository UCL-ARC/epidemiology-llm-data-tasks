library(dplyr)
library(readr)
library(tidyr)
library(haven)
library(labelled)

# Load all wave files
file_wave1 <- "data/input/wave_one_lsype_family_background_2020.tab"
file_wave2 <- "data/input/wave_two_lsype_family_background_2020.tab"
file_wave3 <- "data/input/wave_three_lsype_family_background_2020.tab"
file_wave4 <- "data/input/wave_four_lsype_family_background_2020.tab"
file_wave5 <- "data/input/wave_five_lsype_family_background_2020.tab"

df1 <- read_delim(file_wave1, delim = "\t", show_col_types = FALSE)
df2 <- read_delim(file_wave2, delim = "\t", show_col_types = FALSE)
df3 <- read_delim(file_wave3, delim = "\t", show_col_types = FALSE)
df4 <- read_delim(file_wave4, delim = "\t", show_col_types = FALSE)
df5 <- read_delim(file_wave5, delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID
df <- full_join(df1, df2, by = "NSID")
df <- full_join(df, df3, by = "NSID")
df <- full_join(df, df4, by = "NSID")
df <- full_join(df, df5, by = "NSID")

print(paste("Total rows after merge:", nrow(df)))

# Function to recode NS-SEC to major categories
recode_nssec_major <- function(x) {
  result <- x
  
  # Collapse subcategories to major categories
  # Professional occupations (3.1-3.4, 4.1-4.4) -> 3
  result[result %in% c(3.1, 3.2, 3.3, 3.4, 4.1, 4.2, 4.3, 4.4)] <- 3
  # Intermediate occupations (7.1-7.4) -> 7
  result[result %in% c(7.1, 7.2, 7.3, 7.4)] <- 7
  # Employers small orgs (8.1, 8.2) -> 9
  result[result %in% c(8.1, 8.2)] <- 9
  # Own account workers (9.1, 9.2) -> 9
  result[result %in% c(9.1, 9.2)] <- 9
  # Lower technical (11.1, 11.2) -> 11
  result[result %in% c(11.1, 11.2)] <- 11
  # Semi-routine occupations (12.1-12.7) -> 12
  result[result %in% c(12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7)] <- 12
  # Routine occupations (13.1-13.5) -> 13
  result[result %in% c(13.1, 13.2, 13.3, 13.4, 13.5)] <- 13
  
  # Handle "not working" categories - map to -1 (item not applicable)
  # 14.1: Never worked, 14.2: Long-term unemployed, 14.3: Not currently working
  result[result %in% c(14.1, 14.2, 14.3)] <- -1
  # 15.0: Full-time students
  result[result == 15.0] <- -1
  # 16.0: Not classified or inadequately stated
  result[result == 16.0] <- -1
  # 17.0: Not classifiable for other reasons
  result[result == 17.0] <- -1
  
  # Handle missing values based on label meaning
  # -999 = Missing - household data lost -> -3 (not asked)
  result[result == -999] <- -3
  # -99 = not interviewed -> -3 (not asked)
  result[result == -99] <- -3
  # -98 = not present -> -3 (not asked)
  result[result == -98] <- -3
  # -94 = Insufficient information -> -8 (don't know)
  result[result == -94] <- -8
  
  return(result)
}

# Apply for each wave
# Wave 1 (Age 14)
df$nssecma14 <- recode_nssec_major(df$W1nsseccatmum)
df$nssecpa14 <- recode_nssec_major(df$W1nsseccatdad)

# Wave 2 (Age 15)
df$nssecma15 <- recode_nssec_major(df$W2nsseccatmum)
df$nssecpa15 <- recode_nssec_major(df$W2nsseccatdad)

# Wave 3 (Age 16)
df$nssecma16 <- recode_nssec_major(df$W3cnsseccatmum)
df$nssecpa16 <- recode_nssec_major(df$W3cnsseccatdad)

# Wave 4 (Age 17)
df$nssecma17 <- recode_nssec_major(df$w4cnsseccatmum)
df$nssecpa17 <- recode_nssec_major(df$w4cnsseccatdad)

# Wave 5 (Age 18)
df$nssecma18 <- recode_nssec_major(df$w5Cnsseccatmum)
df$nssecpa18 <- recode_nssec_major(df$w5Cnsseccatdad)

# Select only NSID and the derived variables
output_df <- df %>% select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, 
                           nssecma16, nssecpa16, nssecma17, nssecpa17,
                           nssecma18, nssecpa18)

# Write output
write_csv(output_df, "data/output/cleaned_data.csv")

print("Output written to data/output/cleaned_data.csv")
print(paste("Number of rows:", nrow(output_df)))
print(paste("Number of columns:", ncol(output_df)))

# Summary statistics
for (var in c("nssecma14", "nssecpa14", "nssecma15", "nssecpa15", 
              "nssecma16", "nssecpa16", "nssecma17", "nssecpa17",
              "nssecma18", "nssecpa18")) {
  cat(paste0("\n", var, ":\n"))
  print(table(output_df[[var]], useNA = "ifany"))
}
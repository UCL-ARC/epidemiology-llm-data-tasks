library(dplyr)
library(readr)
library(tidyr)
library(labelled)

# Create output directory if it doesn't exist
if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Load all files
df1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
df2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
df3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
df4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
df5 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
df6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
df7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
df8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t', show_col_types = FALSE)
df9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', show_col_types = FALSE)

# Merge all datasets by NSID
full_df <- df1 %>%
  full_join(df2, by = 'NSID') %>%
  full_join(df3, by = 'NSID') %>%
  full_join(df4, by = 'NSID') %>%
  full_join(df5, by = 'NSID') %>%
  full_join(df6, by = 'NSID') %>%
  full_join(df7, by = 'NSID') %>%
  full_join(df8, by = 'NSID') %>%
  full_join(df9, by = 'NSID')

# Function to recode tenure variables - vectorized
code_tenure <- function(x, detailed = TRUE) {
  # Start with NA
  out <- rep(NA_real_, length(x))
  
  # Map missing values by label meaning
  # -999, -998, -997, -995 -> -2 (schedule not applicable / script error / information lost)
  out[x == -999 | x == -998 | x == -997 | x == -995] <- -2
  # -92 -> -9 (Refusal)
  out[x == -92] <- -9
  # -91 -> -1 (Not applicable)
  out[x == -91] <- -1
  # -1, -8 -> -8 (Don't know / insufficient information)
  out[x == -1 | x == -8] <- -8
  # -9 -> -9 (Refusal)
  out[x == -9] <- -9
  
  # Now handle valid values based on detailed flag
  if (detailed) {
    # Waves 1-4 detailed categories
    # 1 = Owned outright
    out[x == 1] <- 1
    # 2 = Being bought on mortgage/loan
    out[x == 2] <- 2
    # 3 = Shared ownership
    out[x == 3] <- 3
    # 4 = Rented from Council/New Town
    out[x == 4] <- 4
    # 5 = Rented from Housing Association
    out[x == 5] <- 5
    # 6 = Rented privately
    out[x == 6] <- 6
    # 7 = Rent free (keep as is for detailed)
    out[x == 7] <- 7
    # 8 = Some other arrangement
    out[x == 8] <- 8
    
    # Wave 8 categories
    # 1 = Own - outright -> 1
    out[x == 1] <- 1
    # 2 = Own - buying with mortgage -> 2
    out[x == 2] <- 2
    # 3 = Part rent/mortgage (shared/equity) -> 3
    out[x == 3] <- 3
    # 4 = Rent inc Housing Ben -> Rent it (keep as 4 for detailed)
    out[x == 4] <- 4
    # 5 = Rent-free incl rels/friends -> 7 (Rent free)
    out[x == 5] <- 7
    # 6 = Squatting -> -2 (not applicable / unusual)
    out[x == 6] <- -2
    # 7 = Other arrangement incl board to parents -> 8
    out[x == 7] <- 8
    
    # Waves 5-7 simplified categories mapped to detailed
    # 1 = Owned -> 1
    out[x == 1] <- 1
    # 2 = Rented -> map to first rent category (4)
    out[x == 2] <- 4
    # 3 = Something else -> 8
    out[x == 3] <- 8
    # 6 = Not to be asked -> -3
    out[x == 6] <- -3
  } else {
    # Collapsed categories: Rent it = 4, Other = 8
    # Waves 1-4 detailed categories collapsed
    # 1 = Owned outright -> 1
    out[x == 1] <- 1
    # 2 = Being bought on mortgage -> 2
    out[x == 2] <- 2
    # 3 = Shared ownership -> 3
    out[x == 3] <- 3
    # 4, 5, 6 = all rental types -> 4 (Rent it)
    out[x %in% c(4, 5, 6)] <- 4
    # 7 = Rent free -> 4 (Rent it) - merge into Rent it
    out[x == 7] <- 4
    # 8 = Some other arrangement -> 8
    out[x == 8] <- 8
    
    # Wave 8 categories collapsed
    # 1 = Own - outright -> 1
    out[x == 1] <- 1
    # 2 = Own - buying with mortgage -> 2
    out[x == 2] <- 2
    # 3 = Part rent/mortgage -> 3
    out[x == 3] <- 3
    # 4 = Rent -> 4
    out[x == 4] <- 4
    # 5 = Rent-free -> 4 (merge into Rent it)
    out[x == 5] <- 4
    # 6 = Squatting -> -2
    out[x == 6] <- -2
    # 7 = Other -> 8
    out[x == 7] <- 8
    
    # Waves 5-7 simplified categories collapsed
    # 1 = Owned -> 1
    out[x == 1] <- 1
    # 2 = Rented -> 4 (Rent it)
    out[x == 2] <- 4
    # 3 = Something else -> 8
    out[x == 3] <- 8
    # 6 = Not to be asked -> -3
    out[x == 6] <- -3
    
    # Wave 9 categories collapsed
    # 1 = Own outright -> 1
    out[x == 1] <- 1
    # 2 = Own with mortgage -> 2
    out[x == 2] <- 2
    # 3 = Part rent/mortgage -> 3
    out[x == 3] <- 3
    # 4 = Rent it -> 4
    out[x == 4] <- 4
    # 5 = Live rent-free -> 4 (merge into Rent it)
    out[x == 5] <- 4
    # 6 = Squatting -> -2
    out[x == 6] <- -2
    # 7 = Other -> 8
    out[x == 7] <- 8
  }
  
  return(out)
}

# Apply recoding for detailed tenure variables (hownteen) for ages 14-20
full_df$hownteen14 <- code_tenure(full_df$W1hous12HH, detailed = TRUE)
full_df$hownteen15 <- code_tenure(full_df$W2Hous12HH, detailed = TRUE)
full_df$hownteen16 <- code_tenure(full_df$W3hous12HH, detailed = TRUE)
full_df$hownteen17 <- code_tenure(full_df$W4Hous12HH, detailed = TRUE)
full_df$hownteen18 <- code_tenure(full_df$W5Hous12HH, detailed = TRUE)
full_df$hownteen19 <- code_tenure(full_df$W6Hous12YP, detailed = TRUE)
full_df$hownteen20 <- code_tenure(full_df$W7Hous12YP, detailed = TRUE)

# Apply recoding for collapsed tenure variables (hown) for ages 14-32
full_df$hown14 <- code_tenure(full_df$W1hous12HH, detailed = FALSE)
full_df$hown15 <- code_tenure(full_df$W2Hous12HH, detailed = FALSE)
full_df$hown16 <- code_tenure(full_df$W3hous12HH, detailed = FALSE)
full_df$hown17 <- code_tenure(full_df$W4Hous12HH, detailed = FALSE)
full_df$hown18 <- code_tenure(full_df$W5Hous12HH, detailed = FALSE)
full_df$hown19 <- code_tenure(full_df$W6Hous12YP, detailed = FALSE)
full_df$hown20 <- code_tenure(full_df$W7Hous12YP, detailed = FALSE)
full_df$hown25 <- code_tenure(full_df$W8TENURE, detailed = FALSE)
full_df$hown32 <- code_tenure(full_df$W9DTENURE, detailed = FALSE)

# Select only NSID and derived variables
out_df <- full_df %>%
  select(NSID, hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20,
         hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32)

# Write output
write_csv(out_df, 'data/output/cleaned_data.csv')

# Print summary
cat('Output shape:', dim(out_df), '\n')
cat('Variables:', names(out_df), '\n')

# Check for each variable
for (v in names(out_df)[-1]) {
  cat(v, ': min=', min(out_df[[v]], na.rm=TRUE), ', max=', max(out_df[[v]], na.rm=TRUE),
      ', n_missing=', sum(is.na(out_df[[v]])), '\n')
}

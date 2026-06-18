library(readr)
df2 <- read_tsv('data/input/wave_two_lsype_family_background_2020.tab', show_col_types = FALSE)

# Check unique values in W2GrsswkHH
unique_vals <- sort(unique(df2$W2GrsswkHH))
print(unique_vals)

# Check which values are not mapped by sweep12_map
# sweep12_map: -3, -1, -992, -999, -94, -92, -91, -99
unmapped <- unique_vals[!unique_vals %in% c(-3, -1, -992, -999, -94, -92, -91, -99)]
print(unmapped)

# Check for any NA values in W2GrsswkHH
print(sum(is.na(df2$W2GrsswkHH)))

# Check the values of W2GrsswkHH for rows that have NA in the output
# First, let me check what values remain after recoding
code_to_label <- function(x, map) {
  for (code in names(map)) {
    x[x == as.numeric(code)] <- map[code]
  }
  x
}

sweep12_map <- c(
  "-3" = -1,
  "-1" = -8,
  "-992" = -9,
  "-999" = -2,
  "-94" = -8,
  "-92" = -9,
  "-91" = -1,
  "-99" = -3
)

recoded <- code_to_label(df2$W2GrsswkHH, sweep12_map)
recoded[is.na(recoded)] <- -3

# Check for any remaining NA
print(sum(is.na(recoded)))

# Check what unique values are in recoded
print(sort(unique(recoded)))

# Check what the 9 NA values in the output correspond to
# The issue might be that some values after recoding don't match any factor level

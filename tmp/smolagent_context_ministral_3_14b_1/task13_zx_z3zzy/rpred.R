
# Load required libraries
library(readr)

# Load all files
wave_one <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave_two <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave_three <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave_four <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')
wave_five <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t')

# Process wave 1
wave_one$nssecma14 <- wave_one$W1nsseccatmum
wave_one$nssecma14[wave_one$nssecma14 == -98] <- -3
wave_one$nssecma14[wave_one$nssecma14 %in% c(-999, -99)] <- -3
wave_one$nssecma14[wave_one$nssecma14 == -94] <- -8
wave_one$nssecma14 <- ifelse(wave_one$nssecma14 %% 1 != 0, as.integer(wave_one$nssecma14), wave_one$nssecma14)

wave_one$nssecpa14 <- wave_one$W1nsseccatdad
wave_one$nssecpa14[wave_one$nssecpa14 == -98] <- -3
wave_one$nssecpa14[wave_one$nssecpa14 %in% c(-999, -99)] <- -3
wave_one$nssecpa14[wave_one$nssecpa14 == -94] <- -8
wave_one$nssecpa14 <- ifelse(wave_one$nssecpa14 %% 1 != 0, as.integer(wave_one$nssecpa14), wave_one$nssecpa14)

# Process wave 2
wave_two$nssecma15 <- wave_two$W2nsseccatmum
wave_two$nssecma15[wave_two$nssecma15 == -98] <- -3
wave_two$nssecma15[wave_two$nssecma15 %in% c(-999, -99)] <- -3
wave_two$nssecma15[wave_two$nssecma15 == -94] <- -8
wave_two$nssecma15 <- ifelse(wave_two$nssecma15 %% 1 != 0, as.integer(wave_two$nssecma15), wave_two$nssecma15)

wave_two$nssecpa15 <- wave_two$W2nsseccatdad
wave_two$nssecpa15[wave_two$nssecpa15 == -98] <- -3
wave_two$nssecpa15[wave_two$nssecpa15 %in% c(-999, -99)] <- -3
wave_two$nssecpa15[wave_two$nssecpa15 == -94] <- -8
wave_two$nssecpa15 <- ifelse(wave_two$nssecpa15 %% 1 != 0, as.integer(wave_two$nssecpa15), wave_two$nssecpa15)

# Process wave 3
wave_three$nssecma16 <- wave_three$W3cnsseccatmum
wave_three$nssecma16[wave_three$nssecma16 == -98] <- -3
wave_three$nssecma16[wave_three$nssecma16 %in% c(-999, -99)] <- -3
wave_three$nssecma16[wave_three$nssecma16 == -94] <- -8
wave_three$nssecma16 <- ifelse(wave_three$nssecma16 %% 1 != 0, as.integer(wave_three$nssecma16), wave_three$nssecma16)

wave_three$nssecpa16 <- wave_three$W3cnsseccatdad
wave_three$nssecpa16[wave_three$nssecpa16 == -98] <- -3
wave_three$nssecpa16[wave_three$nssecpa16 %in% c(-999, -99)] <- -3
wave_three$nssecpa16[wave_three$nssecpa16 == -94] <- -8
wave_three$nssecpa16 <- ifelse(wave_three$nssecpa16 %% 1 != 0, as.integer(wave_three$nssecpa16), wave_three$nssecpa16)

# Process wave 4
wave_four$nssecma17 <- wave_four$w4cnsseccatmum
wave_four$nssecma17[wave_four$nssecma17 == -98] <- -3
wave_four$nssecma17[wave_four$nssecma17 %in% c(-999, -99)] <- -3
wave_four$nssecma17[wave_four$nssecma17 == -94] <- -8
wave_four$nssecma17 <- ifelse(wave_four$nssecma17 %% 1 != 0, as.integer(wave_four$nssecma17), wave_four$nssecma17)

wave_four$nssecpa17 <- wave_four$w4cnsseccatdad
wave_four$nssecpa17[wave_four$nssecpa17 == -98] <- -3
wave_four$nssecpa17[wave_four$nssecpa17 %in% c(-999, -99)] <- -3
wave_four$nssecpa17[wave_four$nssecpa17 == -94] <- -8
wave_four$nssecpa17 <- ifelse(wave_four$nssecpa17 %% 1 != 0, as.integer(wave_four$nssecpa17), wave_four$nssecpa17)

# Process wave 5
wave_five$nssecma18 <- wave_five$w5Cnsseccatmum
wave_five$nssecma18[wave_five$nssecma18 == -98] <- -3
wave_five$nssecma18[wave_five$nssecma18 %in% c(-999, -99)] <- -3
wave_five$nssecma18[wave_five$nssecma18 == -94] <- -8
wave_five$nssecma18 <- ifelse(wave_five$nssecma18 %% 1 != 0, as.integer(wave_five$nssecma18), wave_five$nssecma18)

wave_five$nssecpa18 <- wave_five$w5Cnsseccatdad
wave_five$nssecpa18[wave_five$nssecpa18 == -98] <- -3
wave_five$nssecpa18[wave_five$nssecpa18 %in% c(-999, -99)] <- -3
wave_five$nssecpa18[wave_five$nssecpa18 == -94] <- -8
wave_five$nssecpa18 <- ifelse(wave_five$nssecpa18 %% 1 != 0, as.integer(wave_five$nssecpa18), wave_five$nssecpa18)

# Merge all waves using base R merge
merged_data <- merge(wave_one, wave_two, by = "NSID", all = TRUE)
merged_data <- merge(merged_data, wave_three, by = "NSID", all = TRUE)
merged_data <- merge(merged_data, wave_four, by = "NSID", all = TRUE)
merged_data <- merge(merged_data, wave_five, by = "NSID", all = TRUE)

# Create output data with only required columns
cleaned_data <- merged_data[, c("NSID", "nssecma14", "nssecpa14", "nssecma15", "nssecpa15",
                               "nssecma16", "nssecpa16", "nssecma17", "nssecpa17",
                               "nssecma18", "nssecpa18")]

# Write output
write_csv(cleaned_data, 'data/output/cleaned_data.csv')

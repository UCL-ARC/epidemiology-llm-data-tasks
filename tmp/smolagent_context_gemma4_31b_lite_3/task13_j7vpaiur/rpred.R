library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

collapse_nssec <- function(x) {
  final_val <- rep(NA, length(x))
  
  # Substantive
  final_val[x >= 1.0 & x <= 2.0] <- 1
  final_val[x > 2.0 & x < 5.0] <- 2
  final_val[x >= 5.0 & x <= 6.0] <- 3
  final_val[x > 6.0 & x <= 10.0] <- 4
  final_val[x > 10.0 & x < 13.0] <- 5
  final_val[x >= 13.0 & x < 14.0] <- 6
  final_val[x >= 14.0 & x < 15.0] <- 7
  final_val[x == 15.0] <- 8
  final_val[x >= 16.0 & x <= 17.0] <- 9
  
  # Missing values based on metadata labels
  final_val[is.na(final_val) & x == -999.0] <- -2
  final_val[is.na(final_val) & x == -99.0] <- -3
  final_val[is.na(final_val) & x == -98.0] <- -1
  final_val[is.na(final_val) & x == -94.0] <- -8
  
  # Final fallback
  final_val[is.na(final_val)] <- -3
  
  return(final_val)
}

files <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab',
  'wave_five_lsype_family_background_2020.tab'
)

data_list <- lapply(files, function(f) {
  readr::read_delim(paste0('data/input/', f), delim = "\t", col_types = readr::cols(NSID = readr::col_character(), .default = "numeric"))
})

# Merge datasets
full_df <- data_list[[1]]
for(i in 2:length(data_list)) {
  full_df <- full_join(full_df, data_list[[i]], by = "NSID")
}

# Derive variables
full_df <- full_df %>%
  mutate(
    nssecma14 = collapse_nssec(W1nsseccatmum),
    nssecpa14 = collapse_nssec(W1nsseccatdad),
    nssecma15 = collapse_nssec(W2nsseccatmum),
    nssecpa15 = collapse_nssec(W2nsseccatdad),
    nssecma16 = collapse_nssec(W3cnsseccatmum),
    nssecpa16 = collapse_nssec(W3cnsseccatdad),
    nssecma17 = collapse_nssec(w4cnsseccatmum),
    nssecpa17 = collapse_nssec(w4cnsseccatdad),
    nssecma18 = collapse_nssec(w5Cnsseccatmum),
    nssecpa18 = collapse_nssec(w5Cnsseccatdad)
  )

final_vars <- c("NSID", "nssecma14", "nssecpa14", "nssecma15", "nssecpa15", 
                "nssecma16", "nssecpa16", "nssecma17", "nssecpa17", "nssecma18", "nssecpa18")

output_df <- full_df %>% select(all_of(final_vars))

readr::write_csv(output_df, 'data/output/cleaned_data.csv')

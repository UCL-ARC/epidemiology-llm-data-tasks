library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

harmonize_nssec <- function(x) {
  x[!is.na(x) & x == -999] <- -2
  x[!is.na(x) & x == -94] <- -8
  x[!is.na(x) & x %in% c(-99, -98)] <- -3
  positive_vals <- !is.na(x) & x > 0 & x <= 17
  x[positive_vals] <- floor(x[positive_vals])
  valid_missing <- c(-1, -2, -3, -7, -8, -9)
  invalid_positive <- !is.na(x) & x > 17 & !(x %in% valid_missing)
  x[invalid_positive] <- NA
  return(x)
}

create_labelled_factor <- function(x) {
  x_labelled <- labelled(x)
  lbls <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, -1, -2, -3, -7, -8, -9)
  names(lbls) <- c("Employers in large organisations", "Higher managerial occupations", "Higher professional occupations", "Lower professional occupations", "Lower managerial occupations", "Higher supervisory occupations", "Intermediate occupations", "Employers in small organisations", "Own account workers", "Lower supervisory occupations", "Lower technical craft occupations", "Semi-routine occupations", "Routine occupations", "Never worked/Long-term unemployed", "Full-time students", "Not classified or inadequately stated", "Not classifiable for other reasons", "Not applicable", "Script error/information lost", "Not asked/not interviewed", "Prefer not to say", "Don't know", "Refusal")
  val_labels(x_labelled) <- lbls
  return(x_labelled)
}

wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")

wave1_clean <- wave1 %>% select(NSID, nssecma14 = W1nsseccatmum, nssecpa14 = W1nsseccatdad)
wave2_clean <- wave2 %>% select(NSID, nssecma15 = W2nsseccatmum, nssecpa15 = W2nsseccatdad)
wave3_clean <- wave3 %>% select(NSID, nssecma16 = W3cnsseccatmum, nssecpa16 = W3cnsseccatdad)
wave4_clean <- wave4 %>% select(NSID, nssecma17 = w4cnsseccatmum, nssecpa17 = w4cnsseccatdad)
wave5_clean <- wave5 %>% select(NSID, nssecma18 = w5Cnsseccatmum, nssecpa18 = w5Cnsseccatdad)

merged_data <- wave1_clean %>%
  full_join(wave2_clean, by = "NSID") %>%
  full_join(wave3_clean, by = "NSID") %>%
  full_join(wave4_clean, by = "NSID") %>%
  full_join(wave5_clean, by = "NSID")

nssec_vars <- c("nssecma14", "nssecpa14", "nssecma15", "nssecpa15",
                "nssecma16", "nssecpa16", "nssecma17", "nssecpa17",
                "nssecma18", "nssecpa18")

for (var in nssec_vars) {
  merged_data[[var]] <- harmonize_nssec(merged_data[[var]])
  merged_data[[var]] <- create_labelled_factor(merged_data[[var]])
}

final_data <- merged_data %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15,
         nssecma16, nssecpa16, nssecma17, nssecpa17,
         nssecma18, nssecpa18)

write_csv(final_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete.\n")
cat("Number of rows:", nrow(final_data), "\n")
cat("Number of columns:", ncol(final_data), "\n")
cat("Output written to: data/output/cleaned_data.csv\n")
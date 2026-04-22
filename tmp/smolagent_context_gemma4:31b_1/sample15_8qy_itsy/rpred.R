library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = readr::cols())
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols())

# Merge using full_join by NSID
df <- full_join(wave8, wave9, by = "NSID")

# 2 & 3. Variable Selection and Renaming
NSID_col <- df$NSID
inc25_raw <- if ("W8DINCB" %in% names(df)) df$W8DINCB else rep(NA, nrow(df))
inc32_raw <- if ("W9DINCB" %in% names(df)) df$W9DINCB else rep(NA, nrow(df))

# 4 & 5. Missing Value Code Harmonization
inc25_num <- as.numeric(inc25_raw)
inc32_num <- as.numeric(inc32_raw)
inc25_num[is.na(inc25_num)] <- -3
inc32_num[is.na(inc32_num)] <- -3

# 7. Factor Variables and Labels
income_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-3" = "Not asked/interviewed",
  "-2" = "Script error/lost",
  "-1" = "Item not applicable",
  "1" = "less than 25",
  "2" = "25 to 50",
  "3" = "50 to 90",
  "4" = "90 to 140",
  "5" = "140 to 240",
  "6" = "240 to 300",
  "7" = "300 to 350",
  "8" = "350 to 400",
  "9" = "400 to 500",
  "10" = "500 to 600",
  "11" = "600 to 700",
  "12" = "700 to 800",
  "13" = "800 to 900",
  "14" = "900 to 1200",
  "15" = "1200 to 1400",
  "16" = "more than 1400"
)

# Manual factor creation to avoid labelled package casting issues
create_income_factor <- function(vec, labels_map) {
  levels_vals <- as.numeric(names(labels_map))
  labels_text <- as.character(labels_map)
  vec_num <- as.numeric(vec)
  res <- rep(NA, length(vec_num))
  for(i in seq_along(levels_vals)) {
    res[vec_num == levels_vals[i]] <- labels_text[i]
  }
  return(factor(res, levels = labels_text))
}

inc25_fact <- create_income_factor(inc25_num, income_labels)
inc32_fact <- create_income_factor(inc32_num, income_labels)

# Assemble final dataframe
df_cleaned <- data.frame(NSID = NSID_col, inc25 = inc25_fact, inc32 = inc32_fact)

# 10. Output Requirements
write_csv(df_cleaned, "data/output/cleaned_data.csv")
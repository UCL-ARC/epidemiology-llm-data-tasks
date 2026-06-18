library(readr)
library(dplyr)

input_dir <- "data/input/"
output_path <- "data/output/cleaned_data.csv"

# Missing value mapping functions
map_mother <- function(x) {
  res <- case_when(
    x %in% c(-999, -98) ~ -2,
    x == -99 ~ -3,
    x == -94 ~ -8,
    TRUE ~ x
  )
  res[is.na(res)] <- -3
  res
}

map_father <- function(x) {
  res <- case_when(
    x %in% c(-999, -98, -996) ~ -2,
    x == -99 ~ -3,
    x == -94 ~ -8,
    x == -92 ~ -9,
    TRUE ~ x
  )
  res[is.na(res)] <- -3
  res
}

# Wave configurations
waves <- list(
  list(file="wave_one_lsype_family_background_2020.tab", age=14, mum="W1empsmum", dad="W1empsdad"),
  list(file="wave_two_lsype_family_background_2020.tab", age=15, mum="W2empsmum", dad="W2empsdad"),
  list(file="wave_three_lsype_family_background_2020.tab", age=16, mum="W3empsmum", dad="W3empsdad"),
  list(file="wave_four_lsype_family_background_2020.tab", age=17, mum="w4empsmum", dad="w4empsdad")
)

merged <- NULL
for (w in waves) {
  df <- read_delim(file.path(input_dir, w$file), delim="\t", show_col_types=FALSE)
  df <- df %>%
    mutate(
      !!paste0("ecoactma", w$age) := map_mother(.data[[w$mum]]),
      !!paste0("ecoactpa", w$age) := map_father(.data[[w$dad]])
    ) %>%
    select(NSID, starts_with("ecoactma"), starts_with("ecoactpa"))
  merged <- if (is.null(merged)) df else full_join(merged, df, by="NSID")
}

write_csv(merged, output_path)

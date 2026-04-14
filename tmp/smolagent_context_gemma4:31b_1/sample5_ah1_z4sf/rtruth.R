options(repos = c(CRAN = "https://cloud.r-project.org/"))
list_of_packages <- c('dplyr', 'purrr', 'readr')
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
})

# Data path
data_path <- 'data/input/'

# Required datasets
S1yp <- read_delim(file.path(data_path, "wave_one_lsype_young_person_2020.tab"), show_col_types = FALSE)
S4yp <- read_delim(file.path(data_path, "wave_four_lsype_young_person_2020.tab"), show_col_types = FALSE)
S6yp <- read_delim(file.path(data_path, "wave_six_lsype_young_person_2020.tab"), show_col_types = FALSE)
S8dv <- read_delim(file.path(data_path, "ns8_2015_derived.tab"), show_col_types = FALSE)
S9dv <- read_delim(file.path(data_path, "ns9_2022_derived_variables.tab"), show_col_types = FALSE)

partnr_vars <- list(
  S1 = S1yp %>% select(NSID),
  S4 = S4yp %>% select(NSID),
  S6 = S6yp %>% select(NSID, partnr19_orig = W6MarStatYP),
  S8 = S8dv %>% select(NSID, partnradu25_orig = W8DMARSTAT),
  S9 = S9dv %>% select(NSID, partnradu32_orig = W9DMARSTAT)
)

# Merge all sweeps by ID
partnr_all <- reduce(partnr_vars, full_join, by = "NSID")

## Complete version ----------------------------------------------------------------
# Recode S8 (partnradu25_orig): numeric codes from .tab → harmonised labels
# Original codes: -9=Refused, -8=Insufficient info, -1=Not applicable,
#   1=Single, 2=Married, 3=Separated married, 4=Divorced, 5=Widowed,
#   6=Civil Partner, 7=Separated CP, 8=Former CP, 9=Surviving CP
partnr_all <- partnr_all |>
  mutate(
    partnradu25 = case_when(
      is.na(partnradu25_orig)  ~ "Not asked at the fieldwork stage/participated/interviewed",
      partnradu25_orig == -9   ~ "Refusal",
      partnradu25_orig == -8   ~ "Don't know / insufficient information",
      partnradu25_orig == -1   ~ "Item not applicable",
      partnradu25_orig ==  1   ~ "Single, never married / in CP",
      partnradu25_orig ==  2   ~ "Married",
      partnradu25_orig ==  3   ~ "Separated, still legally married",
      partnradu25_orig ==  4   ~ "Divorced",
      partnradu25_orig ==  5   ~ "Widowed",
      partnradu25_orig ==  6   ~ "Civil partner",
      partnradu25_orig ==  7   ~ "Separated, still legally in CP",
      partnradu25_orig ==  8   ~ "Former civil partner",
      partnradu25_orig ==  9   ~ "Surviving civil partner",
      TRUE                     ~ "Not asked at the fieldwork stage/participated/interviewed"
    ),
    # Recode S9 (partnradu32_orig): numeric codes from .tab → harmonised labels
    # Original codes: -9=Refused, -8=Insufficient info,
    #   1=Single, 2=Married, 3=Divorced, 4=Legally separated,
    #   5=Widowed, 6=Civil Partner, 7=Former CP, 8=Surviving CP
    partnradu32 = case_when(
      is.na(partnradu32_orig)  ~ "Not asked at the fieldwork stage/participated/interviewed",
      partnradu32_orig == -9   ~ "Refusal",
      partnradu32_orig == -8   ~ "Don't know / insufficient information",
      partnradu32_orig ==  1   ~ "Single, never married / in CP",
      partnradu32_orig ==  2   ~ "Married",
      partnradu32_orig ==  3   ~ "Divorced",
      partnradu32_orig ==  4   ~ "Separated, still legally married",
      partnradu32_orig ==  5   ~ "Widowed",
      partnradu32_orig ==  6   ~ "Civil partner",
      partnradu32_orig ==  7   ~ "Former civil partner",
      partnradu32_orig ==  8   ~ "Surviving civil partner",
      TRUE                     ~ "Not asked at the fieldwork stage/participated/interviewed"
    )
  )

# Cross-tab checks
partnr_all |> count(partnradu25_orig, partnradu25)
partnr_all |> count(partnradu32_orig, partnradu32)

## Simple version ------------------------------------------------------------------
# Recode S6 (partnr19_orig): numeric codes from .tab → harmonised simple labels
# Original codes: -997=Script error, -97=Declined self completion,
#   -92=Refused, -91=Not applicable, -1=Don't know,
#   1=Single, 2=Married, 3=Separated, 4=Divorced, 5=Widowed
partnr_all <- partnr_all |>
  mutate(
    partnr19 = case_when(
      is.na(partnr19_orig)     ~ "Not asked at the fieldwork stage/participated/interviewed",
      partnr19_orig == -997    ~ "Script error / information lost",
      partnr19_orig == -97     ~ "Prefer not to say / declined",
      partnr19_orig == -92     ~ "Refusal",
      partnr19_orig == -91     ~ "Item not applicable",
      partnr19_orig == -1      ~ "Don't know / insufficient information",
      partnr19_orig ==  1      ~ "Single, never married / in CP",
      partnr19_orig ==  2      ~ "Married / civil partner",
      partnr19_orig ==  3      ~ "Separated, still legally married / in CP",
      partnr19_orig ==  4      ~ "Divorced / former civil partner",
      partnr19_orig ==  5      ~ "Widowed / surviving civil partner",
      TRUE                     ~ "Not asked at the fieldwork stage/participated/interviewed"
    ),
    # Collapse complete partnradu25 → simple partnr25
    partnr25 = case_when(
      partnradu25 %in% c("Refusal")                                                          ~ "Refusal",
      partnradu25 %in% c("Don't know / insufficient information")                             ~ "Don't know / insufficient information",
      partnradu25 %in% c("Item not applicable")                                               ~ "Item not applicable",
      partnradu25 %in% c("Not asked at the fieldwork stage/participated/interviewed")         ~ "Not asked at the fieldwork stage/participated/interviewed",
      partnradu25 %in% c("Single, never married / in CP")                                     ~ "Single, never married / in CP",
      partnradu25 %in% c("Married", "Civil partner")                                          ~ "Married / civil partner",
      partnradu25 %in% c("Separated, still legally married", "Separated, still legally in CP") ~ "Separated, still legally married / in CP",
      partnradu25 %in% c("Divorced", "Former civil partner")                                  ~ "Divorced / former civil partner",
      partnradu25 %in% c("Widowed", "Surviving civil partner")                                ~ "Widowed / surviving civil partner",
      TRUE                                                                                    ~ "Not asked at the fieldwork stage/participated/interviewed"
    ),
    # Collapse complete partnradu32 → simple partnr32
    partnr32 = case_when(
      partnradu32 %in% c("Refusal")                                                          ~ "Refusal",
      partnradu32 %in% c("Don't know / insufficient information")                             ~ "Don't know / insufficient information",
      partnradu32 %in% c("Not asked at the fieldwork stage/participated/interviewed")         ~ "Not asked at the fieldwork stage/participated/interviewed",
      partnradu32 %in% c("Single, never married / in CP")                                     ~ "Single, never married / in CP",
      partnradu32 %in% c("Married", "Civil partner")                                          ~ "Married / civil partner",
      partnradu32 %in% c("Separated, still legally married")                                  ~ "Separated, still legally married / in CP",
      partnradu32 %in% c("Divorced", "Former civil partner")                                  ~ "Divorced / former civil partner",
      partnradu32 %in% c("Widowed", "Surviving civil partner")                                ~ "Widowed / surviving civil partner",
      TRUE                                                                                    ~ "Not asked at the fieldwork stage/participated/interviewed"
    )
  )

# Checks
partnr_all |> count(partnr19_orig, partnr19)
partnr_all |> count(partnradu25, partnr25)
partnr_all |> count(partnradu32, partnr32)

partnr_all <- partnr_all %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output", "output.csv")
write.csv(partnr_all, output_data_path, row.names = FALSE)
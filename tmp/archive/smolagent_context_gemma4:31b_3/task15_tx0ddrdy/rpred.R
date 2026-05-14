library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
w8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = readr::cols(.default = "c"))
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols(.default = "c"))

w8$NSID <- as.character(w8$NSID)
w9$NSID <- as.character(w9$NSID)

# 3. Variable Selection and Renaming
w8_clean <- w8 %>% 
  select(NSID, W8DINCB) %>% 
  rename(inc25 = W8DINCB)

w9_clean <- w9 %>% 
  select(NSID, W9DINCB) %>% 
  rename(inc32 = W9DINCB)

merged_df <- full_join(w8_clean, w9_clean, by = "NSID")

# 4 & 5. Missing Value Code Harmonization
process_income <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  return(x)
}

merged_df <- merged_df %>% 
  mutate(
    inc25 = process_income(inc25),
    inc32 = process_income(inc32)
  )

# 7. Factor Variables and Labels
# The error occurred because the names of the label vector were characters
# but the data was double. We must ensure the labels vector has numeric names
# and use a named vector where names are the values and values are the labels.

income_vals <- c(-9, -8, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
income_labs <- c(
  "Refusal",
  "Don't know/insufficient information",
  "Not asked/interviewed",
  "Script error/lost",
  "Item not applicable",
  "less than 25",
  "25 to 50",
  "50 to 90",
  "90 to 140",
  "140 to 240",
  "240 to 300",
  "300 to 350",
  "350 to 400",
  "400 to 500",
  "500 to 600",
  "600 to 700",
  "700 to 800",
  "800 to 900",
  "900 to 1200",
  "1200 to 1400",
  "more than 1400"
)

# set_value_labels expects a named vector where names are the labels and values are the codes
# Or a named vector where names are the codes and values are the labels.
# Based on the error, it wants the labels to match the type of x (double).
# Actually, set_value_labels(x, labels) where labels is a named vector: 
# the names are the labels, the values are the codes.

labels_vec <- income_labs
names(labels_vec) <- income_vals

# In labelled::set_value_labels, the vector should be: values = codes, names = labels
# Let's construct it correctly: 
# names(v) = label, v = code

final_labels <- setNames(income_vals, income_labs)

merged_df$inc25 <- set_value_labels(merged_df$inc25, final_labels)
merged_df$inc32 <- set_value_labels(merged_df$inc32, final_labels)

# 10. Output Requirements
write_csv(merged_df, "data/output/cleaned_data.csv")
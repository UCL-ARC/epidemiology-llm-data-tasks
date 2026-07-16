# Human-curated reference implementation for this task.
# Replace every placeholder before initialising or running the task.
#
# The initialiser copies each filename named in metadata.json to data/input/ and
# runs this script from the task directory. The final data frame must contain
# NSID and must be written to data/output/output.csv.

stop("Replace the task-template rtruth.R placeholders before running this task.")

# Example outline after replacing the placeholders:
# library(readr)
# library(dplyr)
#
# source_data <- read_delim(
#   "data/input/<raw-input-file>.tab",
#   delim = "\t",
#   show_col_types = FALSE
# )
# output <- source_data %>% transmute(NSID, <derived_variable> = <derivation>)
# dir.create("data/output", recursive = TRUE, showWarnings = FALSE)
# write_csv(output, "data/output/output.csv")

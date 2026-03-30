library(readr)
library(haven)

# Verify the output file
cat('Reading cleaned_data.csv...\n')

# Read the file
output <- read_csv('data/output/cleaned_data.csv', show_col_types = FALSE)

cat('File loaded successfully\n')
cat('Dimensions:', nrow(output), 'rows x', ncol(output), 'columns\n\n')

cat('Column names:\n')
print(names(output))
cat('\n')

cat('First few rows:\n')
print(head(output))
cat('\n')

cat('Summary of sori variable:\n')
print(table(output$sori, useNA='ifany'))
cat('\n')

cat('Summary of consolidated sori_lbl:\n')
print(table(output$sori_lbl, useNA='ifany'))
cat('\n')

cat('Verify labelled attribute:\n')
print(output$sori_lbl)

# Check if it's a labelled factor
if(is.factor(output$sori_lbl) || is.factor(output$sori)) {
  cat('\nConfirmed: sori_lbl is a factor\n')
}

if(haven::is_labelled(output$sori_lbl)) {
  cat('Confirmed: sori_lbl has labels\n')
  cat('Labels:', haven::labels(output$sori_lbl), '\n')
}

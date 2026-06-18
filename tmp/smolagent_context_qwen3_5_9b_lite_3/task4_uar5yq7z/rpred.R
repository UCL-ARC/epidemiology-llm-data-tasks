library(readr)

# Check the output file
cat('Output file:', 'data/output/cleaned_data.csv', '\n')
if(file.exists('data/output/cleaned_data.csv')) {
  df <- read_csv('data/output/cleaned_data.csv', show_col_types = FALSE)
  cat('Output file dimensions:', dim(df), '\n')
  cat('Output columns:', '\n')
  print(names(df))
  
  # Check if the required variables exist
  required_vars <- c('NSID', 'sori19', 'sori20', 'sori25', 'sori32')
  missing_vars <- setdiff(required_vars, names(df))
  if(length(missing_vars) > 0) {
    cat('Missing variables:', missing_vars, '\n')
  } else {
    cat('All required variables present!\n')
  }
  
  # Show summary of sori variables
  for(var in c('sori19', 'sori20', 'sori25', 'sori32')) {
    cat('\n', var, ':', '\n')
    print(table(df[[var]], useNA = 'ifany'))
  }
} else {
  cat('Output file not found!\n')
}
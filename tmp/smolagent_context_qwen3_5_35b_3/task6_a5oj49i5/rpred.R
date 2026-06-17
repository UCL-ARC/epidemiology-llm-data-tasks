library(readr)

# Verify output file
df_output <- read_csv('data/output/cleaned_data.csv')
cat('File successfully created with', nrow(df_output), 'rows and', ncol(df_output), 'columns\n')
cat('Variables:', paste(names(df_output), collapse=', '), '\n')
cat('\nSample of output:\n')
print(df_output[1:10,])
library(readr)

# Read the output file
output <- read_csv('data/output/cleaned_data.csv')
cat('Final output file summary:\n')
cat('Dimensions:', nrow(output), 'x', ncol(output), '\n')
cat('Columns:', paste(names(output), collapse = ', '), '\n\n')

cat('First 5 rows:\n')
print(head(output, 5))
cat('\nLast 5 rows:\n')
print(tail(output, 5))

cat('\nValue distributions:\n')
cat('educdtlma (20 categories): min=', min(output$educdtlma, na.rm=TRUE), ', max=', max(output$educdtlma, na.rm=TRUE), '\n')
cat('educdtlpa (20 categories): min=', min(output$educdtlpa, na.rm=TRUE), ', max=', max(output$educdtlpa, na.rm=TRUE), '\n')
cat('educma (5 NVQ levels): min=', min(output$educma, na.rm=TRUE), ', max=', max(output$educma, na.rm=TRUE), '\n')
cat('educpa (5 NVQ levels): min=', min(output$educpa, na.rm=TRUE), ', max=', max(output$educpa, na.rm=TRUE), '\n')

cat('\nMissing code distribution in educma:\n')
print(table(output$educma, useNA = 'ifany'))
cat('\nMissing code distribution in educpa:\n')
print(table(output$educpa, useNA = 'ifany'))
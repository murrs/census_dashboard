# Census analysis dashboard

## Requirements

R studio:
https://www.rstudio.com/products/rstudio/download/#download

R:
https://cran.rstudio.com/

## Running the code
Open server.R in RStudio.
Change the path to the data file you have locally.
Click on "Run App".

## Correlation table normalization
Radio buttons will allow you to choose among different normalization possibilities for the correlation table:
- by population: the sum of all numbers in the table is 1.0, i.e. each number represent the fraction of population in that specific category for both variables selected.
- by row: the sum of all numbers in *each* row is 1.0, i.e. reading the table row by row each number gives you the fraction of population in the category represented in each column.
- by column: the sum of all numbers in *each* column is 1.0, i.e. reading the table column by column each number gives you the fraction of population in the category represented in each row.

The last two normalizations are useful to estimate the conditional probability of one category given the other.

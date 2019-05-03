
# Comparing 3 or More Correlation Coefficients in R
This repository contains the code for an R function that will report p-values for pairwise correlation coefficient comparisons and that will report separation lettering for correlation coefficients. This code is based on the work of Levy (1977).

This function takes 7 arguments (the first 3 are required):

Correlation_Coefficients: a numeric vector containing the correlation coefficients to be analyzed

Numbers_of_Observations: a numeric or an integer vector containing the numbers of observations that went in to each of the corresponding correlation coefficients

Identifiers: a character or a factor vector containing names to identify each corresponding correlation coefficient

Data_Frame: an optional data frame to include such that only column names need to be supplied for the first three arguments instead of using the 'Data_Frame$Column_Name' syntax

Alpha = 0.05: a value of alpha against which significance can be tested (the default is 0.05)

Control_for_Experimentwise_Error = TRUE: an argument specifying whether or not this function should give conservative estimates (i.e., holding the experimentwise error rate at the given value of alpha) or liberal estimates (i.e., using the given value of alpha for each pairwise comparison); the default, TRUE, holds the experimentwise error rate at alpha and calculates the comparisonwise error rate based on the number of pairwise comparisons

The_Strength_of_the_Correlation_is_More_Important_Than_the_Sign_of_the_Correlation = FALSE: an argument specifying whether or not the absolute values of the correlation coefficients are to be compared; the default, FALSE, will calculate significant differences without taking the absolute values of the correlation coefficients (in other words, the default is to consider the sign of the correlation coefficient as more important than the strength of the correlation)

Though this function only uses base R functions, it was heavily inspired by the 'agricolae' package, particularly the 'orderPvalue' and 'lastC' functions. Thank you Felipe de Mendiburu!

Works Cited

de Mendiburu, F. 2017. agricolae: Statistical Procedures for Agricultural Research. R package version 1.2-8. <https://CRAN.R-project.org/package=agricolae>.

Levy, K.J. 1977. Pairwise comparisons involving unequal sample sizes associated with correlations, proportions or variances. Br. J. Math. Stat. Psychol. 30:137-139.

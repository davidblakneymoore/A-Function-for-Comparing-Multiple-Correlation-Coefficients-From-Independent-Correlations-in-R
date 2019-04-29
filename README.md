
# Comparing 3 or More Correlation Coefficients in R
This repository contains the code for an R function that will report p-values for pairwise correlation coefficient comparisons and that will report separation lettering for correlation coefficients. This code is based on the work of Levy (1977).

This function requires a vector containing correlation coefficients, a vector containing the number of observations that went in to each corresponding correlation coefficient, and a vector containing names or identifiers of each correlation.

Though this function only uses base R functions, it was heavily inspired by the 'agricolae' package, particularly the 'orderPvalue' and 'lastC' functions. Thank you Felipe de Mendiburu!

Works Cited

de Mendiburu, F. 2017. agricolae: Statistical Procedures for Agricultural Research. R package version 1.2-8. <https://CRAN.R-project.org/package=agricolae>.

Levy, K.J. 1977. Pairwise comparisons involving unequal sample sizes associated with correlations, proportions or variances. Br. J. Math. Stat. Psychol. 30:137-139.


# Comparing 3 or More Correlation Coefficients in R
This repository contains the code for an R function that will report p-values for pairwise correlation coefficient comparisons and that will report separation lettering for correlation coefficients. This code is based on the work of Levy (1977).

This function requires a vector containing correlation coefficients, a vector containing the number of observations that went in to each corresponding correlation coefficient, and a vector containing names or identifiers of each correlation (character strings).

This function draws heavily on the 'agricolae' package, particularly the 'orderPvalue' and 'lastC' functions. Thank you Felipe de Mendiburu!

Works Cited

Levy, K.J. 1977. Pairwise comparisons involving unequal sample sizes associated with correlations, proportions or variances. Br. J. Math. Stat. Psychol. 30:137-139.

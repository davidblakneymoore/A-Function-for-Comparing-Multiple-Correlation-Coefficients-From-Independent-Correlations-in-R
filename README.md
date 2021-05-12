
# A Function for Comparing 3 or More Correlation Coefficients in R

This repository contains the code for an R function that will report p-values for pairwise correlation coefficient comparisons and that will report separation lettering for correlation coefficients. This code is based on the work of Levy (1977).

This function takes 6 arguments (the first 3 are required):

`Correlation_Coefficients`: a numeric vector containing the correlation coefficients to be analyzed

`Numbers_of_Observations`: a numeric or an integer vector containing the numbers of observations that went in to each of the corresponding correlation coefficients

`Identifiers`: a character or a factor vector containing names to identify each corresponding correlation coefficient

`Data_Frame`: an optional data frame to include such that column names can be supplied for the first three arguments (the data frame that these columns are from should be provided for this `Data_Frame` argument)

`Alpha = 0.05`: a value of alpha against which significance can be tested (the default is 0.05)

`Control_for_Experimentwise_Error = TRUE`: an argument specifying whether or not this function should give conservative estimates (i.e., holding the experimentwise error rate at the given value of alpha) or liberal estimates (i.e., using the given value of alpha for each pairwise comparison); the default, TRUE, holds the experimentwise error rate at alpha and calculates the comparisonwise error rate based on the number of pairwise comparisons

Though this function only uses base R functions, it was heavily inspired by the 'agricolae' package, particularly the `orderPvalue()` and `lastC()` functions. Thank you Felipe de Mendiburu!

I'm also indebted to my former advisor, Karl Guillard, for introducing me to this technique and for generally sparking my interest in statistics.

This function has been cited in a peer-reviewed journal article (Findor et al., 2021).

<b>Works Cited</b>

de Mendiburu, F. 2017. agricolae: Statistical Procedures for Agricultural Research. R package version 1.2-8. <https://CRAN.R-project.org/package=agricolae>.

Findor, A., M. Hruska, P. Jankovská, and M. Pobudová. 2021. Re-examining public opinion preferences for migrant categorizations: “Refugees” are evaluated more negatively than “migrants” and “foreigners” related to participants’ direct, extended, and mass-mediated intergroup contact experiences. Int. J. Intercult. Relat. 80:262-273.

Levy, K.J. 1977. Pairwise comparisons involving unequal sample sizes associated with correlations, proportions or variances. Br. J. Math. Stat. Psychol. 30:137-139.

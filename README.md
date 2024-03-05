# A Function for Comparing Multiple Correlation Coefficients From Independent Correlations in R

This repository contains the code for comparing 3 or more correlation coefficients based on a well established and published method (Levy, 1977). Importantly, it assumes the correlations are independent. When independence cannot be assumed, please consider using a different procedure.

This function takes 6 arguments, and the first 3 are required.

`Correlation_Coefficients` is a numeric vector containing the correlation coefficients to be analyzed.

`Numbers_of_Observations`is a numeric or an integer vector containing the numbers of observations that went in to each of the corresponding correlation coefficients.

`Identifiers` is a character or a factor vector containing names to identify each corresponding correlation coefficient.

`Data_Frame` is an optional data frame to include such that column names can be supplied for the first three arguments (the data frame that these columns are from should be provided for this `Data_Frame` argument).

`Alpha = 0.05` is a value of alpha against which significance can be tested (the default is `0.05`).

`Control_for_Experimentwise_Error = TRUE` is an argument specifying whether or not this function should give conservative estimates (by holding the experimentwise error rate at the given value of alpha) or liberal estimates (by using the given value of alpha for each pairwise comparison). The default, `TRUE`, holds the experimentwise error rate at alpha and calculates the comparisonwise error rate based on the number of pairwise comparisons. Since this procedure assumes independence of correlations, the Šidák correction, which is used when there is independence, is used (Šidák, 1967).

Though this function only uses `base` functions, it was heavily inspired by the `agricolae` package, particularly the `orderPvalue()` and `lastC()` functions. Thank you Felipe de Mendiburu!

I'm also indebted to my former advisor, Karl Guillard, for introducing me to this technique and for generally sparking my interest in statistics.

This function has been cited in peer-reviewed journal articles and a book chapter (Beghin, 2023; Chue and Yeo, 2022; Findor et al., 2021; Matko and Sedlmeier, 2023).

<b>Works Cited</b>

Beghin, G. 2023. Does the Lay Concept of Mental Disorder Necessitate a Dysfunction? Advances in Experimental Philosophy of Medicine, edited by Kristien Hens and Andreas De Block. Bloomsbury Publishing. Pp. 71-96.

Chue, K.L., and A. Yeo. 2022. Exploring associations of positive relationships and adolescent well-being across cultures. Youth Soc. 00:1-12.

de Mendiburu, F. 2017. agricolae: Statistical Procedures for Agricultural Research. R package version 1.2-8. <https://CRAN.R-project.org/package=agricolae>.

Findor, A., M. Hruska, P. Jankovská, and M. Pobudová. 2021. Re-examining public opinion preferences for migrant categorizations: “Refugees” are evaluated more negatively than “migrants” and “foreigners” related to participants’ direct, extended, and mass-mediated intergroup contact experiences. Int. J. Intercult. Relat. 80:262-273.

Levy, K.J. 1977. Pairwise comparisons involving unequal sample sizes associated with correlations, proportions or variances. Br. J. Math. Stat. Psychol. 30:137-139.

Matko, K., and P. Sedlmeier. 2023. Which meditation technique for whom? An experimental single-case study comparing concentrative, humming, observing-thoughts, and walking meditation.

Šidák, Z.K. 1967. Rectangular Confidence Regions for the Means of Multivariate Normal Distributions. J. Am. Stat. Assoc. 62: 626–633.

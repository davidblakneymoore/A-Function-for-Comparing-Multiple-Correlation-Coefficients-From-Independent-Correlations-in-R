
# Below is a function for comparing 3 or more correlation coefficients based on Levy (1977).
# This function returns a 'Pairwise_Comparison_Data_Frame' data frame containing p values for each pairwise comparison.
# This function also returns a 'Separation_Lettering_Data_Frame' data frame containing each correlation coefficient and separation lettering.
# 'Correlation_Coefficients' must be a numeric vector containing correlation coefficients.
# 'Numbers_of_Observations' must be either an integer vector or a numeric vector containing the sample sizes of each corresponding correlation coefficient.
# 'Identifiers' must be a vector containing names of the correlations.

Comparing_Correlation_Coefficients <- function (Correlation_Coefficients, Numbers_of_Observations, Identifiers, Data_Frame, Alpha = 0.05, Control_for_Experimentwise_Error = TRUE) {
  if (!missing(Data_Frame)) {
    Correlation_Coefficients = as.numeric(Data_Frame[[deparse(substitute(Correlation_Coefficients))]])
    Numbers_of_Observations = as.numeric(Data_Frame[[deparse(substitute(Numbers_of_Observations))]])
    Identifiers = as.character(Data_Frame[[deparse(substitute(Identifiers))]])
  } else {
    Correlation_Coefficients = as.numeric(Correlation_Coefficients)
    Numbers_of_Observations = as.numeric(Numbers_of_Observations)
    Identifiers = as.character(Identifiers)
  }
  Unsorted_Data_Frame <- data.frame(Identifiers = Identifiers, Correlation_Coefficients = Correlation_Coefficients, Numbers_of_Observations = Numbers_of_Observations)
  Sorted_Data_Frame <- Unsorted_Data_Frame[order(Unsorted_Data_Frame$Correlation_Coefficients, decreasing = TRUE), ]
  rownames(Sorted_Data_Frame) <- NULL
  Correlation_Coefficients <- Sorted_Data_Frame$Correlation_Coefficients
  Numbers_of_Observations <- Sorted_Data_Frame$Numbers_of_Observations
  Identifiers <- Sorted_Data_Frame$Identifiers
  Number_of_Pairwise_Comparisons <- choose(length(Correlation_Coefficients), 2)
  Function_for_Comparing_Correlation_Coefficients <- function (Correlation_Coefficient_1, Correlation_Coefficient_2, Number_of_Observations_1, Number_of_Observations_2) {
    (2 * (1 - pnorm(abs(((0.5 * log((1 + Correlation_Coefficient_1) / (1 - Correlation_Coefficient_1))) - (0.5 * log((1 + Correlation_Coefficient_2) / (1 - Correlation_Coefficient_2)))) / (((1 / (Number_of_Observations_1 - 3)) + (1 / (Number_of_Observations_2 - 3))) ^ 0.5)))))
  }
  if (Control_for_Experimentwise_Error == TRUE) {
    Corrected_Alpha <- 1 - ((1 - Alpha) ^ (1 / choose(length(Correlation_Coefficients), 2)))
  } else {
    Corrected_Alpha <- Alpha
  }
  Comparison <- c(NULL)
  Correlation_1 <- c(NULL)
  Correlation_2 <- c(NULL)
  Counter_1 <- 0
  for (i in Identifiers[1:(length(Identifiers) - 1)]) {
    for (j in Identifiers[((which(Identifiers == i)) + 1):length(Identifiers)]) {
      Counter_1 <- Counter_1 + 1
      Comparison[Counter_1] <- paste(i, "vs.", j, sep = " ")
      Correlation_1[Counter_1] <- i
      Correlation_2[Counter_1] <- j
    }
  }
  names(Comparison) <- NULL
  names(Correlation_1) <- NULL
  names(Correlation_2) <- NULL
  Counter_2 <- 0
  Counter_3 <- 0
  Counter_4 <- 0
  p_Value <- c(NULL)
  for (i in Correlation_Coefficients[1:(length(Correlation_Coefficients) - 1)]) {
    Counter_2 <- Counter_2 + 1
    Counter_3 <- which(Correlation_Coefficients == i)
    for (j in Correlation_Coefficients[((which(Correlation_Coefficients == i)) + 1):length(Correlation_Coefficients)]) {
      Counter_3 <- Counter_3 + 1
      Counter_4 <- Counter_4 + 1
      p_Value[Counter_4] <- Function_for_Comparing_Correlation_Coefficients(i, j, Numbers_of_Observations[Counter_2], Numbers_of_Observations[Counter_3])
    }
  }
  Significance <- rep("Unknown", length.out = length(p_Value))
  for (i in 1:length(p_Value)) {
    Significance[i] <- ifelse(p_Value[i] < Corrected_Alpha, "Significantly Different", "Not Significantly Different")
  }
  names(p_Value) <- NULL
  names(Significance) <- NULL
  Pairwise_Comparison_Data_Frame <- data.frame(Correlation_1 = Correlation_1, Correlation_2 = Correlation_2, Comparison = Comparison, p_Value = p_Value, Significance = Significance)
  Matrix_of_p_Values <- matrix(1, nrow = length(Correlation_Coefficients), ncol = length(Correlation_Coefficients))
  Matrix_of_p_Values[lower.tri(Matrix_of_p_Values, diag = FALSE)] <- Pairwise_Comparison_Data_Frame$p_Value
  Matrix_of_p_Values <- t(Matrix_of_p_Values)
  Matrix_of_p_Values[lower.tri(Matrix_of_p_Values, diag = FALSE)] <- Pairwise_Comparison_Data_Frame$p_Value
  rownames(Matrix_of_p_Values) <- colnames(Matrix_of_p_Values) <- Identifiers
  Separation_Letters_and_Symbols <- c(letters[1:26], LETTERS[1:26], 1:9, c(".", "+", "-", "*", "/", "#", "$", "%", "&", "^", "[", "]", ":", "@", ";"))
  Separation_Lettering <- rep("", length(Correlation_Coefficients))
  i <- 1
  j <- 1
  Counter_5 <- 0
  Counter_6 <- 1
  Counter_7 <- length(Correlation_Coefficients)
  Counter_8 <- 0
  Separation_Lettering[1] <- Separation_Letters_and_Symbols[Counter_6]
  Row_Number <- as.numeric(rownames(Sorted_Data_Frame))
  while (i < length(Correlation_Coefficients)) {
    Counter_5 <- Counter_5 + 1
    if (Counter_5 > length(Correlation_Coefficients)) {break}
    for (j in i:length(Correlation_Coefficients)) {
      Nonsignificance_of_p_Value <- Matrix_of_p_Values[Row_Number[j], Row_Number[i]] > Corrected_Alpha
      if (Nonsignificance_of_p_Value) {
        Last_Character_Function <- function (x) {
          Getting_Rid_of_Spaces_at_the_End_of_Character_Strings <- sub(" +$", "", x)
          Number_of_Characters_in_Character_Strings <- nchar(Getting_Rid_of_Spaces_at_the_End_of_Character_Strings)
          Last_Character <- substr(Getting_Rid_of_Spaces_at_the_End_of_Character_Strings, Number_of_Characters_in_Character_Strings, Number_of_Characters_in_Character_Strings)
          return (Last_Character)
        }
        if (Last_Character_Function(Separation_Lettering[j]) != Separation_Letters_and_Symbols[Counter_6]) {
          Separation_Lettering[j] <- paste(Separation_Lettering[j], Separation_Letters_and_Symbols[Counter_6], sep = "")
        }
      }
      else {
        Counter_6 <- Counter_6 + 1
        Counter_7 <- j
        Counter_8 <- 0
        Counter_9 <- i
        for (k in Counter_7:length(Correlation_Coefficients)) {
          Separation_Lettering[k] <- paste(Separation_Lettering[k], "", sep = "")
        }
        Separation_Lettering[Counter_7] <- paste(Separation_Lettering[Counter_7], Separation_Letters_and_Symbols[Counter_6], sep = "")
        for (l in Counter_9:Counter_7) {
          if (Matrix_of_p_Values[Row_Number[l], Row_Number[Counter_7]] <= Corrected_Alpha) {
            i <- i + 1
            Counter_8 <- 1
          }
          else {break}
        }
        {break}
      }
    }
    if (Counter_8 == 0) {
      i <- i + 1
    }
  }
  Separation_Lettering_Data_Frame <- data.frame(Identifier = Sorted_Data_Frame$Identifiers, Correlation_Coefficient = Sorted_Data_Frame$Correlation_Coefficients, Number_of_Observations = Sorted_Data_Frame$Numbers_of_Observations, Separation_Lettering = Separation_Lettering)
  Parameters <- data.frame(Number_of_Correlation_Coefficients_to_Compare = length(Correlation_Coefficients), Number_of_Pairwise_Comparisons = Number_of_Pairwise_Comparisons, Experimentwise_Error = Alpha, Comparisonwise_Error = Corrected_Alpha, Conservative_or_Liberal = ifelse(Control_for_Experimentwise_Error == TRUE, "Conservative", "Liberal"), Any_Significantly_Different_Correlation_Coefficients = ifelse(any(unlist(sapply(Pairwise_Comparison_Data_Frame$Significance, grepl, "Significantly Different")) == T), "Yes", "No"))
  return (Correlation_Coefficient_Separation_Test_Results <- list(Parameters = Parameters, Pairwise_Comparison_Data_Frame = Pairwise_Comparison_Data_Frame, Separation_Lettering_Data_Frame = Separation_Lettering_Data_Frame))
}


# Works Cited

# Levy, K.J. 1977. Pairwise comparisons involving unequal sample sizes associated with correlations, proportions or variances. Br. J. Math. Stat. Psychol. 30:137-139.


# Example:

Practice_Data_Frame <- data.frame(Name = sample(LETTERS[1:5], 5), Coefficient_of_Correlation = c(-0.909, 0.333, -0.004, 0.444, -0.504), Sample_Size = sample(20:30, 5))
Output_Data <- Comparing_Correlation_Coefficients(Coefficient_of_Correlation, Sample_Size, Name, Practice_Data_Frame)
Output_Data$Pairwise_Comparison_Data_Frame
Output_Data$Separation_Lettering_Data_Frame
Output_Data$Parameters

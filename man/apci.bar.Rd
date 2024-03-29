\name{apci.bar}
\alias{apci.bar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualization for the APC-I model results
}
\description{
Visualize the APC-I model results in a simple bar plot.
}
\usage{
apci.bar(model, age, period, outcome_var,
cohort_label = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
A list, inheriting the corresponding results generated by function apci.
}
  \item{age}{
A vector, representing the age group index taking on a small number of distinct values in the data. Usually, the vector should be converted to a factor (or the terms of "category" and "enumerated type").
}
  \item{period}{
An object of class character, similar to the argument of age, representing the time period index in the data.
}
  \item{outcome_var}{
An object of class character representing the name of the outcome variable used in APC-I model. The outcome variable itself can be numerical and categorical.
}
  \item{cohort_label}{
A vector, representing the labels of cohort groups in the x asix.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}

\examples{
library("APCI")
## load data
test_data <- APCI::women9017
test_data$acc <- as.factor(test_data$acc)
test_data$pcc <- as.factor(test_data$pcc)

## run APCI model
APC_I <- apci(outcome = "inlfc",
              age = "acc",
              period = "pcc",
              cohort = "ccc",
              weight = "wt",
              data = test_data,dev.test=FALSE,
              family = "gaussian")

## plot the bar plot
apci.bar(model = APC_I, age = "acc",period = "pcc")
}


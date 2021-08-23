\name{tests}
\alias{tests}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
local and global F test
}
\description{
implement local and global F test for APCI model
}
\usage{
tests(model, age = "acc", period = "pcc",
cohort = "ccc", A, P, C, data, weight = "wt",
family, outcome, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
A generalized linear regression model generated from the internal function temp_model
}
  \item{age}{
An object of class character representing the age group index taking on a small number of distinct values in the data. Usually, the vector should be converted to a factor (or the terms of "category" and "enumerated type").
}
  \item{period}{
An object of class character, similar to the argument of age, representing the time period index in the data.
}
  \item{cohort}{
An optional object of class character representing cohort membership index in the data. Usually, the cohort index can be generated from the age group index and time period index in the data because of the intrinsic relationship among these three time-related indices.
}
  \item{A, P, C}{
The numbers of age groups, period groups, and cohort groups separately.
}
  \item{data}{
A data frame containing the outcome variable, age group indicator, period group indicator, and covariates to be used in the model. If the variable(s) are not found in data, there will be an error message reminding the users to check the input data again.
}
  \item{weight}{
An optional vector of sample weights to be used in the model fitting process. If non-NULL, the weights will be used in the first step to estimate the model. Observations with negative weights will be automatically dropped in modeling.
}
  \item{family}{
Used to specify the statistical distribution of the error term and link function to be used in the model. Usually, it is a character string naming a family function. For example, family can be "binomial", "multinomial"", or "gaussian". Users could also check R package glm for more details of family functions.
}
  \item{outcome}{
An object of class character containing the name of the outcome variable. The outcome variable can be continuous, categorical, or count.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}

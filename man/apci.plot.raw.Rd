\name{apci.plot.raw}
\alias{apci.plot.raw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
data exploration: visualize age, period, and cohort patterns in the outcome before modeling. 
}
\description{
visualize age, period, and cohort patterns before modeling. 
}
\usage{
apci.plot.raw(data, outcome_var, age, period, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame containing the outcome variable, age group indicator, period group indicator, and covariates to be used in the model. If the variable(s) are not found in data, there will be an error message reminding the users to check the input data again.
}
  \item{outcome_var}{
An object of class character containing the name of the outcome variable. The outcome variable can be continuous, categorical, or count.
}
  \item{age}{
An object of class character representing the age group index taking on a small number of distinct values in the data. Usually, the vector should be converted to a factor (or the terms of "category" and "enumerated type").
}
  \item{period}{
An object of class character, similar to the argument of age, representing the time period index in the data.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}

\examples{
# plot the raw scores
apci.plot.raw(data = simulation, outcome_var = "y",
              age = "age", period = "period")
}

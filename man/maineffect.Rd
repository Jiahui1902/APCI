\name{maineffect}
\alias{maineffect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
estimate age effect and period effect
}
\description{
estimate age and period effect from APCI model
}
\usage{
maineffect(A, P, C, model = temp6, data, gee=FALSE,
...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A, P, C}{
The numbers of age groups, period groups, and cohort groups separately.
}
  \item{model}{
A generalized linear regression model generated from the internal function temp_model
}
  \item{data}{
A data frame containing the outcome variable, age group indicator, period group indicator, and covariates to be used in the model. If the variable(s) are not found in data, there will be an error message reminding the users to check the input data again.
}
\item{gee}{
logical, indicating if the data is cross-sectional data or longitudinal/panel data. If \code{TRUE}, the generalized estimating equation will be used to correct the standard error estimates. The default is \code{FALSE}, indicating that the data are cross-sectional.
  }
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}

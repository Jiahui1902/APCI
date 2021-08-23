\name{cohortdeviation}
\alias{cohortdeviation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate cohort deviation
}
\description{
calculate cohort deviation
}
\usage{
cohortdeviation(A,
  P,
  C,
  model = temp6,
  weight = "wt",
  covariate,
  gee=FALSE,
  unequal_interval = FALSE,
  age_range = NULL,
  period_range = NULL,
  age_interval = NULL,
  period_interval = NULL,
  age_group = NULL,
  period_group = NULL,
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
  \item{weight}{
An optional vector of sample weights to be used in the model fitting process. If non-NULL, the weights will be used in the first step to estimate the model. Observations with negative weights will be automatically dropped in modeling.
}
  \item{covariate}{
An optional vector of characters, representing the name(s) of the user-specified covariate(s) to be used in the model. If the variable(s) are not found in data, there will be an error message reminding the users to check the data again.
}
\item{gee}{
logical, indicating if the data is cross-sectional data or longitudinal/panel data. If \code{TRUE}, the generalized estimating equation will be used to correct the standard error estimates. The default is \code{FALSE}, indicating that the data are cross-sectional.
  }
\item{unequal_interval}{
Logical, indicating if age and period groups are of the same width. The default is set as \code{TRUE}.
  }
  \item{age_range, period_range}{
Numeric vector indicating the actual age and period range (e.g., 10 to 59 years old from 2000 to 2019).
  }
  \item{age_interval, period_interval, age_group, period_group}{
Numeric values or character vectors indicating how age and period are
grouped. \code{age_interval} and \code{period_interval} are numbers
indicating the width of age and period groups respectively. \code{age_group} and \code{period_group} are character vectors explicitly listing all potential age and period groups. Either \code{age_interval}(\code{period_interval}) or \code{age_group} (\code{period_group}) have to be defined when \code{unequal_interval} is \code{TRUE}.
  }
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}

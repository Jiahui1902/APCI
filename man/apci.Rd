\name{apci}
\alias{apci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Run apci model
}
\description{
 run APC-I model
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outcome}{
An object of class character containing the name of the outcome variable. The outcome variable can be continuous, categorical, or count.
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
  \item{weight}{
An optional vector of sample weights to be used in the model fitting process. If non-NULL, the weights will be used in the first step to estimate the model. Observations with negative weights will be automatically dropped in modeling.
}
  \item{covariate}{
An optional vector of characters, representing the name(s) of the user-specified covariate(s) to be used in the model. If the variable(s) are not found in data, there will be an error message reminding the users to check the data again.
}
  \item{data}{
A data frame containing the outcome variable, age group indicator, period group indicator, and covariates to be used in the model. If the variable(s) are not found in data, there will be an error message reminding the users to check the input data again.
}
  \item{family}{
Used to specify the statistical distribution of the error term and link function to be used in the model. Usually, it is a character string naming a family function. For example, family can be "binomial", "multinomial"", or "gaussian". Users could also check R package glm for more details of family functions.
}
  \item{dev.test}{
Logical, specifying if the global F test (step 1) should be implemented before running the APC-I model. If TRUE, apci will first run the global F test and report the test results; otherwise, apci will skip this step and return NULL. The default setting is TRUE. But users should be careful that the algorithm will not automatically stop even if there is no significant cohort average deviation.
}
  \item{print}{
Logical, specifying if the intermediate results should be displayed on the screen in running the model. The default setting is TRUE in order to show the results explicitly although it can be too clumpy when the intermediate results are shown on the screen.
}
  \item{gee}{
logical, indicating if the data is cross-sectional data or longitudinal/panel data. If \code{TRUE}, the generalized estimating equation will be used to correct the standard error estimates. The default is \code{FALSE}, indicating that the data are cross-sectional.
  }
  \item{id}{
A vector of character, specifying the cluster index in longitudinal data. It is required when \code{gee} is \code{TRUE}. The length of the vector should be the same as the number of observations.
  }
  \item{corstr}{
a character string, specifying a possible correlation structure in the error terms when \code{gee} is \code{TRUE}. The following are allowed: \code{independence}, \code{fixed}, \code{stat\_M\_dep}, \code{non\_stat\_M\_dep}, \code{exchangeable}, \code{AR-M} and \code{unstructured}. The default value is \code{exchangeable}.
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

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
 \item{model}{
 A summary of the fitted generalized linear regression. It displays the coefficients, standard errors, etc.
 }
 \item{dev_global}{
 The results of the global F test. It shows that if the interaction terms are significant as a component of the generalized linear regression model.
 }
\item{intercept}{
The overall intercept.
}
\item{age_effect}{
A vector, representing the estimated age effect for each age group.
}
\item{period_effect}{
A vector, representing the estimated period effect for each time period.
}
\item{cohort_average}{
A vector, representing the cohort average effects for comparing inter-cohort differences.
}
\item{cohort_slope}{
A vector, representing intra-cohort life-course changes.
}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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

## check model results
summary(APC_I)

APC_I$model
APC_I$dev_global
APC_I$dev_local
APC_I$intercept
APC_I$age_effect
APC_I$period_effect
APC_I$cohort_average
APC_I$cohort_slope
}

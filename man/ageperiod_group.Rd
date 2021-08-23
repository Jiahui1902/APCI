\name{ageperiod_group}
\alias{ageperiod_group}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the cohort index matrix for any age and period groups
}
\description{
This function returns the cohort index matrix for any age and period groups. The cohort index matrix will then be used to extract the
cohort effects.
}
\usage{
ageperiod_group(age_range, period_range,
age_interval, period_interval,
age_group = NULL, period_group = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{age_range, period_range}{
Numeric vector indicating the actual age and period range (e.g., 10 to 59 years old from 2000 to 2019).
  }
  \item{age_interval, period_interval, age_group, period_group}{
Numeric values or character vectors indicating how age and period are
grouped. \code{age_interval} and \code{period_interval} are numbers
indicating the width of age and period groups respectively. \code{age_group} and \code{period_group} are character vectors explicitly listing all potential age and period groups. Either \code{age_interval}(\code{period_interval}) or \code{age_group} (\code{period_group}) have to be defined when \code{unequal_interval} is \code{TRUE}.
}
}

\value{
It returns a matrix respresenting the relationship among age, period,
and cohort groups under the current setting.
}

\examples{
## age and period groups have equal width
ageperiod_group(age_range = 10:59, period_range = 2000:2019,
                age_interval = 5, period_interval = 5)
ageperiod_group(age_range = 10:59, period_range = 2000:2019,
                age_group = c("10-14","15-19","20-24","25-29",
                              "30-34","35-39","40-44","45-49",
                              "50-54","55-59"),
                period_group = c("2000-2004","2005-2009","2010-2014","2015-2019"))

## age and period groups have unequal width
ageperiod_group(age_range = 10:59, period_range = 2000:2019,
                age_interval = 10, period_interval = 5)
ageperiod_group(age_range = 10:59, period_range = 2000:2019,
                age_group = c("10-19","20-29","30-39","40-49","50-59"),
                period_group = c("2000-2004","2005-2009","2010-2014","2015-2019"))
}

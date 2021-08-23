# install the package and use this script to test the package
library("APCI")
test_data <- APCI::women9017
test_data$acc <- as.factor(test_data$acc)
test_data$pcc <- as.factor(test_data$pcc)
test_data$educc <- as.factor(test_data$educc)
test_data$educr <- as.factor(test_data$educr)

# equal age and period interval
APC_I <- APCI::apci(outcome = "inlfc",
                    age = "acc",
                    period = "pcc",
                    cohort = "ccc",
                    weight = "wt",
                    data = test_data,dev.test=FALSE,
                    print = T,
                    family = "gaussian")

# check results
summary(APC_I)

APC_I$model
summary(APC_I$model)
APC_I$dev_global
APC_I$dev_local
APC_I$intercept
APC_I$age_effect
APC_I$period_effect
APC_I$cohort_average
APC_I$cohort_slope
APC_I$cohort_index

apci.bar(model = APC_I, age = "acc",period = "pcc")

# unequal age and period interval
uneqal_interval1 <- APCI::apci(outcome = "inlfc",
                    age = "age",
                    period = "year",
                    cohort = "ccc",
                    weight = "wt",
                    data = test_data,dev.test=FALSE,
                    print = T,
                    family = "gaussian",
                    unequal_interval = TRUE,
                    age_range = 20:64,
                    period_range = 1990:2019,
                    age_interval = 5,
                    period_interval = 10)
uneqal_interval1$cohort_index

uneqal_interval2 <- APCI::apci(outcome = "inlfc",
                    age = "age",
                    period = "year",
                    cohort = "ccc",
                    weight = "wt",
                    data = test_data,dev.test=FALSE,
                    print = T,
                    family = "gaussian",
                    unequal_interval = TRUE,
                    age_range = 20:64,
                    period_range = 1990:2019,
                    age_interval = 10,
                    period_interval = 5)
uneqal_interval2$cohort_index

uneqal_interval3 <- APCI::apci(outcome = "inlfc",
                               age = "age",
                               period = "year",
                               cohort = "ccc",
                               weight = "wt",
                               data = test_data,dev.test=FALSE,
                               print = T,
                               family = "gaussian",
                               unequal_interval = T,
                               age_range = 20:69,
                               period_range = 1990:2019,
                               age_group = c("20-29","30-39",
                                             "40-49","50-59",
                                             "60-69"),
                               period_group = c("1990-1994","1995-1999",
                                                "2000-2004","2005-2009",
                                                "2010-2014","2015-2019"))
uneqal_interval3$cohort_index

uneqal_interval2$cohort_index
uneqal_interval3$cohort_index
uneqal_interval2$cohort_average$cohort_average
uneqal_interval3$cohort_average$cohort_average



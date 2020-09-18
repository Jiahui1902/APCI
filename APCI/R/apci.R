

# APCI Model
apci <- function(outcome = "inlfc",
                 age = "acc",
                 period = "pcc",
                 cohort = NULL,
                 weight = NULL,
                 covariate = NULL,
                 data,
                 family ="quasibinomial",
                 dev.test = TRUE,
                 print = TRUE,
                 ...){
  # change family name if the input is "binomial"
  if(family=="binomial"){
    family <- "quasibinomial"
  }

  # prepare data
  pre <- temp_model(outcome = outcome,
                    age = age,
                    period = period,
                    cohort = cohort,
                    weight = weight,
                    covariate = covariate,
                    data = data,
                    family = family)
  A <- pre$A
  P <- pre$P
  C <- pre$C
  temp6 <- pre$model
  age. <- age; period. <- period; cohort. <- cohort;outcome. <- outcome
  family. <- family; weight. <- weight
  # F test in Step 1 and Step 2
  if(dev.test==TRUE){
    Tests <- tests(model = temp6,A=A,P=P,C=C,data = data, weight=weight.,
                   age = age.,period = period.,cohort = cohort.,outcome = outcome.,family = family.)
  }else{
    Tests <- NULL
  }
  # main effect
  MainEffect <- maineffect(A=A,P=P,C=C,model = temp6)
  # cohort deviation
  CohortDeviation <- cohortdeviation(A=A,P=P,C=C,model = temp6)

if(print=="TRUE"){
  # Main Effect
  message("Intercept: \n")
  print(MainEffect$intercept)
  message("")

  message("Age Effect: \n")
  print(MainEffect$age_effect)
  message("")

  message("Period Effect: \n")
  print(MainEffect$period_effect)
  message("")

  # Cohort Deviation
  message("Cohort Deviation: \n")
  print(CohortDeviation$cohort_average)
  message("")

  message("Cohort Life Course Dynamics: \n")
  print(CohortDeviation$cohort_slope)
  message("")
}

  # output:
  list(model = pre$model,dev_global=Tests$dev_global,
       # dev_local=Tests$dev_local,
       intercept = MainEffect$intercept,
       age_effect=MainEffect$age_effect,
       period_effect=MainEffect$period_effect,
       cohort_average = CohortDeviation$cohort_average,cohort_slope=CohortDeviation$cohort_slope,
       int_matrix = CohortDeviation$int_matrix,
       data = data)
}


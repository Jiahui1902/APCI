

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
                 gee = FALSE,
                 id = NULL,
                 corstr = "exchangeable",
                 unequal_interval = FALSE,
                 age_range = NULL,
                 period_range = NULL,
                 age_interval = NULL,
                 period_interval = NULL,
                 age_group = NULL,
                 period_group = NULL,
                 ...){
  # change family name if the input is "binomial"
  if(family=="binomial"){
    family <- "quasibinomial"
  }
  data <- as.data.frame(data)

  # prepare data
  if(unequal_interval==TRUE){
  if(!is.null(age_group)){
    age_group_split <- stringr::str_extract_all(age_group, "[0-9]+",simplify = T)
    data$acc <- cut(data[,age],
                  breaks = as.numeric(c(age_group_split[1],age_group_split[,2])),
                  right = T, include.lowest = T,
                  labels = 1:length(age_group))
    data[,age] <- as.factor(data$acc)
  }else{
    data$acc <- floor((data[,age]-min(age_range))/age_interval) + 1
    data[,age] <- as.factor(data$acc)
  }

  if(!is.null(period_group)){
    period_group_split <- stringr::str_extract_all(period_group, "[0-9]+",simplify = T)
    data$pcc <- cut(data[,period],
                  breaks = as.numeric(c(period_group_split[1],period_group_split[,2])),
                  right = T, include.lowest = T,
                  labels = 1:length(period_group))
    data[,period] <- as.factor(data$pcc)
  }else{
    data$pcc <- floor((data[,period]-min(period_range))/period_interval) + 1
    data[,period] <- as.factor(data$pcc)
  }
  }

  pre <- temp_model(outcome = outcome,
                    age = age,
                    period = period,
                    cohort = cohort,
                    weight = weight,
                    covariate = covariate,
                    data = data,
                    family = family,
                    gee = gee,
                    id = id,
                    corstr = corstr)
  A <- pre$A
  P <- pre$P
  C <- pre$C
  temp6 <- pre$model
  age. <- age; period. <- period; cohort. <- cohort;outcome. <- outcome
  family. <- family; weight. <- weight;gee. <- gee
  # F test in Step 1 and Step 2
  if(dev.test==TRUE){
    Tests <- tests(model = temp6,A=A,P=P,C=C,data = data, weight=weight.,
                   age = age.,period = period.,cohort = cohort.,outcome = outcome.,family = family.)
  }else{
    Tests <- NULL
  }
  # main effect
  MainEffect <- maineffect(A=A,P=P,C=C,model = temp6,gee=gee.)
  # cohort deviation
  CohortDeviation <- cohortdeviation(A=A,P=P,C=C,model = temp6,gee=gee.,
                                     unequal_interval = unequal_interval,
                                     age_range = age_range,
                                     period_range = period_range,
                                     age_interval = age_interval,
                                     period_interval = period_interval,
                                     age_group = age_group,
                                     period_group = period_group)

if(print=="TRUE"){
  # Main Effect
  message("Intercept: \n")
  # print(MainEffect$intercept)
  print(data.frame(estimate = sprintf("%.3f",as.numeric(MainEffect$intercept[1])),
                   se = sprintf("%.3f",as.numeric(MainEffect$intercept[2])),
                   p = sprintf("%.3f",as.numeric(MainEffect$intercept[3])),
                   sig = sprintf("%.3s",MainEffect$intercept[4])
  ))
  message("")

  message("Age Effect: \n")
  # print(MainEffect$age_effect)
  print(data.frame(age_group = MainEffect$age_effect[,1],
                   age_estimate = sprintf("%.3f",as.numeric(MainEffect$age_effect[,2])),
                   age_se = sprintf("%.3f",as.numeric(MainEffect$age_effect[,3])),
                   age_p = sprintf("%.3f",as.numeric(MainEffect$age_effect[,4])),
                   age_sig = MainEffect$age_effect[,5]
  ))
  message("")

  message("Period Effect: \n")
  # print(MainEffect$period_effect)
  print(data.frame(period_group = MainEffect$period_effect[,1],
                   period_estimate = sprintf("%.3f",as.numeric(MainEffect$period_effect[,2])),
                   period_se = sprintf("%.3f",as.numeric(MainEffect$period_effect[,3])),
                   period_p = sprintf("%.3f",as.numeric(MainEffect$period_effect[,4])),
                   period_sig = MainEffect$period_effect[,5]
  ))
  message("")

  # Cohort Deviation
  message("Cohort Deviation: \n")
  # print(CohortDeviation$cohort_average)
  print(data.frame(cohort_average_group = CohortDeviation$cohort_average[,1],
                   cohort_average = sprintf("%.3f",as.numeric(CohortDeviation$cohort_average[,2])),
                   cohort_average_se = sprintf("%.3f",as.numeric(CohortDeviation$cohort_average[,3])),
                   cohort_average_t = sprintf("%.3f",as.numeric(CohortDeviation$cohort_average[,4])),
                   cohort_average_p = sprintf("%.3f",as.numeric(CohortDeviation$cohort_average[,5])),
                   cohort_average_sig = CohortDeviation$cohort_average[,6]
  ))
  message("")

  message("Cohort Life Course Dynamics: \n")
  # print(CohortDeviation$cohort_slope)
  print(data.frame(cohort_slope_group = CohortDeviation$cohort_slope[,1],
                   cohort_slope = sprintf("%.3f",as.numeric(CohortDeviation$cohort_slope[,2])),
                   cohort_slope_se = sprintf("%.3f",as.numeric(CohortDeviation$cohort_slope[,3])),
                   cohort_slope_t = sprintf("%.3f",as.numeric(CohortDeviation$cohort_slope[,4])),
                   cohort_slope_p = sprintf("%.3f",as.numeric(CohortDeviation$cohort_slope[,5])),
                   cohort_slope_sig = CohortDeviation$cohort_slope[,6]
  ))
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
       cohort_index = CohortDeviation$cohort_index,
       data = data)
}

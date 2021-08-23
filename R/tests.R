
tests <- function(model,age="acc",period="pcc",cohort="ccc",
                  A, P, C,
                  data,
                  weight = "wt",
                  family,
                  outcome,
                  ...){
  if(family=="binomial"){
    family <- "quasibinomial"
  }

  ## global test ##
  # x = survey::regTermTest(model, "acc:pcc", method="Wald")
  x = survey::regTermTest(model, paste0(age,":",period), method="Wald")
  step1gf = c(x$Ftest, x$df, x$ddf, x$p)
  names(step1gf) = c("GlobalF", "df1", "dd2", "p")

  # message("Global F test: \n")
  # print(step1gf)
  # message("")

  # global test results
  if(step1gf["p"] > 0.05){
    warning("\n no significant overall age-by-period interaction effect")
  }

  ## local F test ##
  # ia.index = array(seq(1:(A*P)), dim=c(A,P))
  # data$ia <- sapply(1:nrow(data),function(i){
  #   ia.index[data$ac[i], data$pc[i]]
  # })
  # wald.test  = array(rep(0, C*5), dim=c(C, 5))
  #
  # step2 <- lapply(1:C, function(k){
  #   data$ia_sel = 0
  #   data$ia_sel[data$cc==k] = data$ia[data$cc==k]
  #   data$ia_sel = as.factor(data$ia_sel)
  #
  #   wtdata = survey::svydesign(id=~1, strata=NULL, data=data,
  #                              weights=as.formula(paste0("~",weight)))
  #
  #   # m2 = survey::svyglm(inlfc ~ acc + pcc + ia_sel, wtdata, family = quasibinomial)
  #   m2 = survey::svyglm(as.formula(paste0(outcome,"~",age,"+",period,"+","ia_sel")), wtdata, family = get(family))
  #
  #   wald = survey::regTermTest(m2, "ia_sel", method="Wald")
  #   wald.sig               = rep('   ', 1)
  #   wald.sig[wald$p<0.05]  = '*  '
  #   wald.sig[wald$p<0.01]  = '** '
  #   wald.sig[wald$p<0.001] = '***'
  #   wald.test[k,] <<- cbind(wald$Ftest, wald$df, wald$ddf, wald.sig, wald$p)
  # })
  #
  # cohort_index = 1:C
  #
  # step2lf=cbind(cohort_index, wald.test[,c(1,2,3,5,4)])
  # colnames(step2lf) = c('cohort_index', 'df1', 'df2', 'Ftest', "p-val", "Significance")

  # message("Local F test: \n")
  # print(step2lf)
  # message("")

  # list(dev_global=step1gf,dev_local=step2lf)
  list(dev_global=step1gf)
}

# tests(model = temp6,A,P,C,data = data,age="acc",period="pcc",cohort="ccc")




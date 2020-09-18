# change column names (cohort slope)

cohortdeviation <- function(A,P,C,
                            model = temp6,
                            weight = "wt",
                            covariate,
                            ...){
  library("magrittr")

  r6 = model$coefficients[stringr::str_detect(names(model$coefficients) , "acc|pcc|(Intercept)")]
  r6se = summary(model)$coef[stringr::str_detect(names(model$coefficients) , "acc|pcc|(Intercept)"),"Std. Error"]
  r6p = summary(model)$coef[stringr::str_detect(names(model$coefficients) , "acc|pcc|(Intercept)") ,"Pr(>|t|)"]

  ############# computing "full" interactions index ##############

  T = array(rep(0, A*P*(A-1)*(P-1)), dim=c(A*P, (A-1)*(P-1)))

  ind1 = A*1:(P-1)
  ind2 = (A*(P-1)+1):(A*P-1)
  ind3 = A*P

  ind = c(ind1,ind2,ind3)

  newind = 1:(A*P)
  newind = newind[-ind]

  T[newind,]  = diag((A-1)*(P-1))
  T[ind1,]    = -diag(P-1)[,rep(1:(P-1),each=A-1)]
  T[ind2,]    = -diag(A-1)[,rep(1:(A-1),P-1)]
  T[ind3,]    = rep(1,(A-1)*(P-1))

  ############# computing "full" interactions contrast ##############

  # iatemp = vcov(model)[(covn+A+P): length(r6), (covn+A+P): length(r6)]
  row_ind <- stringr::str_detect(rownames(vcov(model)) , "^acc([0-9])*:pcc([0-9])*$")
  col_ind <- stringr::str_detect(colnames(vcov(model)) , "^acc([0-9])*:pcc([0-9])*$")

  row_ind_r6 <- stringr::str_detect(names(r6) , "^acc([0-9])*:pcc([0-9])*$")
  col_ind_r6 <- stringr::str_detect(names(r6) , "^acc([0-9])*:pcc([0-9])*$")

  iatemp = vcov(model)[row_ind,col_ind]
  iavcov = T%*%iatemp%*%t(T)
  df = model$df.residual

  iaesti = as.vector(T%*%r6[row_ind_r6])
  iase   = sqrt(diag(iavcov))
  iap    = pt(-abs(iaesti/iase), df)*2

  cindex <- sapply(1:P,function(j){
    seq((A+j-1),j, -1)
  })

  sig = rep('   ', (A*P))
  sig[iap<.05]  = '*  '
  sig[iap<.01]  = '** '
  sig[iap<.001] = '***'
  iasig = sig

  cohortindex = as.vector(cindex)
  ia          = as.data.frame(cbind(iaesti,iase,iap,iasig, cohortindex))

  ####################### inter-cohort changes #######################
cohortint <- sapply(1:C,function(k){
  O = sum(cindex == k)
  k1 = rep(1/O, O)
  k2 = rep(0, A*P)
  k2[cindex == k] = k1

  contresti = k2%*%iaesti
  contrse = sqrt(t(k2)%*%iavcov%*%k2)

  t = contresti/contrse

  if (t > 0){
    p = 2*pt(t, df, lower.tail=F)
  } else {
    p = 2*pt(t, df, lower.tail=T)
  }

  sig <- '   '
  if (p<.05){
    sig <- '*  '
  }
  if(p<.01){
    sig <- '** '
  }
  if(p<.001){
    sig <- '***'
  }


  c(contresti,contrse,t,p,sig)
})%>%t%>%
  as.data.frame%>%
  # `colnames<-`(c("cint","cintse","cintt","cintp","sig"))
  `colnames<-`(c("cohort_average","cohort_average_se",
                 "cohort_average_t","cohort_average_p","sig"))

  cohortint$cohort_group = seq(1, C)
  cohortint = cohortint[,c("cohort_group", "cohort_average","cohort_average_se",
                           "cohort_average_t","cohort_average_p", "sig")]


  ####################### intra-cohort changes #######################


  poly = 1
  cohortslope <- sapply((poly+1):(C-poly),function(k){
    o = sum(cindex == k)
    k1 = contr.poly(o)
    k2 = rep(0, A*P)
    k2[cindex == k] = k1[,poly]

    contresti = k2%*%iaesti
    contrse = sqrt(t(k2)%*%iavcov%*%k2)
    t = contresti/contrse
    if (t > 0){
      p = 2*pt(t, df, lower.tail=F)
    } else {
      p = 2*pt(t, df, lower.tail=T)
    }

    sig <- '   '
    if (p<.05){
      sig <- '*  '
    }
    if(p<.01){
      sig <- '** '
    }
    if(p<.001){
      sig <- '***'
    }


    c(contresti,contrse,t,p,sig)
  })%>%t%>%
    as.data.frame%>%
    # `colnames<-`(c("cslope","cslopese","cslopet","cslopep","sig"))
    `colnames<-`(c("cohort_slope","cohort_slope_se","cohort_slope_t",
                   "cohort_slope_p","sig"))

  cohortslope <- rbind(NA,cohortslope,NA)
  cohortslope$cohort_group = seq(1, C)
  # sigintra = sig
  cohortslope = cohortslope[,c("cohort_group", "cohort_slope","cohort_slope_se","cohort_slope_t",
                               "cohort_slope_p", "sig")]
# message("Cohortint")
# print(cohortint)
# message("Cohortslope")
# print(cohortslope)

  list(cohort_average = cohortint,cohort_slope=cohortslope,
       int_matrix = ia)

}

# cohortdeviation(A,P,C)

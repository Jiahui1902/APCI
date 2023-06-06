


# load packages: if packages is not successfully loaded, install the corresponding
# packages
# pkgs <- c("haven","survey","tidyverse","magrittr",'data.table')
# installpackages <- lapply(pkgs,function(x){
#   if(x %in% rownames(installed.packages()) == FALSE) {install.packages(`x`)}
# })
# loadpackages <- lapply(pkgs,function(x){
#   library(`x`,character.only = T)
# })
# rm(list = c("installpackages","loadpackages","pkgs"))



# change variable names ()

# get temp6
temp_model <- function(data,
                       outcome = "inlfc",
                       age = "acc",
                       period = "pcc",
                       cohort = NULL,
                       weight = NULL,
                       covariate = NULL,
                       family = "quasibinomial",
                       gee = FALSE,
                       id = NULL,
                       corstr = "exchangeable",
                       ...){
  ###############
  #no missing data
  ###############
  data2 <- as.data.frame(data)

  if(family=="binomial"&gee==FALSE){
    family <- "quasibinomial"
  }

  # data2 = na.omit( data[ , unique(c(outcome,age,period,cohort,weight,covariate,id)[!is.null(c(outcome,age,period,cohort,weight,covariate,id))]) ] )

  if(is.null(weight)){
    weight <- 1
  }

  # A = nlevels(data2[,age])
  # P = nlevels(data2[,period])
  A = length(unique(data2[,age]))
  P = length(unique(data2[,period]))
  C = A + P - 1

  data2$acc <- data2[,age]
  data2$pcc <- data2[,period]

  data2$acc <- as.factor(data2$acc)
  data2$pcc <- as.factor(data2$pcc)

  # if(age!='acc'&length(str_which(covariate,age))==0 ){
  # data2[,age] <- NULL
  # }
  # if(period!='pcc'&length(str_which(covariate,period))==0){
  # data2[,period] <- NULL
  # }
  #
  # if(length(str_which(covariate,age))>0){
  # covariate <- covariate[-str_which(covariate,age)]
  # }
  # if(length(str_which(covariate,period))>0){
  # covariate <- covariate[-str_which(covariate,period)]
  # }

  options(contrasts=c("contr.sum","contr.poly"), na.action = na.omit)
  wtdata2 = survey::svydesign(id=~1, strata=NULL, data=data2,
                              weights=as.formula(paste0("~",weight)))

  if(!is.null(covariate)&length(covariate)>0){
    temp6_formula <- as.formula(paste0(outcome,"~",paste(c(paste(covariate,collapse = "+"),
                                                           paste0(age,"*",period)),collapse = "+")))
  }else{
    temp6_formula <- as.formula(paste0(outcome,"~",paste(c(paste0(age,"*",period)),collapse = "+")))
  }

  print(temp6_formula)

  if(!is.null(covariate)&length(covariate)>0){
    temp6_formula <- as.formula(paste0(outcome,"~",paste(c(paste(covariate,collapse = "+"),
                                                           paste0('acc',"*",'pcc')),collapse = "+")))
  }else{
    temp6_formula <- as.formula(paste0(outcome,"~",paste(c(paste0('acc',"*",'pcc')),collapse = "+")))
  }

  if(gee==TRUE){
    temp6 = gee::gee(temp6_formula,
                     id = id,
                     #id = get(id),
                     data = data2,
                     family = get(family),
                     corstr = corstr)
    temp6$model <- data2

  }else{
  temp6 = survey::svyglm(temp6_formula,
                         wtdata2,
                         family = get(family))
  if(temp6$df.residual == 0){
    temp6 = glm(temp6_formula,
                 data2,
                 family = get(family))
  }
  }

  # temp6
  # output
  list(A=A,P=P,C=C,model=temp6)

}


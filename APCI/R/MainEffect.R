# age main effect, period main effect (add effecgt names: Intercept, age1 .. A, period1...P)
# change main effect to age effect, period effect

maineffect <- function(A,P,C,
                       model = temp6,
                       data,
                       ...){

  r6 = model$coefficients[stringr::str_detect(names(model$coefficients) , "acc|pcc|(Intercept)")]
  r6se = summary(model)$coef[stringr::str_detect(names(model$coefficients) , "acc|pcc|(Intercept)"),"Std. Error"]
  r6p = summary(model)$coef[stringr::str_detect(names(model$coefficients) , "acc|pcc|(Intercept)") ,"Pr(>|t|)"]


############# computing "full" age, period, and covariance effects ##############
fullae = array(rep(0, A), dim=c(A, 1))
fullas = array(rep(0, A), dim=c(A, 1))
S1 = array(rep(0, A*(A-1)), dim=c(A, (A-1)))
ind = A*1:(A-1)
newind = 1:(A*(A-1))
newind = newind[-ind]
S1[newind]  = diag(A-1)
S1[ind]    = rep(-1,(A-1))
# fullae = as.vector(S1%*%model$coef[(1+covn+1):(1+covn+A-1)])
fullae = as.vector(S1%*%model$coef[stringr::str_detect(names(model$coef) , "^acc([0-9])*$" )])

row_ind <- stringr::str_detect(rownames(vcov(model)) , "^acc([0-9])*$")
col_ind <- stringr::str_detect(colnames(vcov(model)) , "^acc([0-9])*$")

fullas = sqrt(diag(S1%*%vcov(model)[row_ind, col_ind]%*%t(S1)))
fullat = fullae/fullas
fullap = pt(-abs(fullat),df.residual(model))*2

sig = rep('   ', A)
sig[fullap<.05] = '*  '
sig[fullap<.01] = '** '
sig[fullap<.001] = '***'
fullasig = sig
fulla=cbind(fullae, fullas, fullap, fullasig)


fullpe = array(rep(0, P), dim=c(P, 1))
fullps = array(rep(0, P), dim=c(P, 1))
S2 = array(rep(0, P*(P-1)), dim=c(P, (P-1)))
ind = P*1:(P-1)
newind = 1:(P*(P-1))
newind = newind[-ind]
S2[newind]  = diag(P-1)
S2[ind]    = rep(-1,(P-1))

fullpe = as.vector(S2%*%model$coef[stringr::str_detect(names(model$coef) , "^pcc([0-9])*$" )])
row_ind <- stringr::str_detect(rownames(vcov(model)) , "^pcc([0-9])*$")
col_ind <- stringr::str_detect(colnames(vcov(model)) , "^pcc([0-9])*$")

fullps = sqrt(diag(S2%*%vcov(model)[row_ind,col_ind]%*%t(S2)))
fullpt = fullpe/fullps
fullpp = pt(-abs(fullpt),df.residual(model))*2

sig = rep('   ', P)
sig[fullpp<.05] = '*  '
sig[fullpp<.01] = '** '
sig[fullpp<.001] = '***'
fullpsig = sig
fullp=cbind(fullpe, fullps, fullpp, fullpsig)

inte = as.vector(r6[1])
intse = r6se[1]
intp = r6p[1]
intsig = rep('   ', 1)
intsig[r6p[1]<.05] = '*  '
intsig[r6p[1]<.01] = '** '
intsig[r6p[1]<.001] = '***'
fullint = cbind(inte,intse,intp,intsig)

maineff = rbind(fullint,fulla,fullp)
colnames(maineff) = c("estimate", "se", "p", "sig")
rownames(maineff) = c()

age_results <- maineff[2:(A+1),]
colnames(age_results) = c("age_estimate", "age_se", "age_p", "sig")
rownames(age_results) = c()

period_results <- maineff[(A+2):(A+P+1),]
colnames(period_results) = c("period_estimate", "period_se", "period_p", "sig")
rownames(period_results) = c()


list(intercept = maineff[1,],
     age_effect = cbind(`age_group` = seq(1,A),age_results),
     period_effect = cbind(`period_group` = seq(1,P), period_results))

}

# maineffect(A,P,C,model = temp6)

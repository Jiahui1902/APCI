# install the package and run this script
library("APCI")
rm(list=ls())
test_data <- APCI::data
test_data$acc <- as.factor(test_data$acc)
test_data$pcc <- as.factor(test_data$pcc)
test_data$educc <- as.factor(test_data$educc)
test_data$educr <- as.factor(test_data$educr)
# test_data$newsr <- as.factor(test_data$newsr)

APC_I <- APCI::apci(outcome = "inlfc",
                    age = "acc",
                    period = "pcc",
                    cohort = "ccc",
                    weight = "wt",
                    covariate = c("age", "year"),
                    data = test_data,dev.test=FALSE,
                    family = "gaussian")

# check results
summary(APC_I)

APC_I$model
APC_I$model%>%summary
APC_I$dev_global
APC_I$dev_local
APC_I$intercept
APC_I$age_effect
APC_I$period_effect
APC_I$cohort_average
APC_I$cohort_slope


library('tidyverse')
library('data.table')
vis_test <- test_data%>%
  group_by(acc,pcc)%>%
  summarise(inlfc = mean(inlfc,na.rm = T))

ggplot(vis_test,aes(x=pcc,group=acc,y = inlfc,col=acc))+
  geom_point()+
  geom_path()

ggplot(vis_test,aes(x=pcc,y=acc,fill=inlfc))+
  geom_tile()+
  coord_equal()+
  theme_bw()+
  scale_fill_gradient(low = 'white',high = 'black',
                      name="inlfc")+
  labs(x = 'period group',
       y = 'age group')

vis_test <- dcast(test_data%>%as.data.table,acc~pcc,
                  value.var = "inlfc",fun.aggregate = function(x){
                    mean(x,na.rm = T)})%>%
  as_tibble
vis_test <- vis_test[,-1]
vis_test <- as.matrix(vis_test)

apci.plot.hex(data = vis_test,
               first_age = 1,
               first_period = 1,
               interval = 1,
               first_age_isoline = 1,
               first_period_isoline = 1,
               isoline_interval = 1,
               scale_units = "inlfc",
               color_scale = c(0,1))



# do not run ###### parallel plots from regressions
# par(mfrow=c(2,2),ask=F)
# APC_I$model%>%plot

# vis_test <- dcast(test_data%>%as.data.table,acc~pcc,
#       value.var = "inlfc",fun.aggregate = function(x){
#         mean(x,na.rm = T)})%>%
#   as_tibble
# lattice::levelplot(vis_test[,-1]%>%as.matrix%>%t,xlab='period group',
#                    ylab='age group',
#                    col.regions = colorRampPalette(c('white','black')))


### Other tests ##########

rm(list = ls())
library(APCI)
test_data <- read.table("/Users/xujiahui/Documents/APC_I/test_data/ws data 2.txt")
test_data$acc <- as.factor(test_data$acc)
test_data$pcc <- as.factor(test_data$pcc)
test_data$educc <- as.factor(test_data$educc)
test_data$newsr <- as.factor(test_data$newsr)

APC_I <- APCI::apci(outcome = "wordsum",
                    age = "acc",
                    period = "pcc",
                    cohort = "ccc",
                    weight = "wt",
                    covariate = c("educc","newsr"),
                    data = test_data,dev.test=T,family = "gaussian")



s1 <- temp_model(outcome = "wordsum",
                 age = "acc",
                 period = "pcc",
                 cohort = "ccc",
                 weight = "wt",
                 covariate = c("educc","newsr"),
                 data = test_data,dev.test=F,family = "gaussian")

nlevels(as.factor(c("1","1")))
APC_I$age_effect
APC_I$period_effect
APC_I$cohort_average
APC_I$intercept
APC_I$cohort_slope
summary(s1$model)



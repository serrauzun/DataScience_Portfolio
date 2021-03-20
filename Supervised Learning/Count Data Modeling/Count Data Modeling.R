getwd()
setwd("/Users/serrauzun/Desktop/MSDS_410_Supervised/Assignment #9")
install.packages("pscl")
library(pscl)
library(MASS)
library(Metrics)
library(stargazer)
require(caTools)
library(plyr)
library(psych)
library(lessR)
medical.df <- read.delim("medical_care.txt",header = FALSE, sep = "", dec = ".")

## ADD Column Names 
colnames(medical.df) <- c("ofp","ofnp","opp","opnp","emr","hosp","exclhlth","poorhlth","numchron","adldiff","noreast", "midwest","west","age","black","male","married","school","faminc","employed","privins","medicaid")
colnames(medical.df)
str(medical.df)
table(medical.df$ofp)

#drop ofnp, opp, opnp, emr,and hosp
colnames(medical.df)
dim(medical.df)
medical.df_clean <- medical.df[,-c(2:6)]
dim(medical.df_clean)
## SPLIT Data
set.seed(789) 
medical.df_clean$u <- runif(n=dim(medical.df_clean)[1],min=0,max=1)
sample <- sample.split(medical.df_clean$u, SplitRatio = .50)
train.df <- subset(medical.df_clean, sample == TRUE)
test.df  <- subset(medical.df_clean, sample == FALSE)

# Check your data split. The sum of the parts should equal the whole. # Do your totals add up?
dim(medical.df_clean)[1]
dim(train.df)[1]
dim(test.df)[1] 
dim(train.df)[1] + dim(test.df)[1]

dim(medical.df_clean)
colnames(medical.df_clean)
colnames(train.df)

train.df <- train.df[,-18]
test.df <- test.df[,-18]

upper.glm <- glm(ofp ~ ., data=train.df,family=c('poisson')) 
lower.glm <- glm(ofp ~ 1, data=train.df,family=c('poisson'))

#backward lm
backward.lm <- stepAIC(object=upper.glm,direction=c('backward')) 
summary(backward.lm)
out.path = "/Users/serrauzun/Desktop/MSDS_410_Supervised/Assignment #9"
bckwrd_lm <- 'Backward.html';
stargazer(backward.lm, type=c('html'),out=paste(out.path,bckwrd_lm,sep=''),
          title=c('Backward LM'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, 
          column.labels=c('Backward LM'), intercept.bottom=FALSE)

#############################################
#############################################
#############################################
#############################################
#############################################
#############################################
gof.ins <- function(model,response){
  
  f.aic <- AIC(model);
  f.bic <- BIC(model);
  
  f.mse <- mean((response - model$fitted.values)^2);
  f.mae <- mean(abs(response - model$fitted.values));
  f.mape <- mean(abs(response - model$fitted.values)/response);
  
  abs.residual <- abs(response - model$fitted.values);
  grade.2 <- mean(ifelse(abs.residual<=2,1,0));
  
  output <- list(AIC=f.aic,BIC=f.bic,MSE=f.mse,MAE=f.mae,MAPE=f.mape,GRADE=grade.2);
  return(output)
}

gof.oos <- function(model,response){

  f.mse <- mean((response - model$fitted.values)^2);
  f.mae <- mean(abs(response - model$fitted.values));
  f.mape <- mean(abs(response - model$fitted.values)/response);
  
  abs.residual <- abs(response - model$fitted.values);
  grade.2 <- mean(ifelse(abs.residual<=2,1,0));
  
  output <- list(MSE=f.mse,MAE=f.mae,MAPE=f.mape,GRADE=grade.2);
  return(output)
}

#############################################
#############################################
#############################################
#############################################
#############################################
#############################################
#POISSON REGRESSION
model.poisson <- glm(ofp ~ exclhlth + poorhlth + numchron + adldiff + noreast + midwest + west + age + male + school + faminc + privins + medicaid,family="poisson", data=train.df)
summary(model.poisson)

poisson <- 'Poisson.html';
stargazer(model.poisson, type=c('html'),out=paste(out.path,poisson,sep=''),
          title=c('Poisson Regression'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, 
          column.labels=c('Poisson Regression'), intercept.bottom=FALSE)

#POISSON REGRESSION WITH DISPERSION 
model.poisson_disp <- glm(ofp ~ exclhlth + poorhlth + numchron + adldiff + noreast + midwest + west + age + male + school + faminc + privins + medicaid,family="quasipoisson", data=train.df)
summary(model.poisson_disp)
help("abs")
poisson_disp <- 'Poisson_disp.html';
stargazer(model.poisson_disp, type=c('html'),out=paste(out.path,poisson_disp,sep=''),
          title=c('Poisson Regression with Dispersion'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, 
          column.labels=c('Poisson Regression with Dispersion'), intercept.bottom=FALSE)

#############################################
#NEGATIVE BINOMINAL 
help(glm.nb)
neg.binom <- glm.nb(ofp ~ exclhlth + poorhlth + numchron + adldiff + noreast + midwest + west + age + male + school + faminc + privins + medicaid, data=train.df)
summary(neg.binom)

neg_binom <- 'Neg Binom.html';
stargazer(neg.binom, type=c('html'),out=paste(out.path,neg_binom,sep=''),
          title=c('Negative Binominal'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, 
          column.labels=c('Negative Binominal'), intercept.bottom=FALSE)

#############################################
#HURDLE 
model.hurdle <- hurdle(ofp ~ exclhlth + poorhlth + numchron + adldiff + noreast + midwest + west + age + male + school + faminc + privins + medicaid, data=train.df)
summary(model.hurdle)

hurdle_file <- 'Hurdle Regression.html';
stargazer(model.hurdle, type=c('html'),out=paste(out.path,hurdle_file,sep=''),
          title=c('Hurdle Regression'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, 
          column.labels=c('Hurdle Regression'), intercept.bottom=FALSE)

#############################################
#ZERO-INFLATED
model.zeroinfl <- zeroinfl(ofp ~ exclhlth + poorhlth + numchron + adldiff + noreast + midwest + west + age + male + school + faminc + privins + medicaid, data=train.df)
summary(model.zeroinfl)

zeroinfl_file <- 'Zero-Inflated Regression.html';
stargazer(model.hurdle, type=c('html'),out=paste(out.path,zeroinfl_file,sep=''),
          title=c('Zero-Inflated Regression'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, 
          column.labels=c('Zero-Inflated Regression'), intercept.bottom=FALSE)

### Results
gof.ins(model.poisson,train.df$ofp)
gof.ins(model.poisson_disp,train.df$ofp)
gof.ins(neg.binom,train.df$ofp)
gof.ins(model.hurdle,train.df$ofp)
gof.ins(model.zeroinfl,train.df$ofp)

############################################################
############################################################
############################################################
#POISSON REGRESSION TEST
model.poisson_test <- glm(ofp ~ exclhlth + poorhlth + numchron + adldiff + noreast + midwest + west + age + male + school + faminc + privins + medicaid,family="poisson", data=test.df)
summary(model.poisson_test)

#############################################
#POISSON REGRESSION WITH DISPERSION TEST
model.poisson_disp_test <- glm(ofp ~ exclhlth + poorhlth + numchron + adldiff + noreast + midwest + west + age + male + school + faminc + privins + medicaid,family="poisson", data=test.df)
summary(model.poisson_disp_test)

#############################################
#NEGATIVE BINOMINAL TEST
neg.binom_test <- glm.nb(ofp ~ exclhlth + poorhlth + numchron + adldiff + noreast + midwest + west + age + male + school + faminc + privins + medicaid, data=test.df)
summary(neg.binom_test)

#HURDLE TEST
help(hurdle)
model.hurdle_test <- hurdle(ofp ~ exclhlth + poorhlth + numchron + adldiff + noreast + midwest + west + age + male + school + faminc + privins + medicaid, data=test.df)
summary(model.hurdle_test)

#############################################
#ZERO-INFLATED TEST
help(zeroinfl)
model.zeroinfl_test <- zeroinfl(ofp ~ exclhlth + poorhlth + numchron + adldiff + noreast + midwest + west + age + male + school + faminc + privins + medicaid, data=test.df)
summary(model.zeroinfl_test)

### Results
gof.oos(model.poisson_test,test.df$ofp)
gof.oos(model.poisson_disp_test,test.df$ofp)
gof.oos(neg.binom_test,test.df$ofp)
gof.oos(model.hurdle_test,test.df$ofp)
gof.oos(model.zeroinfl_test,test.df$ofp)

# Classification - Function
conf_matrix <- function(actual, predict){
  actual_segment <- ifelse(actual<=5,'0-5',
                           ifelse(actual>5 & actual<=10,'6-10', 
                                  ifelse(actual>10,'11+', 
                                         'Other')))
  predict_segment <- ifelse(predict<=5,'0-5',
                            ifelse(predict>5 & predict<=10,'6-10', 
                                   ifelse(predict>10,'11+', 
                                          'Other')))
  t <- table(actual_segment, predict_segment)
  r <- apply(t,MARGIN=1,FUN=sum)
  return (t/r)
}


# Classification - In-Sample
conf_matrix(train.df$ofp, model.poisson$fitted.values)
conf_matrix(train.df$ofp, model.poisson_disp$fitted.values)
conf_matrix(train.df$ofp, neg.binom$fitted.values)
conf_matrix(train.df$ofp, model.hurdle$fitted.values)
conf_matrix(train.df$ofp, model.zeroinfl$fitted.values)

# Classification - Out-Sample
conf_matrix(test.df$ofp, model.poisson_test$fitted.values)
conf_matrix(test.df$ofp, model.poisson_disp_test$fitted.values)
conf_matrix(test.df$ofp, neg.binom_test$fitted.values)
conf_matrix(test.df$ofp, model.hurdle_test$fitted.values)
conf_matrix(test.df$ofp, model.zeroinfl_test$fitted.values)


# Assignment_8
# Serra Uzun
# 11.08.2020

install.packages("ggplot2")
library(ggplot2)
library(stargazer)
library(plyr)
library(psych)
library(lessR)
library(corrplot)
library(pROC)
out.path <- '/Users/serrauzun/Desktop/MSDS_410_Supervised/Assignment #8';

unibank <- UniversalBank

dim(unibank)
str(unibank)
head(unibank)

#remove ID and ZIPCODE from main dataset
colnames(unibank)
unibank <- unibank[,-c(1,5)]
head(unibank)

set.seed(12345)
unibank$u <- runif(n=dim(unibank)[1],min=0,max=1)

# Create train/test split;
train.df <- subset(unibank, u<0.70)
test.df <- subset(unibank, u>=0.70)
# Check your data split. The sum of the parts should equal the whole. # Do your totals add up?
dim(unibank)[1]
dim(train.df)[1]
dim(test.df)[1] 
dim(train.df)[1] + dim(test.df)[1]

#####################################################################
# Response rates for discrete variables
#####################################################################

summary(train.df)
cor(train.df)
corrplot(cor(train.df))


PersonalLoan <- train.df$PersonalLoan
Age <- train.df$Age
Online <- train.df$Online
SecuritiesAccount <- train.df$SecuritiesAccount
CDAccount <- train.df$CDAccount
CreditCard <- train.df$CreditCard

par(mfrow=c(1,4))
online_table <- table(PersonalLoan, Online)
barplot(online_table, main="Online Counts",
        xlab="Online", col=c('#42687C','#B3DAF1'))
online_table
Securities_table <- table(PersonalLoan, SecuritiesAccount)
barplot(Securities_table, main="Securities Account Counts",
        xlab="Securities Account", col=c('#42687C','#B3DAF1'))


CDAccount_table <- table(PersonalLoan, CDAccount)
barplot(CDAccount_table, main="CDAccount Counts",
        xlab="CDAccount", col=c('#42687C','#B3DAF1'))


CreditCard_table <- table(PersonalLoan, CreditCard)
barplot(CreditCard_table, main="CreditCard Counts",
        xlab="CreditCard", col=c('#42687C','#B3DAF1'),
        legend = rownames(CreditCard_table))

table(train.df$Online)
multi.hist(train.df[,-c(13)])
colnames(train.df)

colnames(train.df)
par(mfrow=c(1,4))
boxplot(Income ~ PersonalLoan, data = train.df, col='#42687C',xlab='PersonalLoan', ylab='Income' )
abline(h=mean(train.df$Income), col='red',lwd=1 , lty = 2 )
boxplot(Experience ~ PersonalLoan, data = train.df, col='#84A5B8',xlab='PersonalLoan', ylab='Experience' )
abline(h=mean(train.df$Experience), col='red',lwd=1 , lty = 2 )
boxplot(Mortgage ~ PersonalLoan, data = train.df, col='#CBCBCB',xlab='PersonalLoan', ylab='Mortgage' )
abline(h=mean(train.df$Mortgage), col='red',lwd=1 , lty = 2 )
boxplot(Age ~ PersonalLoan, data = train.df, col='#707571',xlab='PersonalLoan', ylab='Age' )
abline(h=mean(train.df$Age), col='red',lwd=1 , lty = 2 )


par(mfrow=c(1,2))
response.Education <- aggregate(train.df$PersonalLoan,by=list(Education=train.df$Education),FUN=mean)
barplot(height=response.Education$x,names.arg=response.Education$Education,xlab='Education Level',ylab='Response Rate', col = '#42687C')

response.Family <- aggregate(train.df$PersonalLoan,by=list(Family=train.df$Family),FUN=mean)
barplot(height=response.Family$x,names.arg=response.Family$Family, xlab='Family',ylab='Response Rate', col = '#84A5B8')

######################
# Bins
######################
par(mfrow=c(1,3))
train.df$CCAvg_Bins <- cut(train.df$CCAvg,breaks=10) 
table(train.df$CCAvg_Bins)
response.CCAvg_Bins <- aggregate(train.df$PersonalLoan,by=list(CCAvg_Bins=train.df$CCAvg_Bins),FUN=mean)
barplot(height=response.CCAvg_Bins$x,names.arg=response.CCAvg_Bins$CCAvg_Bins,xlab='CCAvg_Bin',ylab='Response Rate',las=2,cex.names=0.75, col = '#CBCBCB')

train.df$Age_Bins <- ifelse(Age <= 30, "20-30",ifelse(Age > 30 & Age <= 40, "30-40", ifelse(Age > 40 & Age <= 50, "40-50",ifelse(Age > 50 & Age <= 60, "50-60",ifelse(Age > 60,"60+","NA")))))
table(train.df$Age_Bins)
response.Age_Bins <- aggregate(train.df$PersonalLoan,by=list(Age_Bins=train.df$Age_Bins),FUN=mean)
barplot(height=response.Age_Bins$x,names.arg=response.Age_Bins$Age_Bins,xlab='Age Bins',ylab='Response Rate',las=2,cex.names=0.75 , col = '#42687C')

Experience <- train.df$Experience
train.df$Exp_Bins <- ifelse(Experience <= 0, "- 0",ifelse(Experience > 0 & Experience <= 10, "0-10", ifelse(Experience > 10 & Experience <= 20, "10-20",ifelse(Experience > 20 & Experience <= 30, "20-30",ifelse(Experience > 30 & Experience <= 40,"30-40",ifelse(Experience > 40,"40+","NA"))))))
table(train.df$Exp_Bins)
response.Exp_Bins <- aggregate(train.df$PersonalLoan,by=list(Exp_Bins=train.df$Exp_Bins),FUN=mean)
barplot(height=response.Exp_Bins$x,names.arg=response.Exp_Bins$Exp_Bins,xlab='Experience Bins',ylab='Response Rate',las=2,cex.names=0.75, col = '#84A5B8')

##################################
#cleaning of dataset for modeling
colnames(train.df)
train.df <- train.df[,-c(13:16)]
colnames(train.df)
dim(train.df)

##################################
#Variable Selection
##################################

library(MASS)

#limits
upper.glm <- glm(PersonalLoan ~ ., data=train.df,family=c('binomial')) 
lower.glm <- glm(PersonalLoan ~ 1, data=train.df,family=c('binomial'))
sqft.glm <- glm(PersonalLoan ~ Income, data=train.df, family=c('binomial')); 

#forward
forward.glm <- stepAIC(object=lower.glm,scope=list(upper=formula(upper.glm),lower=~1),direction=c('forward'))
summary(forward.glm)
stepAIC(forward.glm)

#backward
backward.glm <- stepAIC(object=upper.glm,direction=c('backward')) 
summary(backward.glm)
stepAIC(backward.glm)

#stepwise
stepwise.glm <- stepAIC(object=sqft.glm,scope=list(upper=formula(upper.glm),lower=~1), direction=c('both'));
summary(stepwise.glm)
stepwise.glm


###############################################
###############################################
# Model 1
model.1 <- glm(PersonalLoan ~ Income+CCAvg+CDAccount+factor(Education)+Family
               +SecuritiesAccount, data=train.df, family=c('binomial'))

summary(model.1)

file.name.1 <- 'Model.1.html';
stargazer(model.1, type=c('html'),out=paste(out.path,file.name.1,sep=''),
          title=c('Model.1'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, median=TRUE)

roc.1 <- roc(response=train.df$PersonalLoan, predictor=model.1$fitted.values)
print(roc.1)
auc.1 <- auc(roc.1)
auc.1
text1 = "Area under the curve: 0.9584"

par(mfrow=c(1,1))
plot(roc.1, col= 'red', lwd=2)
text(0.0,0.2,text1)

roc.specs <- coords(roc=roc.1,x=c('best'),input=c('threshold','specificity','sensitivity'),ret=c('threshold','specificity','sensitivity'),as.list=TRUE)

train.df$ModelScores <- model.1$fitted.values;
train.df$classes <- ifelse(train.df$ModelScores>roc.specs$threshold,1,0);

# Rough confusion matrix using counts;
table(train.df$PersonalLoan, train.df$classes)
t1 <- table(train.df$PersonalLoan, train.df$classes);
r1 <- apply(t1,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t1/r1

###############################################
###############################################
# Model 2
model.2 <- glm(PersonalLoan ~ Income + factor(Education) + CDAccount + Family + Online + CreditCard + SecuritiesAccount + CCAvg + Experience, data=train.df, family=c('binomial'))

summary(model.2)

file.name.2 <- 'Model.2.html';
stargazer(model.2, type=c('html'),out=paste(out.path,file.name.2,sep=''),
          title=c('Model.2'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, median=TRUE)

roc.2 <- roc(response=train.df$PersonalLoan, predictor=model.2$fitted.values)
print(roc.2)
auc.2 <- auc(roc.2)
auc.2
text2 = "Area under the curve: 0.9617"

par(mfrow=c(1,1))
plot(roc.2, col= 'blue', lwd=2)
plot(roc.3, col= 'orange', lwd=2, add = TRUE)
plot(roc.1, col= 'red', lwd=2, add = TRUE)


roc.specs2 <- coords(roc=roc.2,x=c('best'),input=c('threshold','specificity','sensitivity'),ret=c('threshold','specificity','sensitivity'),as.list=TRUE)

train.df$ModelScores2 <- model.2$fitted.values;
train.df$classes2 <- ifelse(train.df$ModelScores2>roc.specs2$threshold,1,0);

table(train.df$PersonalLoan, train.df$classes2)
t2 <- table(train.df$PersonalLoan, train.df$classes2);
r2 <- apply(t2,MARGIN=1,FUN=sum);
t2/r2


###############################################
###############################################
# Model 3
model.3 <- glm(PersonalLoan ~ Income+Online+Age+factor(Education)+CCAvg, data=train.df, family=c('binomial'))

summary(model.3)

file.name.3 <- 'Model.3.html';
stargazer(model.3, type=c('html'),out=paste(out.path,file.name.3,sep=''),
          title=c('Model.3'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, median=TRUE)

roc.3 <- roc(response=train.df$PersonalLoan, predictor=model.3$fitted.values)
print(roc.3)
plot(roc.3)
auc.3 <- auc(roc.3);
auc.3

text3 = "Area under the curve: 0.9412"

par(mfrow=c(1,1))
plot(roc.3, col= 'orange', lwd=2)
text(0.0,0.2,text3)

roc.specs3 <- coords(roc=roc.3,x=c('best'),input=c('threshold','specificity','sensitivity'),ret=c('threshold','specificity','sensitivity'),as.list=TRUE)

train.df$ModelScores3 <- model.3$fitted.values;
train.df$classes3 <- ifelse(train.df$ModelScores3>roc.specs3$threshold,1,0);

table(train.df$PersonalLoan, train.df$classes3)
t3 <- table(train.df$PersonalLoan, train.df$classes3);
r3 <- apply(t3,MARGIN=1,FUN=sum);
t3/r3




########
########
########
########
# Model 1 - TEST
model.1test <- glm(PersonalLoan ~ Income+CCAvg+CDAccount+factor(Education)+Family
               +SecuritiesAccount, data=test.df, family=c('binomial'))

summary(model.1test)

file.name.1test <- 'Model.1test.html';
stargazer(model.1test, type=c('html'),out=paste(out.path,file.name.1test,sep=''),
          title=c('Model.1test'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, median=TRUE)

roc.1test <- roc(response=test.df$PersonalLoan, predictor=model.1test$fitted.values)
print(roc.1test)
auc.1test <- auc(roc.1test)
auc.1test
text1test = "Area under the curve: 0.9591"

par(mfrow=c(1,1))
plot(roc.1test, col= 'red', lwd=2)
text(0.0,0.2,text1test)

roc.specs_test <- coords(roc=roc.1test,x=c('best'),input=c('threshold','specificity','sensitivity'),ret=c('threshold','specificity','sensitivity'),as.list=TRUE)

test.df$ModelScores <- model.1test$fitted.values;
test.df$classes <- ifelse(test.df$ModelScores>roc.specs_test$threshold,1,0)


table(test.df$PersonalLoan, test.df$classes)
t1test <- table(test.df$PersonalLoan, test.df$classes);
r1test <- apply(t1test,MARGIN=1,FUN=sum);

t1test/r1test

###############################################
###############################################
# Model 2 TEST
model.2test <- glm(PersonalLoan ~ Income + factor(Education) + CDAccount + Family + Online + CreditCard + SecuritiesAccount + CCAvg + Experience, data=test.df, family=c('binomial'))

summary(model.2test)

file.name.2test <- 'Model.2test.html';
stargazer(model.2test, type=c('html'),out=paste(out.path,file.name.2test,sep=''),
          title=c('Model.2test'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, median=TRUE)

roc.2test <- roc(response=test.df$PersonalLoan, predictor=model.2test$fitted.values)
print(roc.2test)
auc.2test <- auc(roc.2test)
auc.2test
text2test = "Area under the curve: 0.9635"

par(mfrow=c(1,1))
plot(roc.2test, col= 'red', lwd=2)
text(0.0,0.2,text1test)

roc.specs2test <- coords(roc=roc.2test,x=c('best'),input=c('threshold','specificity','sensitivity'),ret=c('threshold','specificity','sensitivity'),as.list=TRUE)

test.df$ModelScores2 <- model.2test$fitted.values;
test.df$classes2 <- ifelse(test.df$ModelScores2>roc.specs2test$threshold,1,0);

table(test.df$PersonalLoan, test.df$classes2)
t2test <- table(test.df$PersonalLoan, test.df$classes2);
r2test <- apply(t2test,MARGIN=1,FUN=sum);
t2test/r2test

###############################################
###############################################
# Model 3 TEST
model.3test <- glm(PersonalLoan ~ Income+Online+Age+factor(Education)+CCAvg, data=test.df, family=c('binomial'))

summary(model.3test)

file.name.3test <- 'Model.3test.html';
stargazer(model.3test, type=c('html'),out=paste(out.path,file.name.3test,sep=''),
          title=c('Model.3test'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, median=TRUE)

roc.3test <- roc(response=test.df$PersonalLoan, predictor=model.3test$fitted.values)
print(roc.3test)
plot(roc.3test)
auc.3test <- auc(roc.3test)
auc.3test
text3test = "Area under the curve: 0.9311"

par(mfrow=c(1,1))
plot(roc.3test, col= 'red', lwd=2)
text(0.0,0.2,text3test)

roc.specs3test <- coords(roc=roc.3test,x=c('best'),input=c('threshold','specificity','sensitivity'),ret=c('threshold','specificity','sensitivity'),as.list=TRUE)

test.df$ModelScores3 <- model.3test$fitted.values;
test.df$classes3 <- ifelse(test.df$ModelScores3>roc.specs3test$threshold,1,0);

table(test.df$PersonalLoan, test.df$classes3)
t3test <- table(test.df$PersonalLoan, test.df$classes3);
r3test <- apply(t3test,MARGIN=1,FUN=sum);
t3test/r3test

getwd()
setwd("/Users/serrauzun/Desktop/MSDS_450_Marketing/Solo3")
load("XYZ_complete_customer_data_frame.RData")
ls()
mydata <- complete.customer.data.frame
str(mydata)
names(mydata)

#EDA
hist(mydata$MED_RENT)
table(mydata$BUYER_STATUS)
table(mydata$RESPONSE16)
table(mydata$ANY_MAIL_16)
table(mydata$ZKITCHEN)
xtabs(~RESPONSE16 + ANY_MAIL_16, data = mydata)
#note: send mail to active customers instead of inactive and lapsed customers
xtabs(~BUYER_STATUS + ANY_MAIL_16, data = mydata)
xtabs(~BUYER_STATUS + RESPONSE16, data = mydata)

mydata$EXAGE[mydata$EXAGE=="U"] <- NA
mydata$EXAGE <- as.numeric(mydata$EXAGE)

par(mfrow=c(1,2))
#barplot(table(mydata$BUYER_STATUS), col = "#99AEAD" , main = "Buyer Status")
barplot(table(mydata$ANY_MAIL_16), col = "#658B6F" , main = "Mail 16", xlab= "(0=didn't receive mail, 1= received mail)")
barplot(table(mydata$RESPONSE16), col = "#6D9197" , main = "Response 16", xlab= "(0=didn't respond, 1= responded)")

barplot(table(resp.data.v3$D5), col = "#2F575D" , main = "Annual Income", xlab = "1=Under $25k, 2=$25k-$35k, 3=$35k-$50k, 4=$50k-$75k, 5=$75k-$100k, 6=$100k-$200k, 7=$200k+, 8=I prefer not to answer")

hist(mydata$EXAGE)
colnames(mydata)

#Variable Featuring
mydata$PRE2009SALES <- mydata$LTD_SALES - mydata$YTD_SALES_2009
mydata$PRE2009TRANSACTIONS <- mydata$LTD_TRANSACTIONS - mydata$YTD_TRANSACTIONS_2009
mydata$cum15QTY <- mydata$QTY0 + mydata$QTY1 + mydata$QTY2 + mydata$QTY3 +
  mydata$QTY4 + mydata$QTY5 + mydata$QTY6 + mydata$QTY7 + mydata$QTY8 +
  mydata$QTY9 + mydata$QTY10 + mydata$QTY11 + mydata$QTY12 + mydata$QTY13 +
  mydata$QTY14 + mydata$QTY15

mydata$cum15TOTAMT <- mydata$TOTAMT0 + mydata$TOTAMT1 + mydata$TOTAMT2 + mydata$TOTAMT3 +
  mydata$TOTAMT4 + mydata$TOTAMT5 + mydata$TOTAMT6 + mydata$TOTAMT7 + mydata$TOTAMT8 +
  mydata$TOTAMT9 + mydata$TOTAMT10 + mydata$TOTAMT11 + mydata$TOTAMT12 + mydata$TOTAMT13 +
  mydata$TOTAMT14 + mydata$TOTAMT15

par(mfrow=c(1,5))
boxplot(mydata$PRE2009SALES, main="Pre2009 Sales")
boxplot(mydata$PRE2009_TRANSACTIONS, main="Pre2009 Transactions")
boxplot(mydata$MED_INC, main="Median Income")
boxplot(mydata$INC_WOUTSCS_AMT_4, main="Median Household Income")
boxplot(mydata$ESTHMVL, main="Estimated Current Home Value")

mydata$ESTHMVL <- as.numeric(mydata$ESTHMVL)
table(mydata$ESTHMVL)
str(mydata$ESTHMVL)
par(mfrow=c(1,3))

hist(mydata$MED_INC, main="Median Income", xlab = "Median Income")
hist(mydata$EXAGE, main="Age", xlab = "Customer Age")
hist(mydata$INC_WOUTSCS_AMT_4, main="Household Income", xlab = "Estimated Household Income")

hist(mydata$PRE2009_SALES)
hist(mydata$PRE2009_TRANSACTIONS)
aggregate(PRE2009_SALES, RESPONSE16, DATA=mydata)

boxplot(mydata$INC_WOUTSCS_AMT_4)

colnames(mydata)
boxplot(mydata$cum15TOTAMT)
unique(mydata$ZIP)
summary(mydata$cum15TOTAMT)
summary(mydata$mydata$cum15QTY)

mean(mydata$PRE2009SALES)
#average sale per customer pre 2009 was 979 dollars
mean(mydata$PRE2009TRANSACTIONS)
#pre 2009 on average each customer made 3.8 transactions
mean(mydata$cum15TOTAMT)
mean(mydata$cum15QTY)

mydata$salepercust <- mydata$PRE2009SALES/mydata$CENSUS_FACT1
summary(mydata$CENSUS_FACT1)

mydata$salepertrans <- mydata$PRE2009SALES/mydata$PRE2009TRANSACTIONS
mydata$salepercamp <- mydata$cum15TOTAMT/mydata$TOTAL_MAIL_15

mean(mydata$PRE2009SALES, trim = 0.001, na.rm = TRUE)
mean(mydata$PRE2009TRANSACTIONS, trim = 0.001, na.rm = TRUE)

table(mydata$ZKITCHEN)
aa <- mydata[mydata$ZKITCHEN == "",]
table(mydata$MEDIANAGE)
head(mydata$MEDIANAGE)
require(rpart)
require(rpart.plot)
require(tree)
require(rattle)
require(caTools)
require(ROCR)
require(ResourceSelection)

library(corrgram)
library(MASS)
library(randomForest)
library(inTrees)
library(pROC)
library(caret)
library(dplyr)

#Subsetting and Cleaning
subdat <- subset(mydata, select=c("PRE2009SALES","PRE2009TRANSACTIONS","MED_INC","cum15QTY", "QTY15",
                                  "cum15TOTAMT", "TOTAMT15","ESTHMVL","EXAGE", "INC_WOUTSCS_AMT_4","ZKITCHEN",
                                  "SUM_MAIL_16","TOTAL_MAIL_16","salepercamp", "NUMBADLT",
                                  "ANY_MAIL_16","RESPONSE16","salepertrans"))

subdat2 <- subset(subdat, ANY_MAIL_16 > 0)
sapply(subdat2, function(x) sum(is.na(x)))

subdat2 <- subset(subdat, ANY_MAIL_16 > 0)
str(subdat2)

subdat2$EXAGE[is.na(subdat2$EXAGE)] <- mean(subdat2$EXAGE,na.rm=TRUE)
subdat2$ESTHMVL[is.na(subdat2$ESTHMVL)] <- mean(subdat2$ESTHMVL,na.rm=TRUE)
subdat2$salepercamp[is.na(subdat2$salepercamp)] <- mean(subdat2$salepercamp,na.rm=TRUE)
subdat2$salepertrans[is.na(subdat2$salepertrans)] <- mean(subdat2$salepertrans,na.rm=TRUE)

sapply(subdat2, function(x) sum(is.na(x)))

subdat2$ZKITCHEN[subdat2$ZKITCHEN=="U"] <- 0
subdat2$ZKITCHEN[subdat2$ZKITCHEN==""] <- 0
subdat2$ZKITCHEN[subdat2$ZKITCHEN=="Y"] <- 1
subdat2$ZKITCHEN_u <- NULL
table(subdat2$ZKITCHEN)

subdat2$ZKITCHEN_U <- subdat2$ZKITCHEN
subdat2$ZKITCHEN_u[subdat2$ZKITCHEN== 1] <- 0
subdat2$ZKITCHEN_u[subdat2$ZKITCHEN== 0] <- 1
subdat2$ZKITCHEN_u <- NULL

Q1_sales <- quantile(subdat2$PRE2009SALES, .01)
Q3_sales <- quantile(subdat2$PRE2009SALES, .99)
IQR_sales <- IQR(subdat2$PRE2009SALES)

Q1_trans <- quantile(subdat2$PRE2009TRANSACTIONS, .01)
Q3_trans <- quantile(subdat2$PRE2009TRANSACTIONS, .99)
IQR_trans <- IQR(subdat2$PRE2009TRANSACTIONS)

Q1_value <- quantile(subdat2$ESTHMVL, .01)
Q3_value <- quantile(subdat2$ESTHMVL, .99)
IQR_value <- IQR(subdat2$ESTHMVL)
#
no_outliers1 <- subset(subdat2, subdat2$PRE2009SALES> (Q1_sales - 1.5*IQR_sales) & subdat2$PRE2009SALES< (Q3_sales + 1.5*IQR_sales))
no_outliers2 <- subset(no_outliers1, no_outliers1$PRE2009TRANSACTIONS> (Q1_trans - 1.5*IQR_trans) & no_outliers1$PRE2009TRANSACTIONS< (Q3_trans + 1.5*IQR_trans))
no_outliers3 <- subset(no_outliers2, no_outliers2$ESTHMVL> (Q1_value - 1.5*IQR_value) & no_outliers2$ESTHMVL< (Q3_value + 1.5*IQR_value))

par(mfrow=c(1,3))
boxplot(no_outliers3$PRE2009SALES, main="Pre2009 Sales")
boxplot(no_outliers3$PRE2009TRANSACTIONS,main="Pre2009 Transactions" )
boxplot(no_outliers3$ESTHMVL, main="Estimated Current Home Value")

subdat2 <- no_outliers3
dim(subdat2)
str(subdat2)

df_final <- subdat2

mean(df_final$EXAGE)
df_final$logPRE2009TRANSACTIONS <- log(df_final$PRE2009TRANSACTIONS)
df_final$logPRE2009SALES <- log(df_final$PRE2009SALES)
df_final$ZKITCHEN <- as.numeric(df_final$ZKITCHEN)
df_final$ZKITCHEN_U <- as.numeric(df_final$ZKITCHEN_U)
head(df_final)
str(df_final)


table(df_final$RESPONSE16)
### note: 8562/9712 = 88% did not respond

dim(df_final)
colnames(df_final)

#############################
#Logistic Regression Model 
sapply(df_final, function(x) sum(is.na(x)))
str(df_final)
df_final$PRE2009SALES<- as.numeric(df_final$PRE2009SALES)
df_final$PRE2009TRANSACTIONS<- as.numeric(df_final$PRE2009TRANSACTIONS)

df_final$TOTAL_MAIL_16 <- as.numeric(df_final$TOTAL_MAIL_16)
summary(df_final)

mylogit <- glm(RESPONSE16 ~ PRE2009SALES+MED_INC+EXAGE+PRE2009TRANSACTIONS+TOTAMT15+
                 salepertrans + cum15QTY, data = df_final, family = "binomial")

summary(mylogit)

install.packages("ResourceSelection")
library("ResourceSelection")
hoslem.test(df_final$RESPONSE16, fitted(mylogit))
pred2 <- predict(mylogit,data=df_final, type="response")
head(pred2)
str(pred2)
pred2_df <- as.data.frame(pred2)
pred2round <- round(pred2,0)
#confusion matrix
cm <- xtabs(~RESPONSE16 + pred2round, data = df_final)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy
##accuracy = 8510+35/9712 = 88%
##accuracy among responded: 35/(1115+35)= 3%

dim(pred2_df)

install.packages("prediction")
library(prediction)
library(caret)
### ROCR curve
ROCRpred <- prediction(pred2_df, df_final$RESPONSE16)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
abline(a=0,b=1)
auc(df_final$RESPONSE16,pred2)

par(mfrow=c(1,1))
hist(pred2, main = "Histogram of Logistic Regression Predictions", xlab="Accuracy")

pred2df <- as.data.frame(pred2)
data_all <- cbind(df_final,pred2)
str(data_all)
mean(data_all$salepertrans)
mean(data_all$salepertrans [data_all$pred2>0.8])

mean(data_all$EXAGE)
mean(data_all$EXAGE [data_all$pred2>0.5])

#############################
#Random Forest model
rf1 <- randomForest(RESPONSE16 ~ PRE2009SALES+MED_INC+EXAGE+PRE2009TRANSACTIONS+TOTAMT15+
                      salepertrans + cum15QTY ,data=df_final,importance=TRUE,ntree=100)
summary(rf1)

print(rf1)
par(mfrow=c(1,1))
plot(rf1)
importance(rf1)
varImpPlot(rf1, main = "Variable Importance Plot")

#prediction probabilities
rf1p  <- predict(rf1, newdata=df_final,type="response")
head(rf1p)
hist(rf1p, main = "Histogram of Random Forest Model Predictions", xlab="Accuracy")
rf1pdf <- as.data.frame(rf1p)
rf1.pred = prediction(rf1p, df_final$RESPONSE16)
rf1.perf = performance(rf1.pred,"tpr","fpr")
plot(rf1.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

auc(df_final$RESPONSE16,rf1p)

rf1pround <- round(rf1p,0)
cm <- xtabs(~RESPONSE16 + rf1pround, data = df_final)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy

data_alldf <- cbind(df_final,rf1pdf)
str(data_alldf)
head(data_alldf)

mean(data_alldf$salepertrans)
mean(data_alldf$salepertrans[data_alldf$rf1p>0.30])
mean(data_alldf$salepertrans[data_alldf$rf1p>0.35])
mean(data_alldf$salepertrans[data_alldf$rf1p>0.40])
mean(data_alldf$salepertrans[data_alldf$rf1p>0.45])
mean(data_alldf$salepertrans[data_alldf$rf1p>0.50])

mean(data_alldf$PRE2009SALES)
mean(data_alldf$PRE2009SALES[data_alldf$rf1p>0.75])

##Ploting the ZipCode Map
library(ggmap)
library(gridExtra)
library(ggplot2)

cc <- mydata
ccMap <- subset(cc,select=c(LONG,LAT,RESPONSE16))
ccMap$lon <- -(as.numeric(substr(ccMap$LONG,2,9))/1000000)
ccMap$lat <- (as.numeric(substr(ccMap$LAT,2,9))/1000000)
m1 <- qmplot(lon, lat, data =ccMap[ccMap$RESPONSE16 == 0,], color=I('red'),size = I(0.5), darken = .0) + ggtitle("Response16 = 0")
m2 <- qmplot(lon, lat, data =ccMap[ccMap$RESPONSE16 == 1,], color=I('green'),size = I(0.5), darken = .0) + ggtitle("Response16 = 1")
grid.arrange(m1, m2, nrow=2)
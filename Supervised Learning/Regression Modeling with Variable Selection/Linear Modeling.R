# Serra Uzun
# MSDS_410 Supervised Learning Methods_FALL 2020
# 09.27.2020
# Assignment_02

getwd()
setwd('/Users/serrauzun/Desktop/MSDS_410_Supervised/Assignment #1')
#Import dataset
ames.df <- readRDS('/Users/serrauzun/Desktop/MSDS_410_Supervised/Assignment #1/ames_sample.Rdata')

# columns and the types using the structure
str(ames.df)
colnames(ames.df)

# subset of predictor variables;
small.df <- ames.df[,c('SalePrice','TotalSqftCalc','TotalBathCalc','QualityIndex',
	'TotRmsAbvGrd','OverallQual','OverallCond')];
str(small.df)

# count of missing values for each variable
sapply(small.df, function(x) sum(is.na(x)))

#summary statistics of small.df
summary(small.df)

#a more detailed descriptive summary with stat.desc
install.packages("pastecs",repos = "http://cran.us.r-project.org")
library(pastecs)
small.df.summary <- as.data.frame(t(round(stat.desc(small.df), 1)))
small.df.summary


#####################################################################################
# Correlation Plots
#####################################################################################

# Install and load the corrplot package
#install.packages('corrplot', dependencies=TRUE)
library(corrplot)

# Correlations with SalePrice;
cor(small.df)

# Correlations with log(SalePrice);
# Need to drop SalePrice and add logSalePrice;
log.df <- subset(small.df, select=-c(SalePrice));
head(log.df)
log.df$logSalePrice <- log(small.df$SalePrice);
head(log.df)

sales_cor <- as.data.frame(cor(small.df))
sales_log_cor <- as.data.frame(cor(log.df))

par(mfrow = c(1,2))
corrplot(cor(small.df),method='number',tl.cex = 0.8, tl.col = "black")
corrplot(cor(log.df),method='number', tl.cex = 0.8, tl.col = "black")


#####################################################################################
# Scatterplots, Scatterplot Smoothers, and Simple Linear Regression
# Response Variable: SalePrice
#####################################################################################
par(mfrow = c(3,2))

### TotalSfqtCalc
loess.1 <- loess(SalePrice ~ TotalSqftCalc,data=small.df);
lm.1 <- lm(SalePrice ~ TotalSqftCalc,data=small.df);

plot(small.df$TotalSqftCalc, small.df$SalePrice,xlab='Total Square Footage',ylab='SalePrice')
points(loess.1$x,loess.1$fitted,type='p',col='red')
abline(coef=lm.1$coef,col='blue',lwd=2)
title('SLR - Total Square Footage')

### OverallQual
loess.2 <- loess(SalePrice ~ OverallQual,data=small.df);
lm.2 <- lm(SalePrice ~ OverallQual,data=small.df);

plot(small.df$SalePrice ~ small.df$OverallQual,xlab='Overall Quality',ylab='SalePrice')
points(loess.2$x,loess.2$fitted,type='p',col='red',pch=19)
abline(coef=lm.2$coef,col='blue',lwd=2)
title('SLR - Overall Quality')

# Note that loess() outputs warning messages.  Can we guess why?

### OverallCond
loess.3 <- loess(SalePrice ~ OverallCond,data=small.df);
lm.3 <- lm(SalePrice ~ OverallCond,data=small.df);

plot(small.df$SalePrice ~ small.df$OverallCond,xlab='Overall Condition',ylab='SalePrice')
points(loess.3$x,loess.3$fitted,type='p',col='red',pch=19)
abline(coef=lm.3$coef,col='blue',lwd=2)
title('SLR - Overall Condition')

### TotalBathCalc
loess.4 <- loess(SalePrice ~ TotalBathCalc ,data=small.df);
lm.4 <- lm(SalePrice ~ TotalBathCalc,data=small.df);

plot(small.df$SalePrice ~ small.df$TotalBathCalc,xlab='Total Number of Bathrooms',ylab='SalePrice')
points(loess.4$x,loess.4$fitted,type='p',col='red',pch=19)
abline(coef=lm.4$coef,col='blue',lwd=2)
title('SLR - Total Number of Bathrooms')

### QualityIndex
loess.5 <- loess(SalePrice ~ QualityIndex ,data=small.df);
lm.5 <- lm(SalePrice ~ QualityIndex,data=small.df);

plot(small.df$SalePrice ~ small.df$QualityIndex,xlab='Quality Index',ylab='SalePrice')
points(loess.5$x,loess.5$fitted,type='p',col='red',pch=19)
abline(coef=lm.5$coef,col='blue',lwd=2)
title('SLR - Quality Index')

### TotRmsAbvGrd
loess.6 <- loess(SalePrice ~ TotRmsAbvGrd ,data=small.df);
lm.6 <- lm(SalePrice ~ TotRmsAbvGrd,data=small.df);

plot(small.df$SalePrice ~ small.df$TotRmsAbvGrd,xlab='Total Rooms Above Ground',ylab='SalePrice')
points(loess.6$x,loess.6$fitted,type='p',col='red',pch=19)
abline(coef=lm.6$coef,col='blue',lwd=2)
title('SLR - Total Rooms Above Ground')


#####################################################################################
# Scatterplots, Scatterplot Smoothers, and Simple Linear Regression
# Response Variable: log(SalePrice)
#####################################################################################

par(mfrow = c(3,2))

### TotalSfqtCalc
loess.1 <- loess(log(SalePrice) ~ TotalSqftCalc,data=small.df);
lm.1 <- lm(log(SalePrice) ~ TotalSqftCalc,data=small.df);

plot(small.df$TotalSqftCalc, log(small.df$SalePrice),xlab='Total Square Footage',ylab='log(SalePrice)')
points(loess.1$x,loess.1$fitted,type='p',col='red')
abline(coef=lm.1$coef,col='blue',lwd=2)
title('SLR - Total Square Footage')

### OverallQual
loess.2 <- loess(log(SalePrice) ~ OverallQual,data=small.df);
lm.2 <- lm(log(SalePrice) ~ OverallQual,data=small.df);

plot(log(small.df$SalePrice) ~ small.df$OverallQual,xlab='Overall Quality',ylab='log(SalePrice)')
points(loess.2$x,loess.2$fitted,type='p',col='red',pch=19)
abline(coef=lm.2$coef,col='blue',lwd=2)
title('SLR - Overall Quality')

# Note that loess() outputs warning messages.  Can we guess why?

### OverallCond
loess.3 <- loess(log(SalePrice) ~ OverallCond,data=small.df);
lm.3 <- lm(log(SalePrice) ~ OverallCond,data=small.df);

plot(log(small.df$SalePrice) ~ small.df$OverallCond,xlab='Overall Condition',ylab='log(SalePrice)')
points(loess.3$x,loess.3$fitted,type='p',col='red',pch=19)
abline(coef=lm.3$coef,col='blue',lwd=2)
title('SLR - Overall Condition')

### TotalBathCalc
loess.4 <- loess(log(SalePrice) ~ TotalBathCalc,data=small.df);
lm.4 <- lm(log(SalePrice) ~ TotalBathCalc,data=small.df);

plot(log(small.df$SalePrice) ~ small.df$TotalBathCalc,xlab='Total Number of Bathrooms',ylab='log(SalePrice)')
points(loess.4$x,loess.4$fitted,type='p',col='red',pch=19)
abline(coef=lm.4$coef,col='blue',lwd=2)
title('SLR - Total Number of Bathrooms')

### QualityIndex
loess.5 <- loess(log(SalePrice) ~ QualityIndex,data=small.df);
lm.5 <- lm(log(SalePrice) ~ QualityIndex,data=small.df);

plot(log(small.df$SalePrice) ~ small.df$QualityIndex,xlab='Quality Index',ylab='log(SalePrice)')
points(loess.5$x,loess.5$fitted,type='p',col='red',pch=19)
abline(coef=lm.5$coef,col='blue',lwd=2)
title('SLR - Quality Index')

### TotRmsAbvGrd
loess.6 <- loess(log(SalePrice) ~ TotRmsAbvGrd,data=small.df);
lm.6 <- lm(log(SalePrice) ~ TotRmsAbvGrd,data=small.df);

plot(log(small.df$SalePrice) ~ small.df$TotRmsAbvGrd,xlab='Total Rooms Above Ground',ylab='log(SalePrice)')
points(loess.6$x,loess.6$fitted,type='p',col='red',pch=19)
abline(coef=lm.6$coef,col='blue',lwd=2)
title('SLR - Total Rooms Above Ground')


#####################################################################################
# Discrete or Continuous?
#####################################################################################

par(mfrow = c(3,2))

#Total Rooms Above Grade
plot(log(small.df$SalePrice) ~ small.df$TotRmsAbvGrd,xlab='Total Rooms Above Ground',ylab='log(SalePrice)')
points(loess.6$x,loess.6$fitted,type='p',col='red',pch=19)
abline(coef=lm.6$coef,col='blue',lwd=2)
title('SLR - Total Rooms Above Ground')
boxplot(small.df$SalePrice ~ small.df$TotRmsAbvGrd,col = "gray",xlab='Total Rooms Above Ground',ylab='SalePrice', main = "Total Rooms Above Grade BoxPlot")

#Overall Quality
plot(log(small.df$SalePrice) ~ small.df$OverallQual,xlab='Overall Quality',ylab='log(SalePrice)')
points(loess.2$x,loess.2$fitted,type='p',col='red',pch=19)
abline(coef=lm.2$coef,col='blue',lwd=2)
title('SLR - Overall Quality')
boxplot(small.df$SalePrice ~ small.df$OverallQual,col = "gray", xlab='Overall Quality',ylab='SalePrice', main = "Overall Quality BoxPlot")

#Overall Condition
plot(log(small.df$SalePrice) ~ small.df$OverallCond,xlab='Overall Condition',ylab='log(SalePrice)')
points(loess.3$x,loess.3$fitted,type='p',col='red',pch=19)
abline(coef=lm.3$coef,col='blue',lwd=2)
title('SLR - Overall Condition')
boxplot(small.df$SalePrice ~ small.df$OverallCond, col = "gray",xlab='Overall Condition',ylab='SalePrice', main = "Overall Condition BoxPlot")

###Knitr
install.packages("knitr",repos = "http://cran.us.r-project.org")
library(knitr)

install.packages("rmarkdown",repos = "http://cran.us.r-project.org")
library(rmarkdown)



#####################################################################################
# Assignment #2
#####################################################################################

#Question 1
par(mfrow = c(1,2))

#SLR with Total SF
SLR_loess_1a <- loess(SalePrice ~ TotalSqftCalc ,data=ames.df)
SLR_model_1a <- lm(SalePrice ~ TotalSqftCalc,data=ames.df)

plot(ames.df$SalePrice ~ ames.df$TotalSqftCalc,xlab='Total Square Footage', ylab='SalePrice')
points(SLR_loess_1a$x, SLR_loess_1a$fitted,type='p',col='red',pch=19)
abline(coef=SLR_model_1a$coef,col='blue',lwd=2)
title('SLR - Total Square Footage')

#SLR with Garage Area
SLR_loess_1b  <- loess(SalePrice ~  GarageArea ,data=ames.df)
SLR_model_1b <- lm(SalePrice ~ GarageArea, data=ames.df)

plot(ames.df$SalePrice ~ ames.df$GarageArea,xlab='Garage Area', ylab='SalePrice')
points(SLR_loess_1b$x, SLR_loess_1b$fitted,type='p',col='red',pch=19)
abline(coef=SLR_model_1b$coef,col='blue',lwd=2)
title('SLR - Garage Area')

#Plot model summaries with stargazer
install.packages('stargazer',dependencies=TRUE, repos = "http://cran.us.r-project.org")
library(MASS)
library(stargazer)
out.path <- '/Users/serrauzun/Desktop/MSDS_410_Supervised/Assignment #2/';

summary(SLR_model_1a)
summary(SLR_model_1b)

file.name_1 <- 'SLR Model Comparison.html';
stargazer(SLR_model_1a, SLR_model_1b, type=c('html'),out=paste(out.path,file.name_1,sep=''),
          title=c('Comparison of SLR Model #1 and Model #2'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, 
          column.labels=c('SLR Model #1','SLR Model #2'), intercept.bottom=FALSE )

#Coefficients
summary(SLR_model_1a)
SLR_model_1a$coefficients

SLR_model_1b$coefficients

#Diagnostic Plots
plot(SLR_model_1a)
plot(SLR_model_1b)


########################
#Question 2
MLR_model_1 <- lm(SalePrice ~ TotalSqftCalc + GarageArea + TotRmsAbvGrd + LotArea, data=ames.df)
MLR_summ_1 <- summary(MLR_model_1)
MLR_summ_1
plot(MLR_model_1)

file.name_2 <- 'MLR #1.html';
stargazer(MLR_model_1, type=c('html'),out=paste(out.path,file.name_2,sep=''),
          title=c('MLR Model #1 Results'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, 
          column.labels=c('MLR Model #1'), intercept.bottom=FALSE )

file.name_3 <- 'MLR #2.html';
stargazer(MLR_model_1,SLR_model_1a,SLR_model_1b, type=c('html'),out=paste(out.path,file.name_3,sep=''),
          title=c('MLR Model #1 Results & Comparison'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, 
          column.labels=c("MLR Model #1", "SLR Model #1", "SLR Model #2"), intercept.bottom=FALSE )

plot(MLR_model_1)

########################
#Question 3
MLR_model_2 <- lm(log(SalePrice) ~ TotalSqftCalc + GarageArea + TotRmsAbvGrd + LotArea, data=ames.df)
MLR_summ_2 <- summary(MLR_model_2)
plot(MLR_model_2)

file.name_4 <- 'MLR Question Results Comp.html';
stargazer(MLR_model_1, MLR_model_2, type=c('html'),out=paste(out.path,file.name_4,sep=''),
          title=c('Comparison of MLR Model #1 and Model #2'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, 
          column.labels=c('MLR Model #1','MLR Model #2'), intercept.bottom=FALSE )

#MSE for both MLR
mse.1 <- mean(MLR_model_1$residuals^2)
mae.1 <- mean(abs(MLR_model_1$residuals))

mse.2 <- mean((ames.df$SalePrice-exp(MLR_model_2$fitted.values))^2)
mae.2 <- mean(abs(ames.df$SalePrice-exp(MLR_model_2$fitted.values)))

mse.1
mae.1

mse.2
mae.2

mse.1/mse.2
mae.1/mae.2

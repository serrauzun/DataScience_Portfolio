#set directory and import data
getwd()
setwd('/Users/serrauzun/Desktop/MSDS_411_Unsupervised/Assignment #1')
install.packages("readxl",repos = "http://cran.us.r-project.org")
library(readxl)
install.packages("readxl",repos = "http://cran.us.r-project.org")
install.packages("tidyverse",repos = "http://cran.us.r-project.org")
install.packages("Rtsne",repos = "http://cran.us.r-project.org")

library(readxl)
library(tidyverse)
library(Rtsne)

Stock.Data <- read_excel("/Users/serrauzun/Desktop/MSDS_411_Unsupervised/Assignment #1/stockdata.xlsx")
raw.data <- read_excel("/Users/serrauzun/Desktop/MSDS_411_Unsupervised/Assignment #1/stockdata.xlsx")

str(raw.data)
head(raw.data)
names(raw.data)
dim(raw.data)

str(Stock.Data)
head(Stock.Data)
names(Stock.Data)
dim(Stock.Data)


# Note Date is a string of dd-Mon-yy in R this is '%d-%B-%y';
Stock.Data$RDate <- as.Date(Stock.Data$Date,'%d-%B-%y');
sorted.df <- Stock.Data[order(Stock.Data$RDate),];
head(sorted.df)

AA <- log(sorted.df$AA[-1]/sorted.df$AA[-dim(sorted.df)[1]]);
head(AA)
# Manually check the first entry: log(9.45/9.23)
# Type cast the array as a data frame;
returns.df <- as.data.frame(AA);
str(returns.df)
returns.df$BAC <- log(sorted.df$BAC[-1]/sorted.df$BAC[-dim(sorted.df)[1]]);
returns.df$BHI <- log(sorted.df$BHI[-1]/sorted.df$BHI[-dim(sorted.df)[1]]);
returns.df$CVX <- log(sorted.df$CVX[-1]/sorted.df$CVX[-dim(sorted.df)[1]]);
returns.df$DD  <- log(sorted.df$DD[-1]/sorted.df$DD[-dim(sorted.df)[1]]);
returns.df$DOW <- log(sorted.df$DOW[-1]/sorted.df$DOW[-dim(sorted.df)[1]]);
returns.df$DPS <- log(sorted.df$DPS[-1]/sorted.df$DPS[-dim(sorted.df)[1]]);
returns.df$GS  <- log(sorted.df$GS[-1]/sorted.df$GS[-dim(sorted.df)[1]]);
returns.df$HAL <- log(sorted.df$HAL[-1]/sorted.df$HAL[-dim(sorted.df)[1]]);
returns.df$HES <- log(sorted.df$HES[-1]/sorted.df$HES[-dim(sorted.df)[1]]);
returns.df$HON <- log(sorted.df$HON[-1]/sorted.df$HON[-dim(sorted.df)[1]]);
returns.df$HUN <- log(sorted.df$HUN[-1]/sorted.df$HUN[-dim(sorted.df)[1]]);
returns.df$JPM <- log(sorted.df$JPM[-1]/sorted.df$JPM[-dim(sorted.df)[1]]);
returns.df$KO  <- log(sorted.df$KO[-1]/sorted.df$KO[-dim(sorted.df)[1]]);
returns.df$MMM <- log(sorted.df$MMM[-1]/sorted.df$MMM[-dim(sorted.df)[1]]);
returns.df$MPC <- log(sorted.df$MPC[-1]/sorted.df$MPC[-dim(sorted.df)[1]]);
returns.df$PEP <- log(sorted.df$PEP[-1]/sorted.df$PEP[-dim(sorted.df)[1]]);
returns.df$SLB <- log(sorted.df$SLB[-1]/sorted.df$SLB[-dim(sorted.df)[1]]);
returns.df$WFC <- log(sorted.df$WFC[-1]/sorted.df$WFC[-dim(sorted.df)[1]]);
returns.df$XOM <- log(sorted.df$XOM[-1]/sorted.df$XOM[-dim(sorted.df)[1]]);
returns.df$VV  <- log(sorted.df$VV[-1]/sorted.df$VV[-dim(sorted.df)[1]]);
str(returns.df)

# Compute correlation matrix for returns;
returns.cor <- cor(returns.df)
corr_ <- as.data.frame(returns.cor[,c('VV')])


# Barplot the last column to visualize magnitude of correlations;
par(mfrow = c(1,1))
barplot(returns.cor[1:20,c('VV')],las=2,ylim=c(0,1.0), col = "light gray")
title('Correlations with VV')

# Make correlation plot for returns;
# If you need to install corrplot package;  Note how many dependencies this package has;
install.packages('corrplot', dependencies=TRUE, repos = "http://cran.us.r-project.org")

require(corrplot)
par(mfrow = c(1,1))
#corrplot(returns.cor, tl.cex = 0.8, tl.col = "black")
corrplot(returns.cor, tl.cex = 0.8, cl.cex = 1, tl.col = "black")

# load car package
require(car)

# Fit some model
model.1 <- lm(VV ~ GS+DD+DOW+HON+HUN+JPM+KO+MMM+XOM, data=returns.df)
summary(model.1)
vif(model.1)

# Fit the full model
model.2 <- lm(VV ~ BAC+GS+JPM+WFC+BHI+CVX+DD+DOW+DPS+HAL+HES+HON+HUN+KO+MMM+MPC+PEP+SLB+XOM,data=returns.df)
summary(model.2)
vif(model.2)

install.packages('stargazer',repos = "http://cran.us.r-project.org")
library(stargazer)



returns.pca <- princomp(x=returns.df[,-21],cor=TRUE)
# See the output components returned by princomp();
names(returns.pca)
 
# check the range of the components
range(pc.1)
range(pc.2)

dev.off()
plot(-10,10,type='p',xlim=c(0.10,0.27),ylim=c(-0.6,.27),xlab='PC 1',ylab='PC 2', main = "Loadings for PC1 and PC2")
text(pc.1,pc.2,labels=names(pc.1),cex=0.75, col = stock_data_industry$IndustryCode)

pc.1 <- returns.pca$loadings[,1];
pc.2 <- returns.pca$loadings[,2];
names(pc.1)



# Plot the default scree plot;
plot(returns.pca)
returns.pca

# Make Scree Plot
scree.values <- (returns.pca$sdev^2)/sum(returns.pca$sdev^2);
scree.values
plot(scree.values,xlab='Number of Components',ylab='',type='l',lwd=2)
points(scree.values,lwd=2,cex=1.5)
title('Scree Plot')


# Make Proportion of Variance Explained
variance.values <- cumsum(returns.pca$sdev^2)/sum(returns.pca$sdev^2);
variance.values
#80 percent of the variance is covered in first 8 components

plot(variance.values,xlab='Number of Components',ylab='',type='l',lwd=2)
points(variance.values,lwd=2,cex=1.5)
abline(h=0.8,lwd=1.5,col='red')
abline(v=8,lwd=1.5,col='red')
text(13,0.5,'Keep 8 Principal Components',col='red')
title('Total Variance Explained Plot')

# Create the data frame of PCA predictor variables;
return.scores <- as.data.frame(returns.pca$scores);
return.scores$VV <- returns.df$VV;
return.scores$u <- runif(n=dim(return.scores)[1],min=0,max=1);
head(return.scores)

# Split the data set into train and test data sets;
train.scores <- subset(return.scores,u<0.70);
test.scores <- subset(return.scores,u>=0.70);
dim(train.scores)
dim(test.scores)
dim(train.scores)+dim(test.scores)
dim(return.scores)

# Fit a linear regression model using the first 8 principal components;
pca1.lm <- lm(VV ~ Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+Comp.6+Comp.7+Comp.8, data=train.scores);
summary(pca1.lm)

install.packages('stargazer',dependencies=TRUE, repos = "http://cran.us.r-project.org")
library(MASS)
library(stargazer)
out.path <- '/Users/serrauzun/Desktop/MSDS_411_Unsupervised/Assignment #1/'

file.name <- 'pca1.lm .html';
stargazer(pca1.lm, type=c('html'),out=paste(out.path,file.name,sep=''),
          title=c('PCA Model 1'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, 
          column.labels=c("pca1.lm"), intercept.bottom=FALSE )

# Compute the Mean Absolute Error on the training sample;
pca1.mae.train <- mean(abs(train.scores$VV-pca1.lm$fitted.values));
vif(pca1.lm)

# Score the model out-of-sample and compute MAE;
pca1.test <- predict(pca1.lm,newdata=test.scores);
pca1.mae.test <- mean(abs(test.scores$VV-pca1.test));


pca1.mae.train
pca1.mae.test 

# Let's compare the PCA regression model with a 'raw' regression model;
# Create a train/test split of the returns data set to match the scores data set;
returns.df$u <- return.scores$u;
train.returns <- subset(returns.df,u<0.70);
test.returns <- subset(returns.df,u>=0.70);
dim(train.returns)
dim(test.returns)
dim(train.returns)+dim(test.returns)
dim(returns.df)


# Fit model.1 on train data set and 'test' on test data;
model.1_pca <- lm(VV ~ GS+DD+DOW+HON+HUN+JPM+KO+MMM+XOM, data=train.returns)
model1_pca.mae.train <- mean(abs(train.returns$VV-model.1_pca$fitted.values));
model1_pca.test <- predict(model.1_pca,newdata=test.returns);
model1_pca.mae.test <- mean(abs(test.returns$VV-model1_pca.test));


# Fit model.2 on train data set and 'test' on test data;
model.2_pca <- lm(VV ~ BAC+GS+JPM+WFC+BHI+CVX+DD+DOW+DPS+HAL+HES+HON+HUN+KO+MMM+MPC+PEP+SLB+XOM, data=train.returns)
model2_pca.mae.train <- mean(abs(train.returns$VV-model.2_pca$fitted.values));
model2_pca.test <- predict(model.2_pca,newdata=test.returns);
model2_pca.mae.test <- mean(abs(test.returns$VV-model2_pca.test));
summary(model.2)

file.name1 <- 'model 1 and 2.lm .html';
stargazer(model.1_pca,model.2_pca, type=c('html'),out=paste(out.path,file.name1,sep=''),
          title=c('PCA LM 1 & 2'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, 
          column.labels=c("PCA LM #1","PCA LM #2"), intercept.bottom=FALSE )

#MAE comparison 
pca1.mae.train
model1_pca.mae.train
model2_pca.mae.train

pca1.mae.test
model1_pca.mae.test
model2_pca.mae.test

# remove u

train.scores <- train.scores[c(-22)]

# Fit full.lm on PCA scores of train data
full.lm <- lm(VV ~ ., data=train.scores);
summary(full.lm)


library(MASS)
backward.lm <- stepAIC(full.lm,direction=c('backward'))
summary(backward.lm)
backward.mae.train <- mean(abs(train.scores$VV-backward.lm$fitted.values));

file.name2 <- 'backward.html';
stargazer(backward.lm, type=c('html'),out=paste(out.path,file.name2,sep=''),
          title=c('Backward Linear Model'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, 
          column.labels=c("Backward LM"), intercept.bottom=FALSE )

vif(backward.lm)

backward.test <- predict(backward.lm,newdata=test.scores);
backward.mae.test <- mean(abs(test.scores$VV-backward.test))

pca1.mae.train
model1_pca.mae.train
model2_pca.mae.train
backward.mae.train

pca1.mae.test
model1_pca.mae.test
model2_pca.mae.test
backward.mae.test 

#############
#############

# t-Distributed Stochastic Neighbor Embedding [t-SNE] 

stock_data_industry <- read_excel("/Users/serrauzun/Desktop/MSDS_411_Unsupervised/Assignment #1/stock_data_industry.xlsx")


###########################
# Data load and prep
###########################
return.df_tsne <- returns.df[,c(1:20)]
colnames(return.df_tsne)

tsne_transpose <- as.data.frame(t(as.matrix(return.df_tsne)))
dim(tsne_transpose)
colnames(tsne_transpose)

###########################
# Exploratory Data Analysis
# Data Prep
###########################

# review range of variables and ensure no N/As exist
summary(tsne_data)
sapply(tsne_transpose, function(x) sum(is.na(x)))


tsne_plot <- function(perpl=3,iterations=5000,learning=200){
  set.seed(1) # for reproducibility
  tsne <- Rtsne(tsne_transpose , dims = 2, perplexity=perpl, verbose=TRUE, max_iter=iterations, eta=learning)
  plot(tsne$Y, t='n', main = print(paste0("perplexity = ",perpl, ", max_iter = ",iterations, ", learning rate = ",learning)), xlab="tSNE dimension 1", ylab="tSNE dimension 2", cex.main=1, cex.lab=1.5)
  text(tsne$Y, labels = stock_data_industry$IndustryCode, col = stock_data_industry$IndustryCode)
}

par(mfrow = c(2,3))
perplexity_values <- c(1,2,3,4,5)
sapply(perplexity_values, function(i){tsne_plot(perpl=i)})

par(mfrow = c(2,3))
learning_values <- c(20,100,200,500,1000,2000)
sapply(learning_values,function(i){tsne_plot(learning=i)})

par(mfrow = c(1,1))
learning_values <- c(200)
sapply(learning_values,function(i){tsne_plot(learning=i)})

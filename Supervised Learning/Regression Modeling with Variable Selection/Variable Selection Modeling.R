# Serra Uzun
# MSDS_410 Supervised Learning Methods_FALL 2020
# 11.01.2020
# Assignment_07

getwd()
setwd("/Users/serrauzun/Desktop/MSDS_410_Supervised/Assignment #7")

ames.df <- readRDS("Ames_eligible_sample.Rdata")
dim(ames.df)
colnames(ames.df)
str(ames.df)

unique(ames.df$LotConfig)

ames.df$LandSlope <- as.factor(ames.df$LandSlope)
ames.df$LotConfig <- as.factor(ames.df$LotConfig)
colnames(ames.df)
str(ames.df)
sapply(ames.df, function(x) sum(is.na(x)))

# Set the seed on the random number generator so you get the same split every time that # you run the code.
set.seed(123)
ames.df$u <- runif(n=dim(ames.df)[1],min=0,max=1)
# Define these two variables for later use;
ames.df$QualityIndex <- ames.df$OverallQual * ames.df$OverallCond 
ames.df$TotalSqftCalc <- ames.df$BsmtFinSF1 + ames.df$BsmtFinSF2 + ames.df$GrLivArea
# Create train/test split;
train.df <- subset(ames.df, u<0.70)
test.df <- subset(ames.df, u>=0.70)
# Check your data split. The sum of the parts should equal the whole. # Do your totals add up?
dim(ames.df)[1]
dim(train.df)[1]
dim(test.df)[1] 
dim(train.df)[1] + dim(test.df)[1]

colnames(ames.df)

##
ames.df.small <- ames.df[,c(6, 12:13, 19:21, 40, 45:46, 48, 51:53, 56, 58, 63:64, 68:69, 82,86)]
colnames(ames.df.small)
var_pool <- as.data.frame(colnames(ames.df.small))
str(ames.df.small)

ames.small_cor <- cor(ames.df.small[sapply(ames.df.small, is.numeric)])
ames.small_cor

par(mfrow=c(1,1))
corrplot(ames.small_cor, method = "color", outline = T, addrect = 4, rect.col = "black", rect.lwd = 3,cl.pos = "b", tl.col = "black", tl.cex = 0.8, cl.cex = 0.8, addCoef.col = "black", number.digits = 1, number.cex = 0.60, col = colorRampPalette(c("darkblue","white","darkgreen"))(100))

train.clean <-train.df[,(names(ames.df.small))]
colnames(train.clean)
dim(train.clean )

sapply(train.clean, function(x) sum(is.na(x)))

# Define the upper model as the FULL model 
upper.lm <- lm(SalePrice ~ ., data=train.clean) 
summary(upper.lm)
# Define the lower model as the Intercept model 
lower.lm <- lm(SalePrice ~ 1, data=train.clean)
# Need a SLR to initialize stepwise selection
sqft.lm <- lm(SalePrice ~ TotalSqftCalc, data=train.clean); 
summary(sqft.lm)


# Note: There is only one function for classical model selection in R - stepAIC();
# stepAIC() is part of the MASS library.
# The MASS library comes with the BASE R distribution, but you still need to load it; 
library(stargazer)
library(MASS)
install.packages("Metrics")
library(Metrics)
# Call stepAIC() for variable selection
forward.lm <- stepAIC(object=lower.lm,scope=list(upper=formula(upper.lm),lower=~1),
                      direction=c('forward'))
summary(forward.lm)
mse.fwd <- mse(train.clean$SalePrice, predict(forward.lm, train.clean))
mae.fwd <- mae(predict(forward.lm), train.clean$SalePrice)

sqrt(mse.fwd)
mae.fwd

stepAIC(forward.lm)
out.path = "/Users/serrauzun/Desktop/MSDS_410_Supervised/Assignment #7"
frwd_lm <- 'Forward.html';
stargazer(forward.lm, type=c('html'),out=paste(out.path,frwd_lm,sep=''),
          title=c('Forward LM'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, 
          column.labels=c('Forward LM'), intercept.bottom=FALSE)


backward.lm <- stepAIC(object=upper.lm,direction=c('backward')) 
summary(backward.lm)

mse.bkwd <- mse(train.clean$SalePrice, predict(backward.lm, train.clean))
mae.bkwd <- mae(predict(backward.lm), train.clean$SalePrice)

sqrt(mse.bkwd)
mae.bkwd

stepAIC(backward.lm)
out.path = "/Users/serrauzun/Desktop/MSDS_410_Supervised/Assignment #7"
bckwrd_lm <- 'Backward.html';
stargazer(backward.lm, type=c('html'),out=paste(out.path,bckwrd_lm,sep=''),
          title=c('Backward LM'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, 
          column.labels=c('Backward LM'), intercept.bottom=FALSE)


stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1), direction=c('both'));
summary(stepwise.lm)

mse.step <- mse(train.clean$SalePrice, predict(stepwise.lm, train.clean))
mae.step <- mae(predict(stepwise.lm), train.clean$SalePrice)

sqrt(mse.step)
mae.step

stepAIC(stepwise.lm)
out.path = "/Users/serrauzun/Desktop/MSDS_410_Supervised/Assignment #7"
stpws_lm <- 'Stepwise.html';
stargazer(stepwise.lm, type=c('html'),out=paste(out.path,stpws_lm,sep=''),
          title=c('Stepwise LM'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE, 
          column.labels=c('Steowise LM'), intercept.bottom=FALSE)


junk.lm <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea + TotalSqftCalc, data=train.df)
summary(junk.lm)
stepAIC(junk.lm)
AIC(junk.lm)

# Compute the VIF values
library(car) 
sort(vif(forward.lm),decreasing=TRUE) 
sort(vif(backward.lm),decreasing=TRUE) 
sort(vif(stepwise.lm),decreasing=TRUE)
sort(vif(junk.lm),decreasing=TRUE)

AIC(forward.lm)
AIC(backward.lm)
AIC(stepwise.lm)

BIC(forward.lm)
BIC(backward.lm)
BIC(stepwise.lm)



###Predict
forward.test <- predict(forward.lm,newdata=test.df)
backward.test <- predict(backward.lm,newdata=test.df)
stepwise.test <- predict(stepwise.lm,newdata=test.df)
#junk.test <- predict(junk.lm,newdata=test.df)

summary(forward.test)

mse.fwd_test <- mse(test.df$SalePrice, predict(forward.lm,newdata=test.df))
mae.fwd_test <- mae(predict(forward.lm,newdata=test.df), test.df$SalePrice)
sqrt(mse.fwd_test)
mae.fwd_test

mse.bck_test <- mse(test.df$SalePrice, predict(backward.lm,newdata=test.df))
mae.bck_test <- mae(predict(backward.lm,newdata=test.df), test.df$SalePrice)
sqrt(mse.bck_test)
mae.bck_test

mse.stp_test <- mse(test.df$SalePrice, predict(stepwise.lm,newdata=test.df))
mae.stp_test <- mae(predict(stepwise.lm,newdata=test.df), test.df$SalePrice)
sqrt(mse.stp_test)
mae.stp_test


# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean$SalePrice
backward.pct <- abs(backward.lm$residuals)/train.clean$SalePrice
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$SalePrice
# Assign Prediction Grades;
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0,0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]', 
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]', 
                                                'Grade 4: (0.25+]')))
backward.PredictionGrade <- ifelse(backward.pct<=0.10,'Grade 1: [0,0.10]',
                                  ifelse(backward.pct<=0.15,'Grade 2: (0.10,0.15]', 
                                         ifelse(backward.pct<=0.25,'Grade 3: (0.15,0.25]', 
                                                'Grade 4: (0.25+]')))
stepwise.PredictionGrade <- ifelse(stepwise.pct<=0.10,'Grade 1: [0,0.10]',
                                  ifelse(stepwise.pct<=0.15,'Grade 2: (0.10,0.15]', 
                                         ifelse(stepwise.pct<=0.25,'Grade 3: (0.15,0.25]', 
                                                'Grade 4: (0.25+]')))

forward.trainTable <- table(forward.PredictionGrade) 
backward.trainTable <- table(backward.PredictionGrade) 
stepwise.trainTable <- table(stepwise.PredictionGrade) 

forward.trainTable/sum(forward.trainTable)
backward.trainTable/sum(backward.trainTable)
stepwise.trainTable/sum(stepwise.trainTable)

# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.df$SalePrice-forward.test)/test.df$SalePrice
backward.testPCT <- abs(test.df$SalePrice-backward.test)/test.df$SalePrice
stepwise.testPCT <- abs(test.df$SalePrice-stepwise.test)/test.df$SalePrice


# Assign Prediction Grades;
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]', 
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
                                      ))
backward.testPredictionGrade <- ifelse(backward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(backward.testPCT<=0.15,'Grade 2: (0.10,0.15]', 
                                             ifelse(backward.testPCT<=0.25,'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
                                      ))
stepwise.testPredictionGrade <- ifelse(stepwise.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(stepwise.testPCT<=0.15,'Grade 2: (0.10,0.15]', 
                                             ifelse(stepwise.testPCT<=0.25,'Grade 3: (0.15,0.25]', 'Grade 4: (0.25+]')
                                      ))

forward.testTable <-table(forward.testPredictionGrade) 
backward.testTable <-table(backward.testPredictionGrade) 
stepwise.testTable <-table(stepwise.testPredictionGrade) 

forward.testTable/sum(forward.testTable)
backward.testTable/sum(backward.testTable)
stepwise.testTable/sum(stepwise.testTable)



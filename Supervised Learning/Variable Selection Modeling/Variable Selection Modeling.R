getwd()
setwd("/Users/serrauzun/Desktop/MSDS_410_Supervised/Assignment #7")

ames.df <- readRDS("Ames_eligible_sample.Rdata")
dim(ames.df)
colnames(ames.df)
unique(ames.df$dropCondition)

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

str(train.df)
num_cols <- unlist(lapply(train.df,is.numeric))
num_cols
train.df_num <- train.df[,num_cols]
train.df_num
dim(train.df_num)

install.packages("corrplot")
library(corrplot)
train_cor <- cor(train.df_num)
train_cor 

str(train.df)


corrplot(train_cor , method = "color", outline = T, addrect = 4, rect.col = "black", rect.lwd = 3,cl.pos = "b", tl.col = "black", tl.cex = 1, cl.cex = 1, addCoef.col = "black", number.digits = 1, number.cex = 0.60, col = colorRampPalette(c("darkred","white","darkgreen"))(100))

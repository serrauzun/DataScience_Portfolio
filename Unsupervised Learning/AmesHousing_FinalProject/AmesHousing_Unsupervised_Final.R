library(corrplot)
library(MASS)
library(standardize)
library(ggplot2)
library(cluster)
library(psych)
library("factoextra")
ames.df <- ames_housing_data_copy

dim(ames.df)
colnames(ames.df)
str(ames.df)

sapply(ames.df, function(x) sum(is.na(x)))

# SAMPLE 
ames.df$dropCondition <- ifelse(ames.df$BldgType!='1Fam','01: Not SFR',
                                ifelse(ames.df$SaleCondition!='Normal','02: Non-Normal Sale',
                                              ifelse(ames.df$YearBuilt <1970,'03: Built Pre-1970',
                                                                   ifelse(ames.df$Utilities!='AllPub','04: Not Public Utilities',
                                                                          ifelse(ames.df$TotalBsmtSF <1,'05: No Basement',
                                                                                 ifelse(ames.df$GarageArea <1,'06: No Garage',
                                                                          '99: Eligible Sample')
                                                            )))))
# Save the table
waterfall <- table(ames.df$dropCondition)
as.matrix(waterfall,7,1)
# Eliminate all observations that are not part of the eligible sample population;
eligible.population <- subset(ames.df,dropCondition=='99: Eligible Sample');
table(eligible.population$dropCondition)

eligible.population_final <- eligible.population[,c("LotArea","TotalBsmtSF","FirstFlrSF","SecondFlrSF",
                                                    "WoodDeckSF","GarageArea","OpenPorchSF",
                                                    "FullBath","HalfBath","BedroomAbvGr","TotRmsAbvGrd",
                                                    "BsmtFullBath","BsmtHalfBath","YearBuilt","SalePrice")]

sapply(eligible.population_final, function(x) sum(is.na(x)))
colnames(eligible.population_final)
eligible.population_final$TotAbvGrdSF <-  eligible.population_final$FirstFlrSF + eligible.population_final$SecondFlrSF
eligible.population_final$FirstFlrSF  <- NULL
eligible.population_final$SecondFlrSF<- NULL
ames.df_clean <- eligible.population_final
colnames(ames.df_clean)
#EXPLORATORY DATA ANALYSIS
par(mfrow = c(1,1))
corrplot(cor(ames.df_clean), method = "color", outline = T, addrect = 4, rect.col = "black", rect.lwd = 1,cl.pos = "b", tl.col = "black", tl.cex = 1, cl.cex = 1, addCoef.col = "black", number.digits = 1, number.cex = 1, col = colorRampPalette(c("#F3B54A","white","#164C64"))(100))

par(mar=c(7,5,1,1))
multi.hist(ames.df_clean)
hist(ames.df_clean$SalePrice, col = '#0294C7')

ggplot(ames.df_clean, aes(x=SalePrice, y=TotAbvGrdSF, colour = YearBuilt)) + 
  geom_point() + 
  ggtitle("Scatter Plot SalePrice vs. TotAbvGrdSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(ames.df_clean, aes(x=SalePrice, y=LotArea, colour = YearBuilt)) + 
  geom_point() + 
  ggtitle("Scatter Plot SalePrice vs. LotArea") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

###BOXPLOTS
#response variable
par(mar=c(5,3,1,1))
par(mfrow = c(1,1))
boxplot(ames.df_clean$SalePrice, xlab = "SalePrice", col = "#0294C7")
#area related variables
par(mfrow = c(1,6))
boxplot(ames.df_clean$LotArea, xlab = "Lot Area", col = "#F3B54A")
boxplot(ames.df_clean$TotalBsmtSF, xlab = "Total Basement Area", col = "#F3B54A")
boxplot(ames.df_clean$TotAbvGrdSF, xlab = "Total Above Grade Area", col = "#F3B54A")
boxplot(ames.df_clean$WoodDeckSF, xlab = "Wood Deck Area", col = "#F3B54A")
boxplot(ames.df_clean$GarageArea, xlab = "Garage Area", col = "#F3B54A")
boxplot(ames.df_clean$OpenPorchSF, xlab = "Open Porch Area", col = "#F3B54A")

#room count related variables
par(mfrow = c(2,4))
boxplot(ames.df_clean$FullBath, xlab = "Full Bath",col = "#6E7889")
boxplot(ames.df_clean$HalfBath, xlab = "Half Bath", col = "#6E7889")
boxplot(ames.df_clean$BedroomAbvGr, xlab = "Bedroom Above Grade", col = "#6E7889")
boxplot(ames.df_clean$TotRmsAbvGrd, xlab = "Total Rooms Above Grade", col = "#6E7889")
boxplot(ames.df_clean$BsmtFullBath, xlab = "Basement Full Bath", col = "#6E7889")
boxplot(ames.df_clean$BsmtHalfBath, xlab = "Basement Half Bath", col = "#6E7889")
###
summary(ames.df_clean)

#find Q1, Q3, and interquartile range for values in column A
Q1_SalePrice <- quantile(ames.df_clean$SalePrice, .25)
Q3_SalePrice <- quantile(ames.df_clean$SalePrice, .75)
IQR_SalePrice <- IQR(ames.df_clean$SalePrice)

Q1_LotArea <- quantile(ames.df_clean$LotArea, .25)
Q3_LotArea <- quantile(ames.df_clean$LotArea, .75)
IQR_LotArea <- IQR(ames.df_clean$LotArea)

Q1_WoodDeck <- quantile(ames.df_clean$WoodDeckSF, .25)
Q3_WoodDeck <- quantile(ames.df_clean$WoodDeckSF, .75)
IQR_WoodDeck <- IQR(ames.df_clean$WoodDeckSF)

Q1_TotAbvGrd <- quantile(ames.df_clean$TotAbvGrdSF, .25)
Q3_TotAbvGrd <- quantile(ames.df_clean$TotAbvGrdSF, .75)
IQR_TotAbvGrd <- IQR(ames.df_clean$TotAbvGrdSF)

Q1_BsmtArea <- quantile(ames.df_clean$TotalBsmtSF, .25)
Q3_BsmtArea <- quantile(ames.df_clean$TotalBsmtSF, .75)
IQR_BsmtArea <- IQR(ames.df_clean$TotalBsmtSF)

Q1_OpenPorch <- quantile(ames.df_clean$OpenPorchSF, .25)
Q3_OpenPorch <- quantile(ames.df_clean$OpenPorchSF, .75)
IQR_OpenPorch <- IQR(ames.df_clean$OpenPorchSF)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
no_outliers1 <- subset(ames.df_clean, ames.df_clean$SalePrice> (Q1_SalePrice - 1.5*IQR_SalePrice) & ames.df_clean$SalePrice< (Q3_SalePrice + 1.5*IQR_SalePrice))
no_outliers2 <- subset(no_outliers1, no_outliers1$LotArea> (Q1_LotArea - 1.5*IQR_LotArea) & no_outliers1$LotArea< (Q3_LotArea + 1.5*IQR_LotArea))
no_outliers3 <- subset(no_outliers2, no_outliers2$WoodDeckSF> (Q1_WoodDeck - 1.5*IQR_WoodDeck) & no_outliers2$WoodDeckSF< (Q3_WoodDeck + 1.5*IQR_WoodDeck))
no_outliers4 <- subset(no_outliers3, no_outliers3$TotAbvGrdSF> (Q1_TotAbvGrd - 1.5*IQR_TotAbvGrd) & no_outliers3$TotAbvGrdSF< (Q3_TotAbvGrd + 1.5*IQR_TotAbvGrd))
no_outliers5 <- subset(no_outliers4, no_outliers4$TotalBsmtSF> (Q1_BsmtArea - 1.5*IQR_BsmtArea) & no_outliers4$TotalBsmtSF< (Q3_BsmtArea + 1.5*IQR_BsmtArea))
no_outliers6 <- subset(no_outliers5, no_outliers5$OpenPorchSF> (Q1_OpenPorch - 1.5*IQR_OpenPorch) & no_outliers5$OpenPorchSF< (Q3_OpenPorch + 1.5*IQR_OpenPorch))

dim(ames.df_clean)
#view row and column count of new data frame
dim(no_outliers6) 

par(mar=c(5,3,1,1))
par(mfrow = c(1,1))
boxplot(no_outliers6$SalePrice, xlab = "SalePrice", col = "#0294C7")
#area related variables
par(mfrow = c(1,6))
boxplot(no_outliers6$LotArea, xlab = "Lot Area", col = "#F3B54A")
boxplot(no_outliers6$TotalBsmtSF, xlab = "Total Basement Area", col = "#F3B54A")
boxplot(no_outliers6$TotAbvGrdSF, xlab = "Total Above Grade Area", col = "#F3B54A")
boxplot(no_outliers6$WoodDeckSF, xlab = "Wood Deck Area", col = "#F3B54A")
boxplot(no_outliers6$GarageArea, xlab = "Garage Area", col = "#F3B54A")
boxplot(no_outliers6$OpenPorchSF, xlab = "Open Porch Area", col = "#F3B54A")
#room count related variables
par(mar=c(5,3,3,3))
par(mfrow = c(2,3))
boxplot(no_outliers6$FullBath, xlab = "Full Bath",col = "#6E7889")
boxplot(no_outliers6$HalfBath, xlab = "Half Bath", col = "#6E7889")
boxplot(no_outliers6$BedroomAbvGr, xlab = "Bedroom Above Grade", col = "#6E7889")
boxplot(no_outliers6$TotRmsAbvGrd, xlab = "Total Rooms Above Grade", col = "#6E7889")
boxplot(no_outliers6$BsmtFullBath, xlab = "Basement Full Bath", col = "#6E7889")
boxplot(no_outliers6$BsmtHalfBath, xlab = "Basement Half Bath", col = "#6E7889")

par(mfrow = c(1,1))
corrplot(cor(no_outliers6), method = "color", outline = T, addrect = 4, rect.col = "black", rect.lwd = 1,cl.pos = "b", tl.col = "black", tl.cex = 1, cl.cex = 1, addCoef.col = "black", number.digits = 1, number.cex = 1.2, col = colorRampPalette(c("#F3B54A","white","#164C64"))(100))

colnames(no_outliers6)
str(no_outliers6)

#NORMALIZE
ames.df_Final <- as.data.frame(scale(no_outliers6))
summary(ames.df_Final)
colnames(ames.df_Final)

### PCA
ames.pca <- princomp(x=ames.df_Final[,-13],cor=FALSE)
names(ames.pca)

pc.1_scores <- ames.pca$scores[,1];
pc.2_scores <- ames.pca$scores[,2];
pc.3_scores <- ames.pca$scores[,3];
pc.4_scores <- ames.pca$scores[,4];
pc.5_scores <- ames.pca$scores[,5];
pc.6_scores <- ames.pca$scores[,6];
pc.7_scores <- ames.pca$scores[,7];
str(pc.1_scores)
pcdf = data.frame(pc1=pc.1_scores, pc2=pc.2_scores, pc3=pc.3_scores,pc4=pc.4_scores, pc5=pc.5_scores, pc6=pc.6_scores,pc7=pc.7_scores )
pcdf = cbind(pcdf,no_outliers6$SalePrice, no_outliers6$YearBuilt)
str(pcdf)
pcdf

ggplot(pcdf, aes(x=pc.1_scores, y=pc.2_scores, colour = pcdf$`no_outliers6$SalePrice`, label = pcdf$`no_outliers6$SalePrice`)) + 
  geom_point()  +
  ggtitle("Scatter Plot PC1 vs PC2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

par(mfrow = c(3,4))
ggplot(pcdf, aes(x=pc.1_scores, y=pc.2_scores, colour = pcdf$`no_outliers6$SalePrice`, label = pcdf$`no_outliers6$SalePrice`)) + 
  geom_point() + geom_text(aes(label=pcdf$`no_outliers6$YearBuilt`),hjust=0, vjust=0) +
  ggtitle("Scatter Plot PC1 vs PC2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(pcdf, aes(x=pc.1_scores, y=pc.3_scores, colour = pcdf$`no_outliers6$SalePrice`, label = pcdf$`no_outliers6$SalePrice`)) + 
  geom_point() + geom_text(aes(label=pcdf$`no_outliers6$YearBuilt`),hjust=0, vjust=0) +
  ggtitle("Scatter Plot PC1 vs PC2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(pcdf, aes(x=pc.1_scores, y=pc.4_scores, colour = pcdf$`no_outliers6$SalePrice`, label = pcdf$`no_outliers6$SalePrice`)) + 
  geom_point() + geom_text(aes(label=pcdf$`no_outliers6$YearBuilt`),hjust=0, vjust=0) +
  ggtitle("Scatter Plot PC1 vs PC2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(pcdf, aes(x=pc.1_scores, y=pc.5_scores, colour = pcdf$`no_outliers6$SalePrice`, label = pcdf$`no_outliers6$SalePrice`)) + 
  geom_point() + geom_text(aes(label=pcdf$`no_outliers6$YearBuilt`),hjust=0, vjust=0) +
  ggtitle("Scatter Plot PC1 vs PC2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(pcdf, aes(x=pc.1_scores, y=pc.6_scores, colour = pcdf$`no_outliers6$SalePrice`, label = pcdf$`no_outliers6$SalePrice`)) + 
  geom_point() + geom_text(aes(label=pcdf$`no_outliers6$YearBuilt`),hjust=0, vjust=0) +
  ggtitle("Scatter Plot PC1 vs PC2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(pcdf, aes(x=pc.1_scores, y=pc.7_scores, colour = pcdf$`no_outliers6$SalePrice`, label = pcdf$`no_outliers6$SalePrice`)) + 
  geom_point() + geom_text(aes(label=pcdf$`no_outliers6$YearBuilt`),hjust=0, vjust=0) +
  ggtitle("Scatter Plot PC1 vs PC2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ames.scores <- as.data.frame(ames.pca$scores);
ames.scores$SalePrice <- no_outliers6$SalePrice

##
pc.1_loadings <- ames.pca$loadings[,1]
pc.2_loadings <- ames.pca$loadings[,2]
pc.3_loadings <- ames.pca$loadings[,3]
pc.4_loadings <- ames.pca$loadings[,4]
pc.5_loadings <- ames.pca$loadings[,5]
pc.6_loadings <- ames.pca$loadings[,6]
pc.7_loadings <- ames.pca$loadings[,7]

as.data.frame(cbind(pc.1_loadings, pc.2_loadings, pc.3_loadings, pc.4_loadings, pc.5_loadings, pc.6_loadings, pc.7_loadings))

eig.val <- get_eigenvalue(ames.pca)
eig.val

library(dplyr)
pcdf1 = data.frame(pc1=pc.1_loadings, pc2=pc.2_loadings)
dim(pcdf1)
head(pcdf1)
colnames(pcdf1)
pcdf1 = cbind(pcdf1,no_outliers6$SalePrice, no_outliers6$YearBuilt)
pcdf1 

ggplot(pcdf1, aes(x=pc.1_loadings, y=pc.2_loadings, labels=rownames(PCAloadings))) + 
  geom_point()  +
  ggtitle("Scatter Plot PC1 vs PC2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

dev.off()
par(mar=c(5,3,3,3))
plot(-10,10,type='p',xlim=c(0.6,-0.6),ylim=c(0.6,-0.6),xlab='PC 1',ylab='PC 2', main = "Loadings for PC1 and PC2")
text(pc.1_loadings,pc.2_loadings,cex=0.75)

ggplot(pcdf, aes(x=pc.1_scores, y=pc.2_scores, colour = pcdf$`no_outliers6$SalePrice`, label = pcdf$`no_outliers6$SalePrice`)) + 
  geom_point()  +
  ggtitle("Scatter Plot PC1 vs PC2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

scree.values
scree.values <- (ames.pca$sdev^2)/sum(ames.pca$sdev^2);
scree.values
plot(scree.values,xlab='Number of Components',ylab='',type='l',lwd=1, lty = 3)
points(scree.values,lwd=1,cex=1.5)
title('Scree Plot')

variance.values <- cumsum(ames.pca$sdev^2)/sum(ames.pca$sdev^2);
variance.values

#80 percent of the variance is covered in first 7 components
plot(variance.values,xlab='Number of Components',ylab='',type='l',lwd=1)
points(variance.values,lwd=1,cex=1.5)
abline(h=0.8288186,lwd=1,col='#0294C7', lty = 2)
abline(v=7,lwd=1,col='#0294C7', lty = 2)
text(11,0.7,'Keep 7 Principal Components',col='#0294C7')
title('Total Variance Explained Plot')

ames.scores <- as.data.frame(ames.pca$scores);
ames.scores$SalePrice <- ames.df$SalePrice
ames.scores$u <- runif(n=dim(ames.scores)[1],min=0,max=1);
head(ames.scores)

### EFA
is.matrix(ames.df_Final)
isSymmetric(ames.df_Final)

dim(ames.df_Final)
fa.parallel(ames.df_Final, n.obs = 801, fa = "both", n.iter = 100, show.legend = TRUE, main = 'Scree Plots with Parallel Analysis')

fa_varimax <- fa(ames.df_Final, nfactors=5, rotate ="varimax", fm ="ml")
fa_varimax

fs <- factor.scores(ames.df_Final, fa_varimax)
fs <- fs$scores
ames.df_combined <- cbind(ames.df_Final,fs) 

EFA_cor <- cor(ames.df_combined)
corrplot(EFA_cor, method = "color", outline = T, addrect = 4, rect.col = "black", rect.lwd = 1,cl.pos = "b", tl.col = "black", tl.cex = 1, cl.cex = 1, addCoef.col = "black", number.digits = 1, number.cex = 0.90, col = colorRampPalette(c("#F3B54A","white","#164C64"))(100))

colnames(ames.df_Final)
ames.df_Final <- ames.df_Final[,-9]

### k-Means
subdat <- ames.df_Final
wssplot <- function(subdat, nc=20, seed=1234) {
  wss <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(subdat, centers=i)$withinss)}
  rs <- (wss[1] - wss)/wss[1]
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  plot(1:nc, rs, type="b", xlab="Number of Clusters",
       ylab="% of Between SS")} 

wssplot(subdat)
colnames(ames.df_Final)

#with clean df 
ames.df_clus <- ames.df_Final
colnames(ames.df_clus)
clusterresults <- kmeans(ames.df_clus,2)
names(clusterresults)
BetSSPer <- clusterresults$betweenss/clusterresults$totss
BetSSPer
clusterresults$totss
clusterresults
par(mar=c(5,3,2,1))
par(mfrow = c(1,1))
clusplot(ames.df_Final, clusterresults$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
clusterresults

dev.off()
par(mar=c(5,3,1,1))
par(mfrow = c(2,2))
plot(ames.df_Final[c("SalePrice","TotAbvGrdSF")], col = clusterresults$cluster, xlab = "SalePrice", ylab = "Total Above Grade SF")
plot(ames.df_Final[c("SalePrice","TotRmsAbvGrd")], col = clusterresults$cluster)
plot(ames.df_Final[c("SalePrice","LotArea")], col = clusterresults$cluster)
plot(ames.df_Final[c("SalePrice","YearBuilt")],col = clusterresults$cluster )

###
k3 <- kmeans(ames.df_clus, centers = 3, nstart = 25)
k4 <- kmeans(ames.df_clus, centers = 4, nstart = 25)
k5 <- kmeans(ames.df_clus, centers = 5, nstart = 25)
k6 <- kmeans(ames.df_clus, centers = 6, nstart = 25)
k7 <- kmeans(ames.df_clus, centers = 7, nstart = 25)
k8 <- kmeans(ames.df_clus, centers = 8, nstart = 25)
k9 <- kmeans(ames.df_clus, centers = 9, nstart = 25)
k10 <- kmeans(ames.df_clus, centers = 10, nstart = 25)

par(mar=c(2,2,2,2))
par(mfrow = c(2,3))
plot(ames.df_Final[c("SalePrice","TotAbvGrdSF")], col = k3$cluster, xlab = "SalePrice", main = "k=3",cex.main=2)
plot(ames.df_Final[c("SalePrice","TotAbvGrdSF")], col = k4$cluster, xlab = "SalePrice", main = "k=4",cex.main=2)
plot(ames.df_Final[c("SalePrice","TotAbvGrdSF")], col = k5$cluster, xlab = "SalePrice", main = "k=5",cex.main=2)
plot(ames.df_Final[c("SalePrice","TotAbvGrdSF")], col = k6$cluster, xlab = "SalePrice", main = "k=6",cex.main=2)
plot(ames.df_Final[c("SalePrice","TotAbvGrdSF")], col = k7$cluster, xlab = "SalePrice", main = "k=7",cex.main=2)
plot(ames.df_Final[c("SalePrice","TotAbvGrdSF")], col = k8$cluster, xlab = "SalePrice", main = "k=8",cex.main=2)

par(mar=c(2,2,2,2))
par(mfrow = c(2,3))
plot(ames.df_Final[c("SalePrice","LotArea")], col = k3$cluster, xlab = "SalePrice", main = "k=3",cex.main=2)
plot(ames.df_Final[c("SalePrice","LotArea")], col = k4$cluster, xlab = "SalePrice", main = "k=4",cex.main=2)
plot(ames.df_Final[c("SalePrice","LotArea")], col = k5$cluster, xlab = "SalePrice", main = "k=5",cex.main=2)
plot(ames.df_Final[c("SalePrice","LotArea")], col = k6$cluster, xlab = "SalePrice", main = "k=6",cex.main=2)
plot(ames.df_Final[c("SalePrice","LotArea")], col = k7$cluster, xlab = "SalePrice", main = "k=7",cex.main=2)
plot(ames.df_Final[c("SalePrice","LotArea")], col = k8$cluster, xlab = "SalePrice", main = "k=8",cex.main=2)

#with PCDF
pcdf_new <- pcdf[,c(1,2)]
colnames(pcdf_new)
head(pcdf_new)
clusterresultspc <- kmeans(pcdf_new,5)
names(clusterresultspc)
BetSSPer_pc <- clusterresultspc$betweenss/clusterresultspc$totss
BetSSPer_pc
clusterresultspc$totss
clusplot(pcdf, clusterresultspc$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
clusterresultspc




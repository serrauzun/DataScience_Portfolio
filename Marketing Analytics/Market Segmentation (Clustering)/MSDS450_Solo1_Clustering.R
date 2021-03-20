getwd()
setwd("/Users/serrauzun/Desktop/MSDS_450_Marketing/Solo1")
load("apphappyData.RData")
ls()

#bringing all necessary libraries
require(cluster)
require(useful)
require(Hmisc)
library(HSAUR)
library(MVA)
library(HSAUR2)
library(fpc)
library(mclust)
library(lattice)
library(car)

numdata <- apphappy.3.num.frame
dev.off()

#Exploratory Data Analysis
str(numdata)
head(numdata)
tail(numdata)
summary(numdata)
colnames(numdata)

a=table(numdata$q1)
names(a)

par(mfrow=c(2,3))
barplot(table(numdata$q1), col = "dark gray", xlab = "# of Observations", ylab = "Response", main = "Q1 - Age")
barplot(table(numdata$q48), col = "dark gray", xlab = "# of Observations", ylab = "Response", main = "Q48 - Education")
barplot(table(numdata$q49), col = "dark gray", xlab = "# of Observations", ylab = "Response", main = "Q49 - Marital Status")
barplot(table(numdata$q54), col = "dark gray", xlab = "# of Observations", ylab = "Response", main = "Q54 - Race")
barplot(table(numdata$q56), col = "dark gray", xlab = "# of Observations", ylab = "Response", main = "Q56 - Income")
barplot(table(numdata$q57), col = "dark gray", xlab = "# of Observations", ylab = "Response", main = "Q57 - Gender")


library(plyr) 
temp <- count(numdata, c('numdata$q1','numdata$q2r1')) 
str(temp)

b=table(numdata$q1,numdata$q2r1)
barplot(b)

hist(numdata$q1)
hist(numdata$q2r1)

#Creating subsets of responses relevant to our analysis
colnames(numdata)
numsub <- subset(numdata, select=c("q24r1","q24r2","q24r3","q24r4","q24r5",
                                   "q24r6","q24r7","q24r8","q24r9","q24r10",
                                   "q24r11","q24r12","q25r1","q25r2","q25r3","q25r4",
                                   "q25r5","q25r6","q25r7","q25r8","q25r9","q25r10",
                                   "q25r11","q25r12","q26r18","q26r3","q26r4","q26r5",
                                   "q26r6","q26r7","q26r8","q26r9","q26r10","q26r11",
                                   "q26r12","q26r13","q26r14","q26r15","q26r16","q26r17"))

str(numsub)
summary(numsub)
head(numsub)
attach(numsub)

#calculating and plotting correlation matrix
require(corrplot)
numsubcorrelation <- cor(numsub)
corrplot(numsubcorrelation)

mcor <- cor(numsub)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black")

corrplot(numsubcorrelation, method="shade", addCoef.col="black", 
addCoefasPercent=TRUE ,type="lower", shade.col=NA, tl.col="black", 
tl.srt=45, addcolorlabel="no", order="AOE",insig = "p-value")

#Creatnig a 'scree' plot to determine the number of clusters
dev.off()
wssplot <- function(numsub, nc=15, seed=1234) {
  wss <- (nrow(numsub)-1)*sum(apply(numsub,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(numsub, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")} 

wssplot(numsub)


#k-Means Clustering with 5 clusters
clusterresults <- kmeans(numsub,5)
names(clusterresults)
clusterresults$withinss
clusterresults$tot.withinss
clusterresults$totss
clusterresults$betweenss
clusterresults$size
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare

#Creating a PC (Principal Component plot)

plot(clusterresults, data=numsub)
clusterresults$centers
head(clusterresults$cluster)

#Creating silhouette plot
dev.off()
dissE <- daisy(numsub)
names(dissE)
dE2   <- dissE^2
sk2   <- silhouette(clusterresults$cluster, dE2)
str(sk2)
plot(sk2)

newdf <- as.data.frame(clusterresults$cluster)
write.csv(newdf, file = "clusterresults.csv")
write.csv(numsub, file = "numsub.csv")

#Creating a dataset with the original data with the cluster info
newdf <- read.csv("clusterresults.csv")
combdata <- cbind(numsub,newdf,numdata$q1)
head(combdata)

require(reshape)
combdata <- rename(combdata, c(clusterresults.cluster="cluster"))
head(combdata)

aggregate(combdata,by=list(byvar=combdata$cluster), mean)

#Hierarchical Clustering
numsub.dist = dist(numsub)
require(maptree)
hclustmodel <- hclust(dist(numsub), method = 'complete')
plot(hclustmodel)

cut.5 <- cutree(hclustmodel, k=5)
plot(silhouette(cut.5,numsub.dist))
head(cut.5)

write.csv(cut.5, file = "cut5results.csv")

require(proxy)
numsubmat <- as.matrix(numsub)
overallmean <- matrix(apply(numsubmat,2,mean),nrow=1)
overallmean
TSS <- sum(dist(numsubmat,overallmean)^2)
TSS

combcutdata <- cbind(numsub,cut.5)
head(combcutdata)

require(reshape)
combcutdata <- rename(combcutdata, c(cut.5="cluster"))
head(combcutdata)

clust1 <- subset(combcutdata, cluster == 1)
clust1 <- subset(clust1, select=c("q24r1","q24r2","q24r3","q25r1","q25r2","q25r3",
                                  "q26r3","q26r4","q26r5"))
clust1 <- as.matrix(clust1,rowby=T)
clust1mean <- matrix(apply(clust1,2,mean),nrow=1)
dis1 <- sum(dist(clust1mean,clust1)^2)

clust2 <- subset(combcutdata, cluster == 2)
clust2 <- subset(clust2, select=c("q24r1","q24r2","q24r3","q25r1","q25r2","q25r3",
                                  "q26r3","q26r4","q26r5"))
clust2 <- as.matrix(clust2,rowby=T)
clust2mean <- matrix(apply(clust2,2,mean),nrow=1)
dis2 <- sum(dist(clust2mean,clust2)^2)

clust3 <- subset(combcutdata, cluster == 3)
clust3 <- subset(clust3, select=c("q24r1","q24r2","q24r3","q25r1","q25r2","q25r3",
                                  "q26r3","q26r4","q26r5"))
clust3 <- as.matrix(clust3,rowby=T)
clust3mean <- matrix(apply(clust3,2,mean),nrow=1)
dis3 <- sum(dist(clust3mean,clust3)^2)

clust4 <- subset(combcutdata, cluster == 4)
clust4 <- subset(clust4, select=c("q24r1","q24r2","q24r3","q25r1","q25r2","q25r3",
                                  "q26r3","q26r4","q26r5"))
clust4 <- as.matrix(clust4,rowby=T)
clust4mean <- matrix(apply(clust4,2,mean),nrow=1)
dis4 <- sum(dist(clust4mean,clust4)^2)

clust5 <- subset(combcutdata, cluster == 5)
clust5 <- subset(clust5, select=c("q24r1","q24r2","q24r3","q25r1","q25r2","q25r3",
                                  "q26r3","q26r4","q26r5"))
clust5 <- as.matrix(clust5,rowby=T)
clust5mean <- matrix(apply(clust5,2,mean),nrow=1)

dis5 <- sum(dist(clust5mean,clust5)^2)

WSS <- sum(dis1,dis2,dis3,dis4,dis5)
WSS

BSS <- TSS - WSS
BSS
## calculating the % of Between SS/ Total SS
rsquare <- BSS/TSS
rsquare

dev.off()

my.k.choices <- 2:8
avg.sil.width <- rep(0, times=length(my.k.choices))
for (ii in (1:length(my.k.choices)) ){
  avg.sil.width[ii] <- pam(numsub, k=my.k.choices[ii])$silinfo$avg.width
}
print( cbind(my.k.choices, avg.sil.width) )

#PAM method
clusterresultsPAM <-pam(numsub,5)
summary(clusterresultsPAM)
plot(clusterresultsPAM, which.plots=1)
plot(clusterresultsPAM, which.plots=2)

#Model based clustering
library(mclust)
fit <- Mclust(numsub,5)
plot(fit,data=numsub, what="density") # plot results
summary(fit) # display the best model
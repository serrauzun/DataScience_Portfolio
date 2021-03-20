eu.df <- EuropeanEmployment
str(eu.df)
head(eu.df)

##################################################################
install.packages("cluster")
install.packages("useful")
install.packages("Hmisc")
install.packages("HSAUR2")
install.packages("HSAUR")
install.packages("MVA")
install.packages("fpc")
install.packages("mclust")
install.packages("lattice")
install.packages("car")
install.packages("ggplot2")
install.packages("maptree")

library(cluster)
library(useful)
library(Hmisc)
library(HSAUR)
library(MVA)
library(HSAUR2)
library(fpc)
library(mclust)
library(lattice)
library(car)
library(ggplot2)

# EDA to do scatterplots

ggplot(eu.df, aes(x=SER, y=FIN, colour = Group, label= Country)) + 
  geom_point() + geom_text(aes(label=Country),hjust=0, vjust=0) +
  ggtitle("Scatter Plot Services vs Financial") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(eu.df, aes(x=SER, y=MAN, colour = Group, label= Country)) + 
  geom_point() + geom_text(aes(label=Country),hjust=0, vjust=0) +
  ggtitle("Scatter Plot Services vs Manufacturing") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

pairs(eu.df[,-c(1,2)])

apply(eu.df[,-c(1,2)],MARGIN=1,FUN=sum)
pca.out <- princomp(x=eu.df[,-c(1,2)],cor=FALSE);
names(pca.out)

pc.1 <- pca.out$scores[,1];
pc.2 <- pca.out$scores[,2];
str(pc.1)
pcdf = data.frame(pc1=pc.1, pc2=pc.2)
pcdf1 = cbind(pcdf,eu.df$Country)
pcdf2 = cbind(pcdf1,eu.df$Group)
str(pcdf2)

pcdf

ggplot(pcdf2, aes(x=pc1, y=pc2, colour = eu.df$Group, label= eu.df$Country)) + 
  geom_point() + geom_text(aes(label=eu.df$Country),hjust=0, vjust=0) +
  ggtitle("Scatter Plot PC1 vs PC2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

pca.out_sc <- princomp(x=scale(eu.df[,-c(1,2)]),cor=FALSE);
names(pca.out_sc)

pc.1_sc <- pca.out_sc$scores[,1];
pc.2_sc <- pca.out_sc$scores[,2];
str(pc.1)
pcdf_sc = data.frame(pc1_sc=pc.1_sc, pc2_sc=pc.2_sc)
pcdf1_sc = cbind(pcdf_sc,eu.df$Country)
pcdf2_sc = cbind(pcdf1_sc,eu.df$Group)
str(pcdf2_sc)

ggplot(pcdf2_sc, aes(x=pc1_sc, y=pc2_sc, colour = eu.df$Group, label= eu.df$Country)) + 
  geom_point() + geom_text(aes(label=eu.df$Country),hjust=0, vjust=0) +
  ggtitle("Scatter Plot PC1 vs PC2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))



# Hirerarchical clustreing

hier.dist = dist(eu.df[,-c(1,2)])
library(maptree)
hclustmodel <- hclust(hier.dist, method = 'complete')
plot(hclustmodel,labels=eu.df$Country)
hclustmodel

# choose the number of clusters k = 3
cut.3 <- cutree(hclustmodel, k=3)
head(cut.3)
cut.3
pcdf3 <- cbind(pcdf2,cut.3)
pcdf3

# cross tab of clusters vs Group

table(pcdf3$'eu.df$Group',pcdf3$cut.3)

# accuracy - Between % ss
subdat <- eu.df[,-c(1,2)]
TSS3 <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS3
library(fpc)
complete3 <- cutree(hclust(hier.dist),3)
WSS3 <- cluster.stats(hier.dist,complete3, alt.clustering=NULL)$within.cluster.ss
WSS3
BetSSPer3 <- (TSS3-WSS3)/TSS3
BetSSPer3


# choose the number of clusters k = 6
cut.6 <- cutree(hclustmodel, k=6)
head(cut.6)
cut.6
pcdf6 <- cbind(pcdf2,cut.6)
pcdf6

# cross tab of clusters vs Group

table(pcdf6$'eu.df$Group',pcdf6$cut.6)

# accuracy - Between % ss
subdat <- eu.df[,-c(1,2)]
TSS6 <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS6
library(fpc)
complete6 <- cutree(hclust(hier.dist),6)
WSS6 <- cluster.stats(hier.dist,complete6, alt.clustering=NULL)$within.cluster.ss
WSS6
BetSSPer6 <- (TSS6-WSS6)/TSS6
BetSSPer6

###############
##############
#############
# Hirerarchical clustreing

hier.dist_pc = dist(pcdf)
library(maptree)
hclustmodel_pc <- hclust(hier.dist_pc, method = 'complete')
plot(hclustmodel_pc,labels=eu.df$Country)


# choose the number of clusters k = 3
cut.3_pc <- cutree(hclustmodel_pc, k=3)
cut.3_pc
pcdf3_pc <- cbind(pcdf2,cut.3_pc)
pcdf3_pc

# cross tab of clusters vs Group

table(pcdf3_pc$'eu.df$Group',pcdf3_pc$cut.3)

# accuracy - Between % ss
subdat <- pcdf ####
TSS3pc <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS3pc
library(fpc)
complete3_pc <- cutree(hclust(hier.dist_pc),3)
WSS3pc <- cluster.stats(hier.dist_pc,complete3_pc, alt.clustering=NULL)$within.cluster.ss
WSS3pc
BetSSPer3pc <- (TSS3pc-WSS3pc)/TSS3pc
BetSSPer3pc

# choose the number of clusters k = 6
hier.dist_pc = dist(pcdf)
library(maptree)
hclustmodel_pc <- hclust(hier.dist_pc, method = 'complete')
plot(hclustmodel_pc,labels=eu.df$Country)


# choose the number of clusters k = 3
cut.6_pc <- cutree(hclustmodel_pc, k=6)
cut.6_pc
pcdf6_pc <- cbind(pcdf2,cut.6_pc)
pcdf6_pc

# cross tab of clusters vs Group

table(pcdf6_pc$'eu.df$Group',pcdf6_pc$cut.6)

# accuracy - Between % ss
subdat <- pcdf ####
TSS6_pc <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS6_pc
library(fpc)
complete6_pc <- cutree(hclust(hier.dist_pc),6)
WSS6_pc <- cluster.stats(hier.dist_pc,complete6_pc, alt.clustering=NULL)$within.cluster.ss
WSS6_pc
BetSSPer6_pc <- (TSS6_pc-WSS6_pc)/TSS6_pc
BetSSPer6_pc


TSS3
WSS3
BetSSPer3
TSS6
WSS6
BetSSPer6
TSS3pc
WSS3pc
BetSSPer3pc
TSS6_pc
WSS6_pc
BetSSPer6_pc




#############
# kmeans clustering with k=3 clusters

clusterresults_3 <- kmeans(eu.df[,-c(1,2)],3)
names(clusterresults_3)
clusterresults_3$cluster
BetSSPer3k <- clusterresults_3$betweenss/clusterresults_3$totss
BetSSPer3k
clusterresults_3$totss
clusterresults_3

clusplot(eu.df[,-c(1,2)], clusterresults_3$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

install.packages("cluster")
library(cluster)




clusterresults_6 <- kmeans(eu.df[,-c(1,2)],6)
clusterresults_6 
names(clusterresults_6)
BetSSPer6k <- clusterresults_6$betweenss/clusterresults_6$totss
BetSSPer6k
clusterresults_6$totss


clusplot(eu.df[,-c(1,2)], clusterresults_6$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# cluster plots for kmeans

library(cluster) 
par(mar = c(5,5,3,1))
par(mfrow=c(1,1))
clusplot(eu.df[,-c(1,2)], clusterresults_3$cluster,main = "K-Means 3 Cluster Plot", color=TRUE, 
         labels=2, lines=0)

par(mar = c(5,5,3,1))
par(mfrow=c(1,1))
clusplot(eu.df[,-c(1,2)], clusterresults_6$cluster,main = "K-Means 6 Cluster Plot", color=TRUE, 
         labels=2, lines=0)



#############
# kmeans clustering with k=3 clusters

clusterresults_3_pc <- kmeans(pcdf,3)
names(clusterresults_3_pc)
BetSSPer_pc3 <- clusterresults_3_pc$betweenss/clusterresults_3_pc$totss
BetSSPer_pc3
clusterresults_3_pc$totss

par(mar = c(5,5,3,1))
par(mfrow=c(1,1))
plot(clusterresults_3_pc, data=pcdf)

clusplot(pcdf, clusterresults_3_pc$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

clusterresults_6_pc <- kmeans(pcdf,6)
names(clusterresults_6_pc)
BetSSPer_pc6 <- clusterresults_6_pc$betweenss/clusterresults_6_pc$totss
BetSSPer_pc6
clusterresults_6_pc$totss

par(mar = c(5,5,3,1))
par(mfrow=c(1,1))
plot(clusterresults_6_pc, data=pcdf)
clusplot(pcdf, clusterresults_6_pc$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

eu.df[c(21,22),]
# cluster plots for kmeans

library(cluster) 
par(mar = c(5,5,3,1))
par(mfrow=c(1,1))
clusplot(eu.df[,-c(1,2)], clusterresults_3_pc$cluster,main = "K-Means 3 Cluster Plot with PC", color=TRUE, 
         labels=2, lines=0)

par(mar = c(5,5,3,1))
par(mfrow=c(1,1))
clusplot(eu.df[,-c(1,2)], clusterresults_6_pc$cluster,main = "K-Means 6 Cluster Plot with PC", color=TRUE, 
         labels=2, lines=0)






#
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

#Hierarchical clustering
subdat <- eu.df[,-c(1,2)]
wssplot2 <- function(subdat, nc=20, seed=1234) {  
  wss <- (nrow(subdat)-1)*sum(apply(subdat,2,var))  
  for (i in 2:nc) {    
    require(fpc)    
    set.seed(seed)    
    hier.dist <- dist(subdat)    
    complete3 <- cutree(hclust(hier.dist),i)    
    wss[i] <- cluster.stats(hier.dist,complete3, alt.clustering=NULL)$within.cluster.ss}  
  rs <- (wss[1] - wss)/wss[1]  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")  
  plot(1:nc, rs, type="b", xlab="Number of Clusters",ylab="% of Between SS")  
  return(wss)}

wssplot2(subdat)

subdat <- states.df[,-c(1,2)]
wssplot3 <- function(subdat, nc=20, seed=1234) {  
  wss <- (nrow(subdat)-1)*sum(apply(subdat,2,var))  
  for (i in 2:nc) {    
    require(fpc)    
    set.seed(seed)    
    hier.dist <- dist(subdat)    
    complete3 <- cutree(hclust(hier.dist),i)    
    wss[i] <- cluster.stats(hier.dist,complete3, alt.clustering=NULL)$within.cluster.ss}  
  rs <- (wss[1] - wss)/wss[1]  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")  
  plot(1:nc, rs, type="b", xlab="Number of Clusters",ylab="% of Between SS")  
  return(wss)}

wssplot2(subdat)




###########
###########
states.df <- USStates
dim(states.df)
str(states.df)
hier.dist_us = dist(states.df[,-c(1,2)])
library(maptree)
hclustmodel_us <- hclust(hier.dist_us, method = 'complete')
plot(hclustmodel_us,labels=states.df$State)

subdat <- states.df[,-c(1,2)] ####
TSS_us <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS_us
library(fpc)
complete_us <- cutree(hclust(hier.dist_us),7)
WSS_us <- cluster.stats(hier.dist_us,complete_us, alt.clustering=NULL)$within.cluster.ss
WSS_us
BetSSPer_us <- (TSS_us-WSS_us)/TSS_us
BetSSPer_us

subdat_us <- states.df[,-c(1,2)]
wssplot3 <- function(subdat, nc=20, seed=1234) {  
  wss <- (nrow(subdat_us)-1)*sum(apply(subdat_us,2,var))  
  for (i in 2:nc) {    
    require(fpc)    
    set.seed(seed)    
    hier.dist_us <- dist(subdat_us)    
    complete_us <- cutree(hclust(hier.dist_us),i)    
    wss[i] <- cluster.stats(hier.dist_us,complete_us, alt.clustering=NULL)$within.cluster.ss}  
  rs <- (wss[1] - wss)/wss[1]  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")  
  plot(1:nc, rs, type="b", xlab="Number of Clusters",ylab="% of Between SS")  
  return(wss)}

wssplot3(subdat_us)


###########
###########
rec.df <- recidivism[,c(-18)]
colnames(rec.df)
clusterresults_rec <- kmeans(rec.df,4)
names(clusterresults_rec)
BetSSPer_rec <- clusterresults_rec$betweenss/clusterresults_rec$totss
BetSSPer_rec
clusterresults_rec$totss

clusterresults_rec

par(mar = c(5,5,3,1))
par(mfrow=c(1,1))
plot(clusterresults_rec, data=rec.df)

subdat <- rec.df
wssplot <- function(subdat, nc=15, seed=1234) {
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


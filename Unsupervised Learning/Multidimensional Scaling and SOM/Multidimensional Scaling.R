library(plyr)
library(psych)
install.packages("corrplot")
library(corrplot)
install.packages("vegan")
recidivism.df <- data.frame(recidivism)

colnames(recidivism.df)
head(recidivism.df)
str(recidivism.df)
summary(recidivism.df)

unique(recidivism.df$priors)
apply(recidivism.df, 2, function(x) length(unique(x)))

x1 <- recidivism.df$black
x2 <- recidivism.df$alcohol
x3 <- recidivism.df$drugs
x4 <- recidivism.df$super
x5 <- recidivism.df$married
x6 <- recidivism.df$felon
x7 <- recidivism.df$workprg
x8 <- recidivism.df$property
x9 <- recidivism.df$person
x10 <- recidivism.df$priors
x11 <- recidivism.df$educ
x12 <- recidivism.df$rules
x13 <- recidivism.df$age
x14 <- recidivism.df$tserved
x15 <- recidivism.df$follow
x16 <- recidivism.df$durat
x17 <- recidivism.df$cens
x18 <- recidivism.df$ldurat

multi.hist(recidivism.df)
rec_cor <- cor(recidivism.df)
corrplot(rec_cor, method = "color", outline = T, addrect = 4, rect.col = "black", rect.lwd = 3,cl.pos = "b", tl.col = "black", tl.cex = 1, cl.cex = 1, addCoef.col = "black", number.digits = 1, number.cex = 0.60, col = colorRampPalette(c("dark blue","white","dark green"))(100))

recidivism.df$ldurat <- NULL 

rec.df.mds1 <-cbind.data.frame(x1,x10,x14,x6,x8,x15,x16)


d1 <- dist(rec.df.mds1)
d1[1:200]
d1a <- as.matrix(d1)
d1a
fit1 <- cmdscale(d1a, eig=TRUE, k=2) # k is the number of dim
fit1 # view results


# plot solution
par(mar = c(5,5,3,1), bg="white")
x <- fit1$points[,1]
y <- fit1$points[,2]
plot(x, y, col = ifelse(recidivism.df$black <= 0,'blue','green'),  xlab="Dimension 1", ylab="Dimension 2",main="Metric MDS")
legend("topright", 
       legend = c("Black", "Non-Black"), 
       col = c("green", 
               "blue"), 
       pch = 1, 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


d3 <- as.matrix(dist(rec.df.mds1)) # euclidean distances between the rows
d3
fit3d <- cmdscale(d3, eig=TRUE, k=3) # k is the number of dim
fit3d # view results

#Plot in 3d
install.packages("rgl")
library(rgl)
plot3d(fit3d$points, col="dodgerblue2", size=4, pch=19, xlab="Dimension 1", ylab="Dimension 2", zlab="Dimension 3", main="3d Visualization of Florida Lakes MDS")
text3d(fit3d$points, texts=row.names(rec.df.mds), cex=0.6, col="dodgerblue2")

###
###
library(MASS)
d2 <- dist(rec.df.mds1)
fit2 <- isoMDS(d2, k=2) # k is the number of dim
fit2 # view results

# plot solution
par(mar = c(5,5,3,1))
x <- fit2$points[,1]
y <- fit2$points[,2]
plot(x, y, col = ifelse(recidivism.df$black <= 0,'blue','green'),  xlab="Dimension 1", ylab="Dimension 2",main="Non-Metric MDS")
legend("topright", 
       legend = c("Black", "Non-Black"), 
       col = c("green", 
               "blue"), 
       pch = 1, 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

library(focusedMDS)
focusedMDS(d2, ids=row.names(rec.df.mds1))

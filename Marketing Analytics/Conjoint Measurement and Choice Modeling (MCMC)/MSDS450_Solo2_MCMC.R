getwd()
setwd("/Users/serrauzun/Desktop/MSDS_450_Marketing/Solo2")

##################
#Chapter 1
require(dummies)
require(bayesm)
require("pROC")

load("stc-cbc-respondents-v3(1).RData")
ls()
str(resp.data.v3)
taskV3 <- read.csv("stc-dc-task-cbc -v3(1).csv", sep="\t")
str(taskV3)
require(dummies)
load("efCode.RData")
ls()
str(efcode.att.f)
str(efcode.attmat.f)
str(resp.data.v3)
str(taskV3)
apply(resp.data.v3[4:39], 2, function(x){tabulate(na.omit(x))}) 
task.mat <- as.matrix(taskV3[, c("screen", "RAM", "processor", "price", "brand")])
dim(task.mat)
head(task.mat)
X.mat=efcode.attmat.f(task.mat)  # Here is where we do effects coding
dim(X.mat)
head(X.mat)
pricevec=taskV3$price-mean(taskV3$price)
head(pricevec)
str(pricevec)
X.brands=X.mat[,9:11]
dim(X.brands)
str(X.brands)
X.BrandByPrice = X.brands*pricevec
dim(X.BrandByPrice)
str(X.BrandByPrice)
X.matrix=cbind(X.mat,X.BrandByPrice)
dim(X.matrix)
str(X.matrix)
X2.matrix=X.matrix[,1:2]
dim(X2.matrix)
det(t(X.matrix) %*% X.matrix)
ydata=resp.data.v3[,4:39]
names(ydata)
str(ydata)
ydata=na.omit(ydata)
str(ydata)
ydata=as.matrix(ydata)
dim(ydata)
zowner <- 1 * ( ! is.na(resp.data.v3$vList3) )
lgtdata = NULL
for (i in 1:424) { lgtdata[[i]]=list( y=ydata[i,],X=X.matrix )}
length(lgtdata)
str(lgtdata)


##################
#Chapter 2
mcmctest=list(R=50000, keep=5)
Data1=list(p=3,lgtdata=lgtdata)
testrun1=rhierMnlDP(Data=Data1,Mcmc=mcmctest)
names(testrun1)
betadraw1=testrun1$betadraw
dim(betadraw1)

summary(betadraw1[1,1,1:10000])
betameansoverall <- apply(betadraw1[,,1:10000],c(2),mean)
betameansoverall
perc <- apply(betadraw1[,,1:10000],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))
perc


##################
#Chapter 3
zownertest=matrix(scale(zowner,scale=FALSE),ncol=1)
Data2=list(p=3,lgtdata=lgtdata,Z=zownertest)
testrun2=rhierMnlDP(Data=Data2,Mcmc=mcmctest)
names(testrun2)
dim(testrun2$Deltadraw)
betameansoverall2 <-apply(testrun2$Deltadraw[1:10000,],2,mean) 
perc2 <- apply(testrun2$Deltadraw[1:10000,],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))
deltadraw1 <- apply(testrun2$Deltadraw[1:10000,],c(2),mean)
deltadraw1
betameansoverall2
perc2
betadraw2=testrun2$betadraw
dim(betadraw2)

##################
#Chapter 4
betameans1 <- apply(betadraw1[,,1:10000],c(1,2),mean)
xbeta1=X.matrix%*%t(betameans1)
xbeta2=matrix(xbeta1,ncol=3,byrow=TRUE)
expxbeta2=exp(xbeta2)
rsumvec1=rowSums(expxbeta2)
pchoicemat1=expxbeta2/rsumvec1
custchoice1 <- max.col(pchoicemat1)

ydatavec <- as.vector(t(ydata))
table(custchoice1,ydatavec)
roctest1 <- roc(ydatavec, custchoice1, plot=TRUE)
auc(roctest1)
logliketest1 <- testrun1$loglike
mean(logliketest1)

m1 <- matrix(custchoice1, nrow =36,  byrow=F)
m2 <- t(m1)
apply(m2, 2, function(x){tabulate(na.omit(x))})

#Repeating for betadraw2##
betameans2 <- apply(betadraw2[,,1:10000],c(1,2),mean)
xbeta3=X.matrix%*%t(betameans2)
xbeta4=matrix(xbeta3,ncol=3,byrow=TRUE)
expxbeta3=exp(xbeta4)
rsumvec2=rowSums(expxbeta3)
pchoicemat2=expxbeta3/rsumvec2
custchoice2 <- max.col(pchoicemat2)

ydatavec <- as.vector(t(ydata))
table(custchoice2,ydatavec)
roctest2 <- roc(ydatavec, custchoice2, plot=TRUE)
auc(roctest2)
logliketest2 <- testrun2$loglike
mean(logliketest2)

m3 <- matrix(custchoice2, nrow =36,  byrow=F)
m4 <- t(m3)
apply(m4, 2, function(x){tabulate(na.omit(x))})

zowner
table(zowner)

##################
#Chapter 5 - evaluation of extra scenarios

ex_scen <- read.csv("extra-scenarios.csv")
Xextra.matrix <- as.matrix(ex_scen[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9",
                                      "V10","V11","V12","V13","V14")])

betavec=matrix(betameansoverall,ncol=1,byrow=TRUE)
xbeta=X.matrix%*%(betavec)
dim(xbeta)
xbeta2=matrix(xbeta,ncol=3,byrow=TRUE)
dim(xbeta2)
expxbeta2=exp(xbeta2)
rsumvec=rowSums(expxbeta2)
pchoicemat=expxbeta2/rsumvec
pchoicemat

pchoicemat2 <- round(pchoicemat*424,digits=0)
pchoicemat

####################################################
betavec1=matrix(betameansoverall,ncol=1,byrow=TRUE)
xextrabeta1=Xextra.matrix%*%(betavec1)
xbetaextra2=matrix(xextrabeta1,ncol=3,byrow=TRUE)

expxbetaextra1=exp(xbetaextra2)
rsumvec_a=rowSums(expxbetaextra1)
pchoicemat_a=expxbetaextra2/rsumvec_a
pchoicemat_a

pchoicemat_b <- round(pchoicemat_a*424,digits=0)
pchoicemat_b

###
resp_accuracy <- NULL
for (i in 1:424) {
  start <- i*36-35
  end <- i*36
  d <- table(factor(custchoice[start:end],levels = 1:3),
             factor(ydatavec[start:end], levels = 1:3))
  resp_accuracy[i] <- sum(diag(d))/sum(d)
} 
plot(resp_accuracy, main = "Model Accuracy by Respondent")
respdf <- data.frame(resp_accuracy)
head(respdf)
str(respdf)
head(ydata)
rn <- rownames(ydata)
rndf <- as.data.frame(rn)
resp_all <- cbind(rndf,respdf)
head(resp_all)

ydata

str(resp_all)
hist(resp_all$resp_accuracy)
outlier <- subset(resp_all, resp_accuracy < 0.6)
test <- outlier[order(outlier$resp_accuracy),]
test
dim(test)

dim(resp.data.v3)

a_1 <- resp.data.v3[229,]
a_2 <- resp.data.v3[147,]
a_3 <- resp.data.v3[406,]
a_4 <- resp.data.v3[171,]
a_5 <- resp.data.v3[309,]
a_6 <- resp.data.v3[89,]
a_7 <- resp.data.v3[101,]
a_8 <- resp.data.v3[116,]
a_9 <- resp.data.v3[137,]
a_10 <- resp.data.v3[242,]

a_11 <- resp.data.v3[423,]
a_12 <- resp.data.v3[268,]
a_13 <- resp.data.v3[96,]
a_14 <- resp.data.v3[112,]
a_15 <- resp.data.v3[172,]
a_16 <- resp.data.v3[257,]

all1 <- rbind(a_1, a_2, a_3, a_4, a_5, a_6, a_7, a_8, a_9, a_10, a_11, a_12, a_13, a_14, a_15, a_16)

table(all1$D1)
table(all1$D2)
table(all1$D3)
table(all1$D5)

##############################
resp_accuracy2 <- NULL
for (i in 1:68) {
  start <- i*36-35
  end <- i*36
  d2 <- table(factor(custchoice[start:end],levels = 1:3),
             factor(zowner[start:end], levels = 1:3))
  resp_accuracy2[i] <- sum(diag(d2))/sum(d2)
} 
resp_accuracy2
plot(resp_accuracy2, main = "Model Accuracy by Respondent")
respdf2 <- data.frame(resp_accuracy2)
head(respdf2)
str(respdf2)
head(zowner)
rn2 <- rownames(zowner)
rndf2 <- as.data.frame(rn2)
resp_all2 <- cbind(rndf2,respdf2)
head(resp_all2)

str(resp_all2)
hist(resp_all$resp_accuracy2)
outlier <- subset(resp_all, resp_accuracy < 0.6)
outlier[order(outlier$resp_accuracy),]

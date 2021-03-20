install.packages("psych") 
install.packages("corrplot")
install.packages("GPArotation")
library(GPArotation)
library(psych) 
library(corrplot)
library(RColorBrewer)
library(readxl)
library(corrplot)

bfi_data <- bfi
bfi_data

str(bfi_data)
dim(bfi_data)
head(bfi_data)
summary(bfi_data)

#Remove rows with missing values and keep only complete cases 
sapply(bfi_data, function(x) sum(is.na(x)))
bfi_data  <- bfi_data[complete.cases(bfi_data),]

dim(bfi_data)
2800 - 2236
#564 observations were removed due to having missing values
# we have observation count over variable count * 20 so it is enough data to work with 

bfi_cor <- cor(bfi_data)
bfi_cor
corrplot(bfi_cor, method = "color", outline = T, addrect = 4, rect.col = "black", rect.lwd = 3,cl.pos = "b", tl.col = "black", tl.cex = 1, cl.cex = 1, addCoef.col = "black", number.digits = 1, number.cex = 0.60, col = colorRampPalette(c("darkred","white","darkgreen"))(100))


is.matrix(bfi_cor)
isSymmetric(bfi_cor)

###
bfi_small <- bfi_data[,c(1:25)]
bfi_small_cor <- cor(bfi_small)
bfi_small_cor
colnames(bfi_small_cor)


Z <- eigen(bfi_small_cor)
Z$values

#scree plot
fa.parallel(bfi_small,n.obs = 2236, fa = "both", n.iter = 100, show.legend = TRUE, main = 'Scree Plots with Parallel Analysis')

#Question 2
fa_varimax <- fa(bfi_small, nfactors = 6, rotate = "varimax", fm = "ml")
fa_varimax

#Question 3
fa_promax <- fa(bfi_small, nfactors = 6, rotate = "promax", fm = "ml")
fa_promax

#Question 4
fa_varimax_1 <- fa(bfi_small, nfactors = 1, rotate = "varimax", fm = "ml")
fa_varimax_1

fa_varimax_2 <- fa(bfi_small, nfactors = 2, rotate = "varimax", fm = "ml")
fa_varimax_2

fa_varimax_3 <- fa(bfi_small, nfactors = 3, rotate = "varimax", fm = "ml")
fa_varimax_3

fa_varimax_4 <- fa(bfi_small, nfactors = 4, rotate = "varimax", fm = "ml")
fa_varimax_4

fa_varimax_5 <- fa(bfi_small, nfactors = 5, rotate = "varimax", fm = "ml")
fa_varimax_5
 
#Question 6
fa_varimax_all <- fa(bfi_data, nfactors = 5, rotate = "varimax", fm = "ml")
fs <- factor.scores(bfi_data, fa_varimax_all)
fs <- fs$scores
bfi_data_comb <- cbind(bfi_data,fs) 

bfi_all_cor <- cor(bfi_data_comb)
corrplot(bfi_all_cor, method = "color", outline = T, addrect = 4, rect.col = "black", rect.lwd = 3,cl.pos = "b", tl.col = "black", tl.cex = 1, cl.cex = 1, addCoef.col = "black", number.digits = 1, number.cex = 0.60, col = colorRampPalette(c("darkred","white","darkgreen"))(100))

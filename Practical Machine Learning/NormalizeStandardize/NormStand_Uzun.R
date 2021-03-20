x <- df_clean
colnames(x)

#seperate binary and categorical variablaes to two dfs
x_binary_categorical <- x[,c(1,6:7,15:24)]
x_numeric <- x[,c(2:5,8:14)]

head(x_numeric, n=5)
head(x_binary, n=5)
summary(x_numeric)

######################### NORMALIZING #########################
#### add nor_ prefix 
x_num_nor <- x_numeric
head(x_num_nor)

colnames(x_num_nor) <- paste("nor", colnames(x_num_nor), sep = "_")
colnames(x_num_nor)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

colnames(x_num_nor)
x_num_nor$nor_TARGET_LOSS_AMT <- normalize(x_num_nor$nor_TARGET_LOSS_AMT)
x_num_nor$nor_LOAN <- normalize(x_num_nor$nor_LOAN)
x_num_nor$nor_MORTDUE <- normalize(x_num_nor$nor_MORTDUE)
x_num_nor$nor_VALUE <- normalize(x_num_nor$nor_VALUE)
x_num_nor$nor_YOJ <- normalize(x_num_nor$nor_YOJ)
x_num_nor$nor_DEROG <- normalize(x_num_nor$nor_DEROG)
x_num_nor$nor_DELINQ <- normalize(x_num_nor$nor_DELINQ)
x_num_nor$nor_CLAGE <- normalize(x_num_nor$nor_CLAGE)
x_num_nor$nor_NINQ <- normalize(x_num_nor$nor_NINQ)
x_num_nor$nor_CLNO <- normalize(x_num_nor$nor_CLNO)
x_num_nor$nor_DEBTINC <- normalize(x_num_nor$nor_DEBTINC)

head(x_num_nor)
normalized_all <- cbind(x_num_nor,x_binary_categorical,x_numeric)
colnames(normalized_all)

######################### STANDARDIZING #########################

#### create df for standardizing
x_num_std <- x_numeric
head(x_num_std)

colnames(x_num_std) <- paste("std", colnames(x_num_std), sep = "_")
colnames(x_num_std)

x_num_std$std_TARGET_LOSS_AMT <- scale(x_num_std$std_TARGET_LOSS_AMT)
x_num_std$std_LOAN <- scale(x_num_std$std_LOAN)
x_num_std$std_MORTDUE <- scale(x_num_std$std_MORTDUE)
x_num_std$std_VALUE <- scale(x_num_std$std_VALUE)
x_num_std$std_YOJ <- scale(x_num_std$std_YOJ)
x_num_std$std_DEROG <- scale(x_num_std$std_DEROG)
x_num_std$std_DELINQ <- scale(x_num_std$std_DELINQ)
x_num_std$std_CLAGE <- scale(x_num_std$std_CLAGE)
x_num_std$std_NINQ <- scale(x_num_std$std_NINQ)
x_num_std$std_CLNO <- scale(x_num_std$std_CLNO)
x_num_std$std_DEBTINC <- scale(x_num_std$std_DEBTINC)

head(x_num_std)
standardized_all <- cbind(x_num_std,x_binary_categorical,x_numeric)
colnames(standardized_all)




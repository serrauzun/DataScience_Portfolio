df <- as.data.frame(HMEQ_Loss)
colnames(df)
summary(df)

# number of NAs in each column
colSums(is.na(df))

install.packages("dplyr")
library(dplyr)

#structure of the variables
str(df)

# unique values in columns that are character
unique(df$REASON)
unique(df$JOB)

#only numeric columns
only_num <- df[,c("TARGET_BAD_FLAG", "TARGET_LOSS_AMT","LOAN","MORTDUE","VALUE","YOJ","DEROG","DELINQ","CLAGE","NINQ","CLNO","DEBTINC")]
boxplot(only_num)

install.packages('plyr')
library('plyr')

df$JOB_num <- revalue(df$JOB, c("Office"="1", "Mgr"="2", "ProfExe"="3", "Self"="4", "Sales"="5","Other"="6"))
df$REASON_num <- revalue(df$REASON, c("HomeImp"="10", "DebtCon"="20"))

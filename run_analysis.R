setwd(dir="/users/will/Desktop")
library(dplyr)
sjtest <- read.table("subject_test.txt")
xtest <- read.table("X_test.txt")
ytest <- read.table("y_test.txt")
sjtrain <- read.table("subject_train.txt")
xtrain <- read.table("X_train.txt")
ytrain <- read.table("y_train.txt")
train <- cbind(sjtrain, ytrain, xtrain)
test <- cbind(sjtest, ytest, xtest)
var <- read.table("features.txt")[,2]
df_merge <- rbind(test, train)
colnames(df_merge)[1:2] <- c("subject", "activity")
colnames(df_merge)[3:563] <- var
varind <- c(1,1,grepl("mean", var) + grepl("std", var))
df_merge <- df_merge[,grep(1,varind)]
df_merge[,2] <- gsub(1, "Walking", df_merge[,2])
df_merge[,2] <- gsub(2, "Walking Upstairs",df_merge[,2]) 
df_merge[,2] <- gsub(3, "Walking Downstairs",df_merge[,2])
df_merge[,2] <- gsub(4, "Sitting",df_merge[,2])
df_merge[,2] <- gsub(5, "Standing", df_merge[,2])
df_merge[,2] <- gsub(6, "Laying", df_merge[,2])

#calculate each variable's mean grouped by subject and activity

#group by subject
avgbysj <- as.data.frame(matrix(0, 30, 79))
for (i in 1:79){
  t<-tapply(df_merge[,i+2], df_merge$subject, FUN = mean)
  as.data.frame(t)
  avgbysj[,i] <- t
}
colnames(avgbysj)<-colnames(df_merge)[3:81]
write.table("avg_by_subject.txt")

#group by activity
avgbyaty <- as.data.frame(matrix(0,6,79))
for (i in 1:79){
  t<-tapply(df_merge[,i+2], df_merge$activity, FUN = mean)
  avgbyaty[,i] <- t
}
colnames(avgbyaty)<-colnames(df_merge)[3:81]
write.table("avg_by_activity.txt")
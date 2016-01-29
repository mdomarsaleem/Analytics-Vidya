rm(list=ls())


##Loading library
library(readr)
library(caTools)
library(rpart)
library(caret)
library(dummies)
library(randomForest)

train <- read.csv("Train_Fyxd0t8.csv")
test <- read.csv('Test_C1XBIYq.csv')

train <- train[train$Estimated_Insects_Count<3516,]
train <- train[train$Number_Doses_Week<75,]
train <- train[train$Number_Weeks_Quit<40,]

combi <- rbind(train[,-10],test)
combi <- combi[combi$Estimated_Insects_Count<3516,]
combi <- combi[combi$Number_Doses_Week<75,]
combi <- combi[combi$Number_Weeks_Quit<40,]

apply(train,2,function(x) sum(is.na(x)))

Weeks_Used <- (lm(Number_Weeks_Used~I(Number_Weeks_Quit^2)+I(Number_Doses_Week^2)+I(Estimated_Insects_Count^2)+Number_Weeks_Quit+Number_Doses_Week+Estimated_Insects_Count+Pesticide_Use_Category+Soil_Type+Crop_Type+I(Number_Doses_Week*Number_Weeks_Quit)+I(Number_Weeks_Quit*Estimated_Insects_Count)+I(Estimated_Insects_Count*Number_Doses_Week)+I(Soil_Type*Crop_Type),data=combi[is.na(combi$Number_Weeks_Used)==F,]))
rm(combi)
train[is.na(train$Number_Weeks_Used),'Number_Weeks_Used'] <- as.numeric(predict(Weeks_Used,train[is.na(train$Number_Weeks_Used),]))
test[is.na(test$Number_Weeks_Used),'Number_Weeks_Used'] <- as.numeric(predict(Weeks_Used,test[is.na(test$Number_Weeks_Used),]))
train <- train[train$Number_Weeks_Used<66,]



train <- cbind(train,as.data.frame(dummy(train$Season)))
test <- cbind(test,as.data.frame(dummy(test$Season)))
detach("package:dummies", unload=TRUE)
colnames(train)[11:13] <- c('Season_1','Season_2','Season_3')
colnames(test)[10:12] <- c('Season_1','Season_2','Season_3')
train$Season <- NULL
test$Season <- NULL

train$pest_week <- train$Estimated_Insects_Count*(train$Number_Doses_Week)
test$pest_week <- test$Estimated_Insects_Count*(test$Number_Doses_Week)
train$pest_Used <- train$Estimated_Insects_Count*(train$Number_Weeks_Used)
test$pest_Used <- test$Estimated_Insects_Count*(test$Number_Weeks_Used)
train$pest_Quit <- train$Estimated_Insects_Count*(train$Number_Weeks_Quit)
test$pest_Quit <- test$Estimated_Insects_Count*(test$Number_Weeks_Quit)

train$ID <- NULL
target <- (train$Crop_Damage)
train$Crop_Type <- as.factor(train$Crop_Type)
train$Soil_Type <- as.factor(train$Soil_Type)
train$Pesticide_Use_Category <- as.factor(train$Pesticide_Use_Category)
train$Crop_Damage <- as.factor(ifelse(train$Crop_Damage!=0,1,0))

set.seed(19)
split <- sample.split(train$Crop_Damage,SplitRatio = 0.75)
detach("package:caTools", unload=TRUE)
tra <- train[split,]
val <- train[!split,]


rpart <- rpart(Crop_Damage~.,data=tra,method="class",minsplit = 5)
predict <- predict(rpart,val,type="class")
confusionMatrix(predict,val$Crop_Damage)


rf <- randomForest(Crop_Damage~.,data=tra[,c(1,2,4,5,8,12,13)],method="class",importance=T,do.trace=F,ntree=200,mtry=2)
predict <- predict(rf,val)
confusionMatrix(predict,val$Crop_Damage)
round(importance(rf), 2)



















)
